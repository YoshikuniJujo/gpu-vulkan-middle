{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGe ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image.Middle.Internal (

	-- * CREATE AND DESTROY

	create, recreate, recreate', destroy, I(..), CreateInfo(..),

	-- ** Image Group

	Group, group, create', destroy', lookup,

	-- * GET MEMORY REQUIREMENTS AND BIND MEMORY

	getMemoryRequirements, bindMemory,

	-- * MEMORY BARRIER

	MemoryBarrier(..), memoryBarrierToCore,
	MemoryBarrier2(..), memoryBarrier2ToCore,
	SubresourceRange(..), subresourceRangeToCore,

	-- * BLIT

	Blit(..), blitToCore, Blit2(..), blit2ToCore,
	SubresourceLayers(..), subresourceLayersToCore,

	-- * OTHERS

	Subresource(..), subresourceToCore

	) where

import Prelude hiding (lookup)
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke (withPoked, WithPoked, withPoked', withPtrS)
import Control.Arrow
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Map qualified as M
import Data.IORef
import Data.Word
import Text.Show.ToolsYj

import Gpu.Vulkan.Core
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Image.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Types as Device
import qualified Gpu.Vulkan.Memory.Middle.Internal as Memory
import qualified Gpu.Vulkan.Image.Core as C
import qualified Gpu.Vulkan.Sample.Enum as Sample
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily

import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline

data SubresourceRange = SubresourceRange {
	subresourceRangeAspectMask :: AspectFlags,
	subresourceRangeBaseMipLevel :: Word32,
	subresourceRangeLevelCount :: Word32,
	subresourceRangeBaseArrayLayer :: Word32,
	subresourceRangeLayerCount :: Word32 }
	deriving Show

subresourceRangeToCore :: SubresourceRange -> C.SubresourceRange
subresourceRangeToCore SubresourceRange {
	subresourceRangeAspectMask = AspectFlagBits am,
	subresourceRangeBaseMipLevel = bmlv,
	subresourceRangeLevelCount = lvc,
	subresourceRangeBaseArrayLayer = baly,
	subresourceRangeLayerCount = lyc } = C.SubresourceRange {
		C.subresourceRangeAspectMask = am,
		C.subresourceRangeBaseMipLevel = bmlv,
		C.subresourceRangeLevelCount = lvc,
		C.subresourceRangeBaseArrayLayer = baly,
		C.subresourceRangeLayerCount = lyc }

newtype I = I (IORef (Extent3d, C.I))

instance ShowIO I where
	showIO (I ri) = do
		i <- readIORef ri
		pure $ "I <IORef: " ++ show i ++ ">"

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoImageType :: Type,
	createInfoFormat :: Format,
	createInfoExtent :: Extent3d,
	createInfoMipLevels :: Word32,
	createInfoArrayLayers :: Word32,
	createInfoSamples :: Sample.CountFlagBits,
	createInfoTiling :: Tiling,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index],
	createInfoInitialLayout :: Layout }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoImageType = Type tp,
	createInfoFormat = Format fmt,
	createInfoExtent = ext,
	createInfoMipLevels = mls,
	createInfoArrayLayers = als,
	createInfoSamples = Sample.CountFlagBits smpls,
	createInfoTiling = Tiling tlng,
	createInfoUsage = UsageFlagBits usg,
	createInfoSharingMode = SharingMode sm,
	createInfoQueueFamilyIndices =
		length &&& (QueueFamily.unIndex <$>) -> (qfic, qfis),
	createInfoInitialLayout = Layout lyt } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray qfic \pqfis -> do
		pokeArray pqfis qfis
		let	ci = C.CreateInfo {
				C.createInfoSType = (),
				C.createInfoPNext = pnxt',
				C.createInfoFlags = flgs,
				C.createInfoImageType = tp,
				C.createInfoFormat = fmt,
				C.createInfoExtent = ext,
				C.createInfoMipLevels = mls,
				C.createInfoArrayLayers = als,
				C.createInfoSamples = smpls,
				C.createInfoTiling = tlng,
				C.createInfoUsage = usg,
				C.createInfoSharingMode = sm,
				C.createInfoQueueFamilyIndexCount = fromIntegral qfic,
				C.createInfoPQueueFamilyIndices = pqfis,
				C.createInfoInitialLayout = lyt }
		withPoked ci f

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO I
create (Device.D dvc) ci mac = I <$> alloca \pimg -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.mToCore mac \pac ->
			throwUnlessSuccess . Result
				=<< C.create dvc pci pac pimg
	newIORef . (ex ,) =<< peek pimg
	where ex = createInfoExtent ci

group :: Device.D -> TPMaybe.M AllocationCallbacks.A md ->
	(forall s . Group s k -> IO a) -> IO a
group dvc mac f = do
	(sem, m) <- atomically $ (,) <$> newTSem 1 <*> newTVar M.empty
	rtn <- f $ Group sem m
	((\i -> destroy dvc i mac) `mapM_`) =<< atomically (readTVar m)
	pure rtn

create' :: (Ord k, WithPoked (TMaybe.M mn)) => Device.D ->
	Group sm k -> k -> CreateInfo mn ->
	TPMaybe.M AllocationCallbacks.A mc -> IO (Either String I)
create' (Device.D dvc) (Group sem is) k ci mac = do
	ok <- atomically do
		mx <- (M.lookup k) <$> readTVar is
		case mx of
			Nothing -> waitTSem sem >> pure True
			Just _ -> pure False
	if ok
	then do	i <- I <$> alloca \pimg -> do
			createInfoToCore ci \pci ->
				AllocationCallbacks.mToCore mac \pac ->
					throwUnlessSuccess . Result
						=<< C.create dvc pci pac pimg
			newIORef . (ex ,) =<< peek pimg
		atomically $ modifyTVar is (M.insert k i) >> signalTSem sem
		pure $ Right i
	else pure . Left $ "Gpu.Vulkan.Image.create': The key already exist"
	where ex = createInfoExtent ci

data Group s k = Group TSem (TVar (M.Map k I))

destroy' :: Ord k => Device.D ->
	Group sm k -> k -> TPMaybe.M AllocationCallbacks.A mc ->
	IO (Either String ())
destroy' dvc (Group sem is) k mac = do
	mi <- atomically do
		mx <- (M.lookup k) <$> readTVar is
		case mx of
			Nothing -> pure Nothing
			Just _ -> waitTSem sem >> pure mx
	case mi of
		Nothing -> pure $ Left "Gpu.Vulkan.Image.destroy: No such key"
		Just i -> do
			destroy dvc i mac
			atomically do
				modifyTVar is (M.delete k)
				signalTSem sem
				pure $ Right ()

lookup :: Ord k => Group sm k -> k -> IO (Maybe I)
lookup (Group _sem is) k = atomically $ M.lookup k <$> readTVar is

recreate :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn ->
	TPMaybe.M AllocationCallbacks.A mc ->
	TPMaybe.M AllocationCallbacks.A md ->
	I -> IO ()
recreate d@(Device.D dvc) ci macc macd i@(I ri) = alloca \pimg ->
	createInfoToCore ci \pci ->
	AllocationCallbacks.mToCore macc \pacc -> do
		r <- C.create dvc pci pacc pimg
		throwUnlessSuccess $ Result r
		destroy d i macd
		writeIORef ri . (ex ,) =<< peek pimg
	where ex = createInfoExtent ci

recreate' :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn ->
	TPMaybe.M AllocationCallbacks.A mc ->
	TPMaybe.M AllocationCallbacks.A md ->
	I -> IO a -> IO ()
recreate' (Device.D dvc) ci macc macd (I ri) act = alloca \pimg ->
	createInfoToCore ci \pci ->
	AllocationCallbacks.mToCore macc \pacc -> do
		r <- C.create dvc pci pacc pimg
		throwUnlessSuccess $ Result r
		(_, img) <- readIORef ri
		writeIORef ri . (ex ,) =<< peek pimg
		_ <- act
		AllocationCallbacks.mToCore macd $ C.destroy dvc img
	where ex = createInfoExtent ci

getMemoryRequirements :: Device.D -> I -> IO Memory.Requirements
getMemoryRequirements (Device.D dvc) (I ri) =
	(Memory.requirementsFromCore <$>) . ($ pure) $ runContT do
		pr <- ContT alloca
		lift do	(_, i) <- readIORef ri
			C.getMemoryRequirements dvc i pr
			peek pr

bindMemory :: Device.D -> I -> Memory.M -> Device.Size -> IO ()
bindMemory (Device.D dvc) (I rimg) mem (Device.Size ost) = do
	(_, img) <- readIORef rimg
	cmem <- Memory.mToCore mem
	r <- C.bindMemory dvc img cmem ost
	throwUnlessSuccess $ Result r

data MemoryBarrier mn = MemoryBarrier {
	memoryBarrierNext :: TMaybe.M mn,
	memoryBarrierSrcAccessMask :: AccessFlags,
	memoryBarrierDstAccessMask :: AccessFlags,
	memoryBarrierOldLayout :: Layout,
	memoryBarrierNewLayout :: Layout,
	memoryBarrierSrcQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierDstQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrierImage :: I,
	memoryBarrierSubresourceRange :: SubresourceRange }

memoryBarrierToCore :: WithPoked (TMaybe.M mn) =>
	MemoryBarrier mn -> (C.MemoryBarrier -> IO a) -> IO ()
memoryBarrierToCore MemoryBarrier {
	memoryBarrierNext = mnxt,
	memoryBarrierSrcAccessMask = AccessFlagBits sam,
	memoryBarrierDstAccessMask = AccessFlagBits dam,
	memoryBarrierOldLayout = Layout ol,
	memoryBarrierNewLayout = Layout nl,
	memoryBarrierSrcQueueFamilyIndex = QueueFamily.Index sqfi,
	memoryBarrierDstQueueFamilyIndex = QueueFamily.Index dqfi,
	memoryBarrierImage = I rimg,
	memoryBarrierSubresourceRange = srr } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	readIORef rimg >>= \(_, img) ->
	f C.MemoryBarrier {
		C.memoryBarrierSType = (),
		C.memoryBarrierPNext = pnxt',
		C.memoryBarrierSrcAccessMask = sam,
		C.memoryBarrierDstAccessMask = dam,
		C.memoryBarrierOldLayout = ol,
		C.memoryBarrierNewLayout = nl,
		C.memoryBarrierSrcQueueFamilyIndex = sqfi,
		C.memoryBarrierDstQueueFamilyIndex = dqfi,
		C.memoryBarrierImage = img,
		C.memoryBarrierSubresourceRange = subresourceRangeToCore srr }

data MemoryBarrier2 mn = MemoryBarrier2 {
	memoryBarrier2Next :: TMaybe.M mn,
	memoryBarrier2SrcStageMask :: Pipeline.StageFlags2,
	memoryBarrier2SrcAccessMask :: AccessFlags2,
	memoryBarrier2DstStageMask :: Pipeline.StageFlags2,
	memoryBarrier2DstAccessMask :: AccessFlags2,
	memoryBarrier2OldLayout :: Layout, memoryBarrier2NewLayout :: Layout,
	memoryBarrier2SrcQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrier2DstQueueFamilyIndex :: QueueFamily.Index,
	memoryBarrier2Image :: I,
	memoryBarrier2SubresourceRange :: SubresourceRange }

memoryBarrier2ToCore :: WithPoked (TMaybe.M mn) =>
	MemoryBarrier2 mn -> (C.MemoryBarrier2 -> IO a) -> IO ()
memoryBarrier2ToCore MemoryBarrier2 {
	memoryBarrier2Next = mnxt,
	memoryBarrier2SrcStageMask = Pipeline.StageFlagBits2 ssm,
	memoryBarrier2SrcAccessMask = AccessFlagBits2 sam,
	memoryBarrier2DstStageMask = Pipeline.StageFlagBits2 dsm,
	memoryBarrier2DstAccessMask = AccessFlagBits2 dam,
	memoryBarrier2OldLayout = Layout ol,
	memoryBarrier2NewLayout = Layout nl,
	memoryBarrier2SrcQueueFamilyIndex = QueueFamily.Index sqfi,
	memoryBarrier2DstQueueFamilyIndex = QueueFamily.Index dqfi,
	memoryBarrier2Image = I rimg,
	memoryBarrier2SubresourceRange = srr } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	readIORef rimg >>= \(_, img) ->
	f C.MemoryBarrier2 {
		C.memoryBarrier2SType = (),
		C.memoryBarrier2PNext = pnxt',
		C.memoryBarrier2SrcStageMask = ssm,
		C.memoryBarrier2SrcAccessMask = sam,
		C.memoryBarrier2DstStageMask = dsm,
		C.memoryBarrier2DstAccessMask = dam,
		C.memoryBarrier2OldLayout = ol,
		C.memoryBarrier2NewLayout = nl,
		C.memoryBarrier2SrcQueueFamilyIndex = sqfi,
		C.memoryBarrier2DstQueueFamilyIndex = dqfi,
		C.memoryBarrier2Image = img,
		C.memoryBarrier2SubresourceRange = subresourceRangeToCore srr }

data SubresourceLayers = SubresourceLayers {
	subresourceLayersAspectMask :: AspectFlags,
	subresourceLayersMipLevel :: Word32,
	subresourceLayersBaseArrayLayer :: Word32,
	subresourceLayersLayerCount :: Word32 }
	deriving Show

subresourceLayersToCore :: SubresourceLayers -> C.SubresourceLayers
subresourceLayersToCore SubresourceLayers {
	subresourceLayersAspectMask = AspectFlagBits am,
	subresourceLayersMipLevel = ml,
	subresourceLayersBaseArrayLayer = bal,
	subresourceLayersLayerCount = lc } = C.SubresourceLayers {
		C.subresourceLayersAspectMask = am,
		C.subresourceLayersMipLevel = ml,
		C.subresourceLayersBaseArrayLayer = bal,
		C.subresourceLayersLayerCount = lc }

destroy :: Device.D -> I -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) (I rimg) mac =
	AllocationCallbacks.mToCore mac \pac -> do
		(_, img) <- readIORef rimg
		C.destroy dvc img pac

data Blit = Blit {
	blitSrcSubresource :: SubresourceLayers,
	blitSrcOffsetFrom :: Offset3d,
	blitSrcOffsetTo :: Offset3d,
	blitDstSubresource :: SubresourceLayers,
	blitDstOffsetFrom :: Offset3d,
	blitDstOffsetTo :: Offset3d }
	deriving Show

blitToCore :: Blit -> C.Blit
blitToCore Blit {
	blitSrcSubresource = ssr,
	blitSrcOffsetFrom = sof,
	blitSrcOffsetTo = sot,
	blitDstSubresource = dsr,
	blitDstOffsetFrom = dof,
	blitDstOffsetTo = dot } = C.Blit {
	C.blitSrcSubresource = subresourceLayersToCore ssr,
	C.blitSrcOffsets = [sof, sot],
	C.blitDstSubresource = subresourceLayersToCore dsr,
	C.blitDstOffsets = [dof, dot] }

data Blit2 mn = Blit2 {
	blit2Next :: TMaybe.M mn,
	blit2SrcSubresource :: SubresourceLayers,
	blit2SrcOffsetFrom :: Offset3d,
	blit2SrcOffsetTo :: Offset3d,
	blit2DstSubresource :: SubresourceLayers,
	blit2DstOffsetFrom :: Offset3d,
	blit2DstOffsetTo :: Offset3d }

deriving instance Show (TMaybe.M mn) => Show (Blit2 mn)

blit2ToCore :: WithPoked (TMaybe.M mn) => Blit2 mn -> (C.Blit2 -> IO a) -> IO ()
blit2ToCore Blit2 {
	blit2Next = mnxt,
	blit2SrcSubresource = ssr,
	blit2SrcOffsetFrom = sof, blit2SrcOffsetTo = sot,
	blit2DstSubresource = dsr,
	blit2DstOffsetFrom = dof, blit2DstOffsetTo = dot } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	f C.Blit2 {
	C.blit2SType = (), C.blit2PNext = pnxt',
	C.blit2SrcSubresource = subresourceLayersToCore ssr,
	C.blit2SrcOffsets = [sof, sot],
	C.blit2DstSubresource = subresourceLayersToCore dsr,
	C.blit2DstOffsets = [dof, dot] }

data Subresource = Subresource {
	subresourceAspectMask :: AspectFlags,
	subresourceMipLevel :: Word32,
	subresourceArrayLayer :: Word32 }
	deriving Show

subresourceToCore :: Subresource -> C.Subresource
subresourceToCore Subresource {
	subresourceAspectMask = AspectFlagBits am,
	subresourceMipLevel = ml, subresourceArrayLayer = al } = C.Subresource {
	C.subresourceAspectMask = am,
	C.subresourceMipLevel = ml, C.subresourceArrayLayer = al }
