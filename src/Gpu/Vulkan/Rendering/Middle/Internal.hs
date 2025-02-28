{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Rendering.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.List qualified as TList
import Data.HeteroParList qualified as HPList
import Data.Default
import Data.Word
import Data.Int
import Data.Color

import Gpu.Vulkan.Middle.Internal qualified as Vk
import Gpu.Vulkan.Enum as Vk
import Gpu.Vulkan.Image.Enum qualified as Img
import Gpu.Vulkan.ImageView.Middle.Internal qualified as ImgVw
import Gpu.Vulkan.Attachment.Enum qualified as Att

import Gpu.Vulkan.Rendering.Core qualified as C

data Info mn cas das sas = Info {
	infoNext :: TMaybe.M mn,
	infoFlags :: Vk.RenderingFlags,
	infoRenderArea :: Vk.Rect2d,
	infoLayerCount :: Word32,
	infoViewMask :: Word32,
	infoColorAttachments :: HPList.PL (U2 AttachmentInfo) cas,
	infoDepthAttachment :: TPMaybe.M (U2 AttachmentInfo) das,
	infoStencilAttachment :: TPMaybe.M (U2 AttachmentInfo) sas }

deriving instance (
	Show (TMaybe.M mn),
	Show (HPList.PL (U2 AttachmentInfo) cas),
	Show (TPMaybe.M (U2 AttachmentInfo) das),
	Show (TPMaybe.M (U2 AttachmentInfo) sas)
	) => Show (Info mn cas das sas)

infoToCore :: forall mn cas das sas a . (
	WithPoked (TMaybe.M mn), TList.Length cas,
	HPList.ToListWithCCpsM'' AttachmentInfoToCore cas,
	AttachmentInfoToCoreMaybe das,
	AttachmentInfoToCoreMaybe sas ) =>
	Info mn cas das sas -> (C.Info -> IO a) -> IO ()
infoToCore Info {
	infoNext = mnxt, infoFlags = Vk.RenderingFlagBits flg,
	infoRenderArea = ra, infoLayerCount = lc,
	infoViewMask = vm,
	infoColorAttachments = cas,
	infoDepthAttachment = da,
	infoStencilAttachment = sa
	} f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray cac \pcas -> alloca \pda -> alloca \psa -> do
	HPList.withListWithCCpsM'' @AttachmentInfoToCore
		cas attachmentInfoToCore' \ccas -> pokeArray pcas ccas
	attachmentInfoToCore'' da \cda -> poke pda cda
	attachmentInfoToCore'' sa \csa -> poke psa csa
	f C.Info {
		C.infoSType = (), C.infoPNext = pnxt', C.infoFlags = flg,
		C.infoRenderArea = ra, C.infoLayerCount = lc,
		C.infoViewMask = vm,
		C.infoColorAttachmentCount = cac,
		C.infoPColorAttachments = pcas,
		C.infoPDepthAttachment = pda,
		C.infoPStencilAttachment = psa }
	where
	cac :: Integral n => n
	cac = TList.length @_ @cas

data AttachmentInfo mn ct = AttachmentInfo {
	attachmentInfoNext :: TMaybe.M mn,
	attachmentInfoImageView :: ImgVw.I,
	attachmentInfoImageLayout :: Img.Layout,
	attachmentInfoResolveMode :: ResolveModeFlagBits,
	attachmentInfoResolveImageView :: ImgVw.I,
	attachmentInfoResolveImageLayout :: Img.Layout,
	attachmentInfoLoadOp :: Att.LoadOp,
	attachmentInfoStoreOp :: Att.StoreOp,
	attachmentInfoClearValue :: Vk.ClearValue ct }

deriving instance Show (TMaybe.M mn) => Show (AttachmentInfo mn ct)

attachmentInfoZero ::
	Default (Vk.ClearValue ct) => TMaybe.M mn -> IO (AttachmentInfo mn ct)
attachmentInfoZero mnxt = do
	niv <- ImgVw.null
	nriv <- ImgVw.null
	pure AttachmentInfo {
		attachmentInfoNext = mnxt,
		attachmentInfoImageView = niv,
		attachmentInfoImageLayout = Img.Layout 0,
		attachmentInfoResolveMode = ResolveModeFlagBits 0,
		attachmentInfoResolveImageView = nriv,
		attachmentInfoResolveImageLayout = Img.Layout 0,
		attachmentInfoLoadOp = Att.LoadOp 0,
		attachmentInfoStoreOp = Att.StoreOp 0,
		attachmentInfoClearValue = def }

class (WithPoked (TMaybe.M (I0_2 mnct)), ClearValueToCore (I1_2 mnct)) =>
	AttachmentInfoToCore mnct

class AttachmentInfoToCoreMaybe mmnct where
	attachmentInfoToCore'' ::
		TPMaybe.M (U2 AttachmentInfo) mmnct -> (C.AttachmentInfo -> IO r) -> IO ()

instance AttachmentInfoToCoreMaybe 'Nothing where
	attachmentInfoToCore'' TPMaybe.N _ = pure ()

instance AttachmentInfoToCore mnct => AttachmentInfoToCoreMaybe ('Just mnct) where
	attachmentInfoToCore'' (TPMaybe.J ai) = attachmentInfoToCore' ai

attachmentInfoToCore' :: AttachmentInfoToCore mnct =>
	U2 AttachmentInfo mnct -> (C.AttachmentInfo -> IO r) -> IO ()
attachmentInfoToCore' (U2 ai) = attachmentInfoToCore ai

attachmentInfoToCore :: (WithPoked (TMaybe.M mn), ClearValueToCore ct) =>
	AttachmentInfo mn ct -> (C.AttachmentInfo -> IO r) -> IO ()
attachmentInfoToCore AttachmentInfo {
	attachmentInfoNext = mnxt,
	attachmentInfoImageView = iv,
	attachmentInfoImageLayout = Img.Layout il,
	attachmentInfoResolveMode = Vk.ResolveModeFlagBits rm,
	attachmentInfoResolveImageView = riv,
	attachmentInfoResolveImageLayout = Img.Layout ril,
	attachmentInfoLoadOp = Att.LoadOp lo,
	attachmentInfoStoreOp = Att.StoreOp so,
	attachmentInfoClearValue = cv } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	alloca \piv -> alloca \priv -> do
	poke piv =<< ImgVw.iToCore iv
	poke priv =<< ImgVw.iToCore riv
	f C.AttachmentInfo {
		C.attachmentInfoSType = (),
		C.attachmentInfoPNext = pnxt',
		C.attachmentInfoImageView = piv,
		C.attachmentInfoImageLayout = il,
		C.attachmentInfoResolveMode = rm,
		C.attachmentInfoResolveImageView = priv,
		C.attachmentInfoResolveImageLayout = ril,
		C.attachmentInfoLoadOp = lo,
		C.attachmentInfoStoreOp = so,
		C.attachmentInfoClearValue = clearValueToCore cv }

class ClearValueToCore (ct :: Vk.ClearType) where
	clearValueToCore :: Vk.ClearValue ct -> C.ClearValue

instance ClearColorValueToCore cct =>
	ClearValueToCore (Vk.ClearTypeColor cct) where
	clearValueToCore = C.clearColorValueToClearValue . clearColorValueToCore

instance ClearValueToCore Vk.ClearTypeDepthStencil where
	clearValueToCore (Vk.ClearValueDepthStencil dpst) =
		C.clearDepthStencilValueToClearValue dpst

class ClearColorValueToCore (cct :: Vk.ClearColorType) where
	type ClearColorTypeCore cct
	clearColorValueToCore :: Vk.ClearValue ('Vk.ClearTypeColor cct) ->
		C.ClearColorValue (ClearColorTypeCore cct)

instance ClearColorValueToCore 'Vk.ClearColorTypeFloat32 where
	type ClearColorTypeCore 'Vk.ClearColorTypeFloat32 = Float
	clearColorValueToCore (Vk.ClearValueColor (RgbaDouble r g b a)) =
		C.ClearColorValueFloat r g b a

instance ClearColorValueToCore 'Vk.ClearColorTypeInt32 where
	type ClearColorTypeCore 'Vk.ClearColorTypeInt32 = Int32
	clearColorValueToCore (Vk.ClearValueColor (RgbaInt32 r g b a)) =
		C.ClearColorValueInt r g b a

instance ClearColorValueToCore 'Vk.ClearColorTypeUint32 where
	type ClearColorTypeCore 'Vk.ClearColorTypeUint32 = Word32
	clearColorValueToCore (Vk.ClearValueColor (RgbaWord32 r g b a)) =
		C.ClearColorValueUint r g b a
