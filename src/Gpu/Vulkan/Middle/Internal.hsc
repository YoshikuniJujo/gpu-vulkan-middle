{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Middle.Internal (
	ApplicationInfo(..), applicationInfoToCore,
	ApiVersion(..),
	makeApiVersion, fromApiVersion, Variant, Major, Minor, Patch,
	apiVersion_1_0, apiVersion_1_1, apiVersion_1_2, apiVersion_1_3,
	LayerProperties(..), layerPropertiesFromCore,
	ExtensionProperties(..), extensionPropertiesFromCore,
	StencilOpState(..), stencilOpStateToCore,
	ClearValue(..), ClearValueListToCore(..),
	ClearValueToCore, ClearColorValueToCore(..),
	clearValueListToArray,
	ClearType(..), ClearColorType(..),

	C.ClearDepthStencilValue, pattern C.ClearDepthStencilValue,
	C.clearDepthStencilValueDepth, C.clearDepthStencilValueStencil,

	SubmitInfo(..), SubmitInfoListToCore(..),
	SubmitInfo2(..), SubmitInfo2ListToCore(..),

	FormatProperties(..), formatPropertiesFromCore,

	C.Rect2d, pattern C.Rect2d, C.rect2dExtent, C.rect2dOffset,

	C.Offset2d, pattern C.Offset2d, C.offset2dX, C.offset2dY,
	C.Offset3d, pattern C.Offset3d, C.offset3dX, C.offset3dY, C.offset3dZ,

	C.Extent2d, pattern C.Extent2d, C.extent2dWidth, C.extent2dHeight,
	C.Extent3d,
	pattern C.Extent3d, C.extent3dWidth, C.extent3dHeight, C.extent3dDepth,

	C.Viewport, pattern C.Viewport,
	C.viewportX, C.viewportY, C.viewportWidth, C.viewportHeight,
	C.viewportMinDepth, C.viewportMaxDepth,

	Size(..),

	DependencyInfo(..), dependencyInfoToCore,
	BlitImageInfo2(..), blitImageInfo2ToCore,

	remainingMipLevels, remainingArrayLayers

	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Arrow
import Control.Monad
import Data.Kind
import Data.Bits
import Data.Default
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List qualified as TL
import Data.HeteroParList qualified as HPList
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word
import Data.Text.Foreign.MiscYj
import Data.Color.Internal
import Data.IORef

import qualified Data.Text as T

import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.Core as C

import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.Semaphore.Middle.Internal as Semaphore
import {-# SOURCE #-} qualified
	Gpu.Vulkan.CommandBuffer.Middle.Internal as CommandBuffer

import Gpu.Vulkan.Middle.Types

import qualified Gpu.Vulkan.Memory.Middle.Internal as Memory
import {-# SOURCE #-} qualified Gpu.Vulkan.Buffer.Middle.Internal as Buffer
import qualified Gpu.Vulkan.Image.Middle.Internal as Image
import qualified Gpu.Vulkan.Image.Enum as Image

#include <vulkan/vulkan.h>

data ApplicationInfo mn = ApplicationInfo {
	applicationInfoNext :: TMaybe.M mn,
	applicationInfoApplicationName :: T.Text,
	applicationInfoApplicationVersion :: ApiVersion,
	applicationInfoEngineName :: T.Text,
	applicationInfoEngineVersion :: ApiVersion,
	applicationInfoApiVersion :: ApiVersion }

deriving instance Show (TMaybe.M mn) => Show (ApplicationInfo mn)

newtype ApiVersion = ApiVersion C.ApiVersion
	deriving (Show, Eq, Ord, Storable, Bits)

apiVersion_1_0, apiVersion_1_1, apiVersion_1_2, apiVersion_1_3 :: ApiVersion
apiVersion_1_0 = ApiVersion C.apiVersion_1_0
apiVersion_1_1 = ApiVersion C.apiVersion_1_1
apiVersion_1_2 = ApiVersion C.apiVersion_1_2
apiVersion_1_3 = ApiVersion C.apiVersion_1_3

type Variant = Word8	-- 0 <= variant < 8
type Major = Word8	-- 0 <= major < 127
type Minor = Word16	-- 0 <= minor < 1023
type Patch = Word16	-- 0 <= patch < 4095

makeApiVersion :: Variant -> Major -> Minor -> Patch -> ApiVersion
makeApiVersion v mj mn p = ApiVersion $ C.makeApiVersion v mj mn p

fromApiVersion :: ApiVersion -> (Variant, Major, Minor, Patch)
fromApiVersion (ApiVersion av) = let
	vr = fromIntegral $ av `shiftR` 29
	mj = fromIntegral $ (av `shiftR` 22) .&. 0x7f
	mn = fromIntegral $ (av `shiftR` 12) .&. 0x3ff
	pt = fromIntegral $ av .&. 0xfff in (vr, mj, mn, pt)

applicationInfoToCore :: WithPoked (TMaybe.M mn) =>
	ApplicationInfo mn -> (Ptr C.ApplicationInfo -> IO a) -> IO ()
applicationInfoToCore ApplicationInfo {
	applicationInfoNext = mnxt,
	applicationInfoApplicationName = anm,
	applicationInfoApplicationVersion = (\(ApiVersion v) -> v) -> appv,
	applicationInfoEngineName = enm,
	applicationInfoEngineVersion = (\(ApiVersion v) -> v) -> engv,
	applicationInfoApiVersion = (\(ApiVersion v) -> v) -> apiv
	} f = withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	textToCString anm \canm -> textToCString enm \cenm ->
	let	appInfo = C.ApplicationInfo {
			C.applicationInfoSType = (),
			C.applicationInfoPNext = pnxt',
			C.applicationInfoPApplicationName = canm,
			C.applicationInfoApplicationVersion = appv,
			C.applicationInfoPEngineName = cenm,
			C.applicationInfoEngineVersion = engv,
			C.applicationInfoApiVersion = apiv } in
	withPoked appInfo f

data ExtensionProperties = ExtensionProperties {
	extensionPropertiesExtensionName :: T.Text,
	extensionPropertiesSpecVersion :: ApiVersion }
	deriving Show

extensionPropertiesFromCore :: C.ExtensionProperties -> ExtensionProperties
extensionPropertiesFromCore C.ExtensionProperties {
	C.extensionPropertiesExtensionName = en,
	C.extensionPropertiesSpecVersion = sv } = ExtensionProperties {
		extensionPropertiesExtensionName = en,
		extensionPropertiesSpecVersion = ApiVersion sv }

data LayerProperties = LayerProperties {
	layerPropertiesLayerName :: T.Text,
	layerPropertiesSpecVersion :: ApiVersion,
	layerPropertiesImplementationVersion :: ApiVersion,
	layerPropertiesDescription :: T.Text }
	deriving Show

layerPropertiesFromCore :: C.LayerProperties -> LayerProperties
layerPropertiesFromCore C.LayerProperties {
	C.layerPropertiesLayerName = ln,
	C.layerPropertiesSpecVersion = sv,
	C.layerPropertiesImplementationVersion = iv,
	C.layerPropertiesDescription = dsc } = LayerProperties {
	layerPropertiesLayerName = ln,
	layerPropertiesSpecVersion = ApiVersion sv,
	layerPropertiesImplementationVersion = ApiVersion iv,
	layerPropertiesDescription = dsc }

data StencilOpState = StencilOpState {
	stencilOpStateFailOp :: StencilOp,
	stencilOpStatePassOp :: StencilOp,
	stencilOpStateDepthFailOp :: StencilOp,
	stencilOpStateCompareOp :: CompareOp,
	stencilOpStateCompareMask :: Word32,
	stencilOpStateWriteMask :: Word32,
	stencilOpStateReference :: Word32 }
	deriving Show

instance Default StencilOpState where def = stencilOpStateZero

stencilOpStateZero :: StencilOpState
stencilOpStateZero = StencilOpState {
	stencilOpStateFailOp = StencilOpKeep,
	stencilOpStatePassOp = StencilOpKeep,
	stencilOpStateDepthFailOp = StencilOpKeep,
	stencilOpStateCompareOp = CompareOpNever,
	stencilOpStateCompareMask = 0,
	stencilOpStateWriteMask = 0,
	stencilOpStateReference = 0 }

stencilOpStateToCore :: StencilOpState -> C.StencilOpState
stencilOpStateToCore StencilOpState {
	stencilOpStateFailOp = StencilOp fo,
	stencilOpStatePassOp = StencilOp po,
	stencilOpStateDepthFailOp = StencilOp dfo,
	stencilOpStateCompareOp = CompareOp co,
	stencilOpStateCompareMask = cm,
	stencilOpStateWriteMask = wm,
	stencilOpStateReference = rf } = C.StencilOpState {
		C.stencilOpStateFailOp = fo,
		C.stencilOpStatePassOp = po,
		C.stencilOpStateDepthFailOp = dfo,
		C.stencilOpStateCompareOp = co,
		C.stencilOpStateCompareMask = cm,
		C.stencilOpStateWriteMask = wm,
		C.stencilOpStateReference = rf }

data ClearValue (ct :: ClearType) where
	ClearValueColor :: Rgba Float -> ClearValue ('ClearTypeColor cct)
	ClearValueDepthStencil ::
		C.ClearDepthStencilValue -> ClearValue 'ClearTypeDepthStencil

class ClearColorValueToCore (cct :: ClearColorType) where
	clearColorValueToCore ::
		ClearValue ('ClearTypeColor cct) ->
		(Ptr C.ClearColorValue -> IO a) -> IO a

instance ClearColorValueToCore 'ClearColorTypeFloat32 where
	clearColorValueToCore (ClearValueColor (RgbaDouble r g b a)) f =
		allocaArray 4 \prgba -> do
			pokeArray prgba [r, g, b, a]
			f $ C.clearColorValueFromFloats prgba

instance ClearColorValueToCore 'ClearColorTypeInt32 where
	clearColorValueToCore (ClearValueColor (RgbaInt32 r g b a)) f =
		allocaArray 4 \prgba -> do
			pokeArray prgba [r, g, b, a]
			f $ C.clearColorValueFromInts prgba

instance ClearColorValueToCore 'ClearColorTypeUint32 where
	clearColorValueToCore (ClearValueColor (RgbaWord32 r g b a)) f =
		allocaArray 4 \prgba -> do
			pokeArray prgba [r, g, b, a]
			f $ C.clearColorValueFromUints prgba

deriving instance Show (ClearValue ct)

data ClearType = ClearTypeColor ClearColorType | ClearTypeDepthStencil
	deriving Show

data ClearColorType
	= ClearColorTypeFloat32 | ClearColorTypeInt32 | ClearColorTypeUint32
	deriving Show

class ClearValueToCore (ct :: ClearType) where
	clearValueToCore :: ClearValue ct -> (Ptr C.ClearValue -> IO a) -> IO a

instance ClearValueToCore 'ClearTypeDepthStencil where
	clearValueToCore (ClearValueDepthStencil cdsv) =
		C.clearValueFromClearDepthStencilValue cdsv

instance ClearColorValueToCore cct =>
	ClearValueToCore ('ClearTypeColor cct) where
	clearValueToCore cv@(ClearValueColor _) f =
		clearColorValueToCore cv $ f . C.clearValueFromClearColorValue

class TL.Length cts => ClearValueListToCore (cts :: [ClearType]) where
	clearValueListToCore ::
		HeteroParList.PL ClearValue cts ->
		([Ptr C.ClearValue] -> IO a) -> IO a

instance ClearValueListToCore '[] where clearValueListToCore HeteroParList.Nil = ($ [])

instance (ClearValueToCore ct, ClearValueListToCore cts) =>
	ClearValueListToCore (ct ': cts) where
	clearValueListToCore (cv :** cvs) f =
		clearValueToCore cv \ccv ->
		clearValueListToCore cvs \ccvs -> f $ ccv : ccvs

clearValueListToArray :: [Ptr C.ClearValue] -> (Ptr C.ClearValue -> IO a) -> IO a
clearValueListToArray (length &&& id -> (pcvc, pcvl)) f =
	allocaClearValueArray pcvc \pcva -> do
		pokeClearValueArray pcva pcvl
		f pcva

allocaClearValueArray :: Int -> (Ptr C.ClearValue -> IO a) -> IO a
allocaClearValueArray n = allocaBytesAligned
	(alignedSize #{size VkClearValue} #{alignment VkClearValue} * n)
	#{alignment VkClearValue}

alignedSize :: Int -> Int -> Int
alignedSize sz al = (sz - 1) `div` al * al + 1

pokeClearValueArray :: Ptr C.ClearValue -> [Ptr C.ClearValue] -> IO ()
pokeClearValueArray p lst = zipWithM_ pokeClearValue (clearValueArrayPtrs p) lst

clearValueArrayPtrs :: Ptr C.ClearValue -> [Ptr C.ClearValue]
clearValueArrayPtrs = iterate (
	(`alignPtr` #{alignment VkClearValue})
		. (`plusPtr` #{size VkClearValue}) )

pokeClearValue :: Ptr C.ClearValue -> Ptr C.ClearValue -> IO ()
pokeClearValue dst src = copyBytes dst src #{size VkClearValue}

data SubmitInfo mn = SubmitInfo {
	submitInfoNext :: TMaybe.M mn,
	submitInfoWaitSemaphoreDstStageMasks ::
		[(Semaphore.S, Pipeline.StageFlags)],
	submitInfoCommandBuffers :: [CommandBuffer.C],
	submitInfoSignalSemaphores :: [Semaphore.S] }

class SubmitInfoListToCore ns where
	submitInfoListToCore :: HeteroParList.PL SubmitInfo ns ->
		([C.SubmitInfo] -> IO a) -> IO ()

instance SubmitInfoListToCore '[] where
	submitInfoListToCore HeteroParList.Nil f = () <$ f []

instance (WithPoked (TMaybe.M n), SubmitInfoListToCore ns) =>
	SubmitInfoListToCore (n ': ns) where
	submitInfoListToCore (ci :** cis) f = submitInfoToCore @n ci \cci ->
		submitInfoListToCore cis \ccis -> f $ cci : ccis

submitInfoToCore :: WithPoked (TMaybe.M mn) =>
	SubmitInfo mn -> (C.SubmitInfo -> IO a) -> IO ()
submitInfoToCore SubmitInfo {
	submitInfoNext = mnxt,
	submitInfoWaitSemaphoreDstStageMasks =
		length &&&
		(	(Semaphore.unS <$>) ***
			(Pipeline.unStageFlagBits <$>)) . unzip ->
		(wsc, (wss, wdsms)),
	submitInfoCommandBuffers = (length &&& id) . map CommandBuffer.unC -> (cbc, cbs),
	submitInfoSignalSemaphores =
		length &&& (Semaphore.unS <$>) -> (ssc, sss) } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray wsc \pwss ->
	pokeArray pwss wss >>
	allocaArray wsc \pwdsms ->
	pokeArray pwdsms wdsms >>
	allocaArray cbc \pcbs ->
	pokeArray pcbs cbs >>
	allocaArray ssc \psss ->
	pokeArray psss sss >>
	f C.SubmitInfo {
		C.submitInfoSType = (),
		C.submitInfoPNext = pnxt',
		C.submitInfoWaitSemaphoreCount = fromIntegral wsc,
		C.submitInfoPWaitSemaphores = pwss,
		C.submitInfoPWaitDstStageMask = pwdsms,
		C.submitInfoCommandBufferCount = fromIntegral cbc,
		C.submitInfoPCommandBuffers = pcbs,
		C.submitInfoSignalSemaphoreCount = fromIntegral ssc,
		C.submitInfoPSignalSemaphores = psss }

data SubmitInfo2 mn wsas cbas ssas = SubmitInfo2 {
	submitInfo2Next :: TMaybe.M mn,
	submitInfo2Flags :: SubmitFlags,
	submitInfo2WaitSemaphoreInfos :: HPList.PL Semaphore.SubmitInfo wsas,
	submitInfo2CommandBufferInfos ::
		HPList.PL CommandBuffer.SubmitInfo cbas,
	submitInfo2SignalSemaphoreInfos :: HPList.PL Semaphore.SubmitInfo ssas }

class SubmitInfo2ListToCore si2as where
	submitInfo2ListToCore ::
		HeteroParList.PL (U4 SubmitInfo2) si2as ->
		([C.SubmitInfo2] -> IO a) -> IO ()

instance SubmitInfo2ListToCore '[] where
	submitInfo2ListToCore HPList.Nil f = void $ f []

instance (
	WithPoked (TMaybe.M mn),
	HPList.ToListWithCCpsM' WithPoked TMaybe.M wsas, TL.Length wsas,
	HPList.ToListWithCCpsM' WithPoked TMaybe.M cbas, TL.Length cbas,
	HPList.ToListWithCCpsM' WithPoked TMaybe.M ssas, TL.Length ssas,
	SubmitInfo2ListToCore si2as ) =>
	SubmitInfo2ListToCore ('(mn, wsas, cbas, ssas) ': si2as) where
	submitInfo2ListToCore (U4 si :** sis) f =
		submitInfo2ToCore si \csi ->
		submitInfo2ListToCore sis \csis -> f $ csi : csis

submitInfo2ToCore :: forall mn wsas cbas ssas r . (
	WithPoked (TMaybe.M mn),
	HPList.ToListWithCCpsM' WithPoked TMaybe.M wsas, TL.Length wsas,
	HPList.ToListWithCCpsM' WithPoked TMaybe.M cbas, TL.Length cbas,
	HPList.ToListWithCCpsM' WithPoked TMaybe.M ssas, TL.Length ssas
	) =>
	SubmitInfo2 mn wsas cbas ssas -> (C.SubmitInfo2 -> IO r) -> IO ()
submitInfo2ToCore SubmitInfo2 {
	submitInfo2Next = mnxt,
	submitInfo2Flags = SubmitFlagBits fs,
	submitInfo2WaitSemaphoreInfos = wss,
	submitInfo2CommandBufferInfos = cbs,
	submitInfo2SignalSemaphoreInfos = sss } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray wsc \pwss -> allocaArray cbc \pcbs ->
	allocaArray ssc \psss -> do
	HPList.withListWithCCpsM' @_ @WithPoked @TMaybe.M
		wss Semaphore.submitInfoToCore \cwss -> pokeArray pwss cwss
	HPList.withListWithCCpsM' @_ @WithPoked @TMaybe.M
		cbs CommandBuffer.submitInfoToCore \ccbs -> pokeArray pcbs ccbs
	HPList.withListWithCCpsM' @_ @WithPoked @TMaybe.M
		sss Semaphore.submitInfoToCore \csss -> pokeArray psss csss
	f C.SubmitInfo2 {
		C.submitInfo2SType = (),
		C.submitInfo2PNext = pnxt',
		C.submitInfo2Flags = fs,
		C.submitInfo2WaitSemaphoreInfoCount = wsc,
		C.submitInfo2PWaitSemaphoreInfos = pwss,
		C.submitInfo2CommandBufferInfoCount = cbc,
		C.submitInfo2PCommandBufferInfos = pcbs,
		C.submitInfo2SignalSemaphoreInfoCount = ssc,
		C.submitInfo2PSignalSemaphoreInfos = psss }
	where
	wsc, cbc, ssc :: Integral n => n
	wsc = TL.length @_ @wsas
	cbc = TL.length @_ @cbas
	ssc = TL.length @_ @ssas

data FormatProperties = FormatProperties {
	formatPropertiesLinearTilingFeatures :: FormatFeatureFlags,
	formatPropertiesOptimalTilingFeatures :: FormatFeatureFlags,
	formatPropertiesBufferFeatures :: FormatFeatureFlags }
	deriving Show

formatPropertiesFromCore :: C.FormatProperties -> FormatProperties
formatPropertiesFromCore C.FormatProperties {
	C.formatPropertiesLinearTilingFeatures = ltfs,
	C.formatPropertiesOptimalTilingFeatures = otfs,
	C.formatPropertiesBufferFeatures = bfs
	} = FormatProperties {
		formatPropertiesLinearTilingFeatures =
			FormatFeatureFlagBits ltfs,
		formatPropertiesOptimalTilingFeatures =
			FormatFeatureFlagBits otfs,
		formatPropertiesBufferFeatures = FormatFeatureFlagBits bfs }

data DependencyInfo mn mbs bmbs imbs = DependencyInfo {
	dependencyInfoNext :: TMaybe.M mn,
	dependencyInfoDependencyFlags :: DependencyFlags,
	dependencyInfoMemoryBarriers :: HPList.PL Memory.Barrier2 mbs,
	dependencyInfoBufferMemoryBarriers ::
		HPList.PL Buffer.MemoryBarrier2 bmbs,
	dependencyInfoImageMemoryBarriers ::
		HPList.PL Image.MemoryBarrier2 imbs }

dependencyInfoToCore :: forall mn (mbs :: [Maybe Type]) bmbs imbs a . (
	WithPoked (TMaybe.M mn),
	HPList.ToListWithCCpsM' WithPoked TMaybe.M mbs, TL.Length mbs,
	HPList.ToListWithCCpsM' WithPoked TMaybe.M bmbs, TL.Length bmbs,
	HPList.ToListWithCCpsM' WithPoked TMaybe.M imbs, TL.Length imbs ) =>
	DependencyInfo mn mbs bmbs imbs -> (C.DependencyInfo -> IO a) -> IO ()
dependencyInfoToCore DependencyInfo {
	dependencyInfoNext = mnxt,
	dependencyInfoDependencyFlags = DependencyFlagBits fs,
	dependencyInfoMemoryBarriers = mbs,
	dependencyInfoBufferMemoryBarriers = bmbs,
	dependencyInfoImageMemoryBarriers = imbs } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray mbc \pmbs ->
	allocaArray bmbc \pbmbs -> allocaArray imbc \pimbs -> do
	HPList.withListWithCCpsM' @_ @WithPoked @TMaybe.M
		mbs Memory.barrier2ToCore \cmbs -> pokeArray pmbs cmbs
	HPList.withListWithCCpsM' @_ @WithPoked @TMaybe.M
		bmbs Buffer.memoryBarrier2ToCore \cbmbs -> pokeArray pbmbs cbmbs
	HPList.withListWithCCpsM' @_ @WithPoked @TMaybe.M
		imbs Image.memoryBarrier2ToCore \cimbs -> pokeArray pimbs cimbs
	f C.DependencyInfo {
		C.dependencyInfoSType = (),
		C.dependencyInfoPNext = pnxt',
		C.dependencyInfoDependencyFlags = fs,
		C.dependencyInfoMemoryBarrierCount = mbc,
		C.dependencyInfoPMemoryBarriers = pmbs,
		C.dependencyInfoBufferMemoryBarrierCount = bmbc,
		C.dependencyInfoPBufferMemoryBarriers = pbmbs,
		C.dependencyInfoImageMemoryBarrierCount = imbc,
		C.dependencyInfoPImageMemoryBarriers = pimbs }
	where
	mbc, bmbc, imbc :: Integral n => n
	mbc = TL.length @_ @mbs
	bmbc = TL.length @_ @bmbs
	imbc = TL.length @_ @imbs

data BlitImageInfo2 mn ras = BlitImageInfo2 {
	blitImageInfo2Next :: TMaybe.M mn,
	blitImageInfo2SrcImage :: Image.I,
	blitImageInfo2SrcImageLayout :: Image.Layout,
	blitImageInfo2DstImage :: Image.I,
	blitImageInfo2DstImageLayout :: Image.Layout,
	blitImageInfo2Regions :: HPList.PL Image.Blit2 ras,
	blitImageInfo2Filter :: Filter }

blitImageInfo2ToCore ::
	forall mn ras a . (
	TL.Length ras, HeteroParList.ToListWithCCpsM' WithPoked TMaybe.M ras
	) =>
	WithPoked (TMaybe.M mn) =>
	BlitImageInfo2 mn ras -> (Ptr C.BlitImageInfo2 -> IO a) -> IO ()
blitImageInfo2ToCore BlitImageInfo2 {
	blitImageInfo2Next = mnxt,
	blitImageInfo2SrcImage = Image.I rsi,
	blitImageInfo2SrcImageLayout = Image.Layout sil,
	blitImageInfo2DstImage = Image.I rdi,
	blitImageInfo2DstImageLayout = Image.Layout dil,
	blitImageInfo2Regions = rs,
	blitImageInfo2Filter = Filter flt
	} f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	readIORef rsi >>= \(_, si) ->
	readIORef rdi >>= \(_, di) ->
	allocaArray rc \prs -> do
	HPList.withListWithCCpsM' @_ @WithPoked @TMaybe.M
		rs Image.blit2ToCore \crs -> pokeArray prs crs
	let	cbii = C.BlitImageInfo2 {
			C.blitImageInfo2SType = (),
			C.blitImageInfo2PNext = pnxt',
			C.blitImageInfo2SrcImage = si,
			C.blitImageInfo2SrcImageLayout = sil,
			C.blitImageInfo2DstImage = di,
			C.blitImageInfo2DstImageLayout = dil,
			C.blitImageInfo2RegionCount = rc,
			C.blitImageInfo2PRegions = prs,
			C.blitImageInfo2Filter = flt }
	withPoked cbii f
	where
	rc :: Integral n => n
	rc = TL.length @_ @ras

remainingMipLevels, remainingArrayLayers :: Word32
remainingMipLevels = #{const VK_REMAINING_MIP_LEVELS}
remainingArrayLayers = #{const VK_REMAINING_ARRAY_LAYERS}
