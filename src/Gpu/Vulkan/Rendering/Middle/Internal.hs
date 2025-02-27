{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Rendering.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Word
import Data.Int
import Data.Color

import Gpu.Vulkan.Middle.Internal qualified as Vk
import Gpu.Vulkan.Enum as Vk
import Gpu.Vulkan.Image.Enum qualified as Img
import Gpu.Vulkan.ImageView.Middle.Internal qualified as ImgVw
import Gpu.Vulkan.Attachment.Enum qualified as Att

import Gpu.Vulkan.Rendering.Core qualified as C

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
