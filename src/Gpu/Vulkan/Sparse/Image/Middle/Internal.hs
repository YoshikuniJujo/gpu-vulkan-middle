{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Image.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke
import Control.Arrow
import Data.IORef

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Device.Middle.Internal qualified as Device
import Gpu.Vulkan.Memory.Middle.Internal qualified as Memory
import Gpu.Vulkan.Image.Middle.Internal qualified as Image
import Gpu.Vulkan.Sparse.Enum qualified as S
import Gpu.Vulkan.Sparse.Middle.Internal qualified as S

import Gpu.Vulkan.Sparse.Image.Core qualified as C

data OpaqueMemoryBindInfo = OpaqueMemoryBindInfo {
	opaqueMemoryBindInfoImage :: Image.I,
	opaqueMemoryBindInfoBinds :: [S.MemoryBind] }

opaqueMemoryBindInfoToCore ::
	OpaqueMemoryBindInfo -> (C.OpaqueMemoryBindInfo -> IO a) -> IO a
opaqueMemoryBindInfoToCore OpaqueMemoryBindInfo {
	opaqueMemoryBindInfoImage = Image.I ir,
	opaqueMemoryBindInfoBinds = length &&& id -> (bc, bs) } f = do
	(_, i) <- readIORef ir
	cbs <- S.memoryBindToCore `mapM` bs
	allocaArray bc \pbs -> do
		pokeArray pbs cbs
		f C.OpaqueMemoryBindInfo {
			C.opaqueMemoryBindInfoImage = i,
			C.opaqueMemoryBindInfoBindCount = fromIntegral bc,
			C.opaqueMemoryBindInfoPBinds = pbs }

data MemoryBindInfo = MemoryBindInfo {
	memoryBindInfoImage :: Image.I,
	memoryBindInfoBinds :: [MemoryBind] }

memoryBindInfoToCore ::
	MemoryBindInfo -> (C.MemoryBindInfo -> IO a) -> IO a
memoryBindInfoToCore MemoryBindInfo {
	memoryBindInfoImage = Image.I ir,
	memoryBindInfoBinds = length &&& id -> (bc, bs) } f =  do
	(_, i) <- readIORef ir
	cbs <- memoryBindToCore `mapM` bs
	allocaArray bc \pbs -> do
		pokeArray pbs cbs
		f C.MemoryBindInfo {
			C.memoryBindInfoImage = i,
			C.memoryBindInfoBindCount = fromIntegral bc,
			C.memoryBindInfoPBinds = pbs }

data MemoryBind = MemoryBind {
	memoryBindSubresource :: Image.Subresource,
	memoryBindOffset :: Offset3d,
	memoryBindExtent :: Extent3d,
	memoryBindMemory :: Memory.M,
	memoryBindMemoryOffset :: Device.Size,
	memoryBindFlags :: S.MemoryBindFlags }

memoryBindToCore :: MemoryBind -> IO C.MemoryBind
memoryBindToCore MemoryBind {
	memoryBindSubresource = sr,
	memoryBindOffset = o,
	memoryBindExtent = e,
	memoryBindMemory = Memory.M rm,
	memoryBindMemoryOffset = Device.Size mo,
	memoryBindFlags = S.MemoryBindFlagBits fs } = do
	m <- readIORef rm
	pure C.MemoryBind {
		C.memoryBindSubresource = Image.subresourceToCore sr,
		C.memoryBindOffset = o,
		C.memoryBindExtent = e,
		C.memoryBindMemory = m,
		C.memoryBindMemoryOffset = mo,
		C.memoryBindFlags = fs }
