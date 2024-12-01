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

import Gpu.Vulkan.Image.Middle.Internal qualified as Image
import Gpu.Vulkan.Sparse.Middle.Internal

import Gpu.Vulkan.Sparse.Image.Core qualified as C

data OpaqueMemoryBindInfo = OpaqueMemoryBindInfo {
	opaqueMemoryBindInfoImage :: Image.I,
	opaqueMemoryBindInfoBinds :: [MemoryBind] }

opaqueMemoryBindInfoToCore ::
	OpaqueMemoryBindInfo -> (Ptr C.OpaqueMemoryBindInfo -> IO a) -> IO a
opaqueMemoryBindInfoToCore OpaqueMemoryBindInfo {
	opaqueMemoryBindInfoImage = Image.I ir,
	opaqueMemoryBindInfoBinds = length &&& id -> (bc, bs) } f = do
	(_, i) <- readIORef ir
	cbs <- memoryBindToCore `mapM` bs
	allocaArray bc \pbs -> do
		pokeArray pbs cbs
		withPoked C.OpaqueMemoryBindInfo {
			C.opaqueMemoryBindInfoImage = i,
			C.opaqueMemoryBindInfoBindCount = fromIntegral bc,
			C.opaqueMemoryBindInfoPBinds = pbs } f
