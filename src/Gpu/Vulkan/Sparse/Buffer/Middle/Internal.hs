{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Buffer.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke
import Control.Arrow

import Gpu.Vulkan.Buffer.Middle.Internal qualified as Buffer
import Gpu.Vulkan.Sparse.Middle.Internal

import Gpu.Vulkan.Sparse.Buffer.Core qualified as C

data MemoryBindInfo = MemoryBindInfo {
	memoryBindInfoBuffer :: Buffer.B,
	memoryBindInfoBinds :: [MemoryBind] }

memoryBindInfoToCore :: MemoryBindInfo -> (Ptr C.MemoryBindInfo -> IO a) -> IO a
memoryBindInfoToCore MemoryBindInfo {
	memoryBindInfoBuffer = Buffer.B bffr,
	memoryBindInfoBinds = length &&& id -> (bndc, bnds) } f = do
	cbnds <- memoryBindToCore `mapM` bnds
	allocaArray bndc \pbnds -> do
		pokeArray pbnds cbnds
		withPoked C.MemoryBindInfo {
			C.memoryBindInfoBuffer = bffr,
			C.memoryBindInfoBindCount = fromIntegral bndc,
			C.memoryBindInfoPBinds = pbnds } f
