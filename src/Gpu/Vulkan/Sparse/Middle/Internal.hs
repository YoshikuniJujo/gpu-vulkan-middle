{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Middle.Internal where

import Gpu.Vulkan.Device.Middle.Internal qualified as Device
import Gpu.Vulkan.Memory.Middle.Internal qualified as Memory
import Gpu.Vulkan.Sparse.Enum

import Gpu.Vulkan.Sparse.Core qualified as C

data MemoryBind = MemoryBind {
	memoryBindResourceOffset :: Device.Size,
	memoryBindSize :: Device.Size,
	memoryBindMemory :: Memory.M,
	memoryBindMemoryOffset :: Device.Size,
	memoryBindFlags :: MemoryBindFlags }

memoryBindToCore :: MemoryBind -> IO C.MemoryBind
memoryBindToCore MemoryBind {
	memoryBindResourceOffset = Device.Size ro,
	memoryBindSize = Device.Size sz,
	memoryBindMemory = mm,
	memoryBindMemoryOffset = Device.Size mo,
	memoryBindFlags = MemoryBindFlagBits fs } = do
	cmm <- Memory.mToCore mm
	pure C.MemoryBind {
		C.memoryBindResourceOffset = ro,
		C.memoryBindSize = sz,
		C.memoryBindMemory = cmm,
		C.memoryBindMemoryOffset = mo,
		C.memoryBindFlags = fs }
