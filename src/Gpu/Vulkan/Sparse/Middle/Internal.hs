{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Middle.Internal where

import Gpu.Vulkan.Device.Middle qualified as Device
import Gpu.Vulkan.Memory.Middle qualified as Memory
import Gpu.Vulkan.Sparse.Enum

data MemoryBind = MemoryBind {
	memoryBindResourceOffset :: Device.Size,
	memoryBindSize :: Device.Size,
	memoryBindMemory :: Memory.M,
	memoryBindMemoryOffset :: Device.Size,
	memoryBindFlags :: MemoryBindFlags }
