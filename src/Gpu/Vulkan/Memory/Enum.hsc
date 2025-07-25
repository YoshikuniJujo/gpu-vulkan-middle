-- This file is automatically generated by the tools/makeEnum.hs
--	% stack runghc --cwd tools/ makeEnum

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-export-lists -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Bits
import Data.Word

#include <vulkan/vulkan.h>

enum "PropertyFlagBits" ''#{type VkMemoryPropertyFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits, ''FiniteBits] [
	("PropertyFlagsZero", 0),
	("PropertyDeviceLocalBit",
		#{const VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT}),
	("PropertyHostVisibleBit",
		#{const VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT}),
	("PropertyHostCoherentBit",
		#{const VK_MEMORY_PROPERTY_HOST_COHERENT_BIT}),
	("PropertyHostCachedBit",
		#{const VK_MEMORY_PROPERTY_HOST_CACHED_BIT}),
	("PropertyLazilyAllocatedBit",
		#{const VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT}),
	("PropertyProtectedBit",
		#{const VK_MEMORY_PROPERTY_PROTECTED_BIT}),
	("PropertyDeviceCoherentBitAmd",
		#{const VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD}),
	("PropertyDeviceUncachedBitAmd",
		#{const VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD}),
	("PropertyRdmaCapableBitNv",
		#{const VK_MEMORY_PROPERTY_RDMA_CAPABLE_BIT_NV}),
	("PropertyFlagBitsMaxEnum",
		#{const VK_MEMORY_PROPERTY_FLAG_BITS_MAX_ENUM}) ]

enum "HeapFlagBits" ''#{type VkMemoryHeapFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits, ''FiniteBits] [
	("HeapFlagsZero", 0),
	("HeapDeviceLocalBit", #{const VK_MEMORY_HEAP_DEVICE_LOCAL_BIT}),
	("HeapMultiInstanceBit",
		#{const VK_MEMORY_HEAP_MULTI_INSTANCE_BIT}),
	("HeapTileMemoryBitQcom",
		#{const VK_MEMORY_HEAP_TILE_MEMORY_BIT_QCOM}),
	("HeapMultiInstanceBitKhr",
		#{const VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR}),
	("HeapFlagBitsMaxEnum",
		#{const VK_MEMORY_HEAP_FLAG_BITS_MAX_ENUM}) ]

type PropertyFlags = PropertyFlagBits
type HeapFlags = HeapFlagBits
