-- This file is automatically generated by the tools/makeEnum.hs
--	% stack runghc --cwd tools/ makeEnum

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-export-lists -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Bits
import Data.Word
import Data.Default

#include <vulkan/vulkan.h>

enum "MemoryBindFlagBits" ''#{type VkSparseMemoryBindFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("MemoryBindFlagsZero", 0),
	("MemoryBindMetadataBit",
		#{const VK_SPARSE_MEMORY_BIND_METADATA_BIT}),
	("MemoryBindFlagBitsMaxEnum",
		#{const VK_SPARSE_MEMORY_BIND_FLAG_BITS_MAX_ENUM}) ]

instance Default MemoryBindFlagBits where
	def = MemoryBindFlagsZero

type MemoryBindFlags = MemoryBindFlagBits
