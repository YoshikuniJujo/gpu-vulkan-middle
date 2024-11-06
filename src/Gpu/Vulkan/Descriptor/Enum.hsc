-- This file is automatically generated by the tools/makeEnum.hs
--	% stack runghc --cwd tools/ makeEnum

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-export-lists -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Bits
import Data.Word

#include <vulkan/vulkan.h>

enum "Type" ''#{type VkDescriptorType}
		[''Show, ''Eq, ''Storable] [
	("TypeSampler", #{const VK_DESCRIPTOR_TYPE_SAMPLER}),
	("TypeCombinedImageSampler",
		#{const VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER}),
	("TypeSampledImage",
		#{const VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE}),
	("TypeStorageImage",
		#{const VK_DESCRIPTOR_TYPE_STORAGE_IMAGE}),
	("TypeUniformTexelBuffer",
		#{const VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER}),
	("TypeStorageTexelBuffer",
		#{const VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER}),
	("TypeUniformBuffer",
		#{const VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER}),
	("TypeStorageBuffer",
		#{const VK_DESCRIPTOR_TYPE_STORAGE_BUFFER}),
	("TypeUniformBufferDynamic",
		#{const VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC}),
	("TypeStorageBufferDynamic",
		#{const VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC}),
	("TypeInputAttachment",
		#{const VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT}),
	("TypeInlineUniformBlock",
		#{const VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK}),
	("TypeAccelerationStructureKhr",
		#{const VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR}),
	("TypeAccelerationStructureNv",
		#{const VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV}),
	("TypeSampleWeightImageQcom",
		#{const VK_DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM}),
	("TypeBlockMatchImageQcom",
		#{const VK_DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM}),
	("TypeMutableExt", #{const VK_DESCRIPTOR_TYPE_MUTABLE_EXT}),
	("TypeInlineUniformBlockExt",
		#{const VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT}),
	("TypeMutableValve",
		#{const VK_DESCRIPTOR_TYPE_MUTABLE_VALVE}),
	("TypeMaxEnum", #{const VK_DESCRIPTOR_TYPE_MAX_ENUM}) ]

enum "BindingFlagBits" ''#{type VkDescriptorBindingFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("BindingUpdateAfterBindBit",
		#{const VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT}),
	("BindingUpdateUnusedWhilePendingBit",
		#{const VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT}),
	("BindingPartiallyBoundBit",
		#{const VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT}),
	("BindingVariableDescriptorCountBit",
		#{const VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT}),
	("BindingUpdateAfterBindBitExt",
		#{const VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT}),
	("BindingUpdateUnusedWhilePendingBitExt",
		#{const VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT}),
	("BindingPartiallyBoundBitExt",
		#{const VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT}),
	("BindingVariableDescriptorCountBitExt",
		#{const VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT}),
	("BindingFlagBitsMaxEnum",
		#{const VK_DESCRIPTOR_BINDING_FLAG_BITS_MAX_ENUM}) ]

type BindingFlags = BindingFlagBits
