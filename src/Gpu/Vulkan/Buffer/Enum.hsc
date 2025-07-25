-- This file is automatically generated by the tools/makeEnum.hs
--	% stack runghc --cwd tools/ makeEnum

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-export-lists -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Default
import Data.Bits
import Data.Word
import Data.Default

#include <vulkan/vulkan.h>

enum "CreateFlagBits" ''#{type VkBufferCreateFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("CreateFlagsZero", 0),
	("CreateSparseBindingBit",
		#{const VK_BUFFER_CREATE_SPARSE_BINDING_BIT}),
	("CreateSparseResidencyBit",
		#{const VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT}),
	("CreateSparseAliasedBit",
		#{const VK_BUFFER_CREATE_SPARSE_ALIASED_BIT}),
	("CreateProtectedBit", #{const VK_BUFFER_CREATE_PROTECTED_BIT}),
	("CreateDeviceAddressCaptureReplayBit",
		#{const VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT}),
	("CreateDescriptorBufferCaptureReplayBitExt",
		#{const VK_BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT}),
	("CreateVideoProfileIndependentBitKhr",
		#{const VK_BUFFER_CREATE_VIDEO_PROFILE_INDEPENDENT_BIT_KHR}),
	("CreateDeviceAddressCaptureReplayBitExt",
		#{const VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT}),
	("CreateDeviceAddressCaptureReplayBitKhr",
		#{const VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR}),
	("CreateFlagBitsMaxEnum",
		#{const VK_BUFFER_CREATE_FLAG_BITS_MAX_ENUM}) ]

instance Default CreateFlagBits where
	def = CreateFlagsZero

enum "UsageFlagBits" ''#{type VkBufferUsageFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("UsageFlagsZero", 0),
	("UsageTransferSrcBit",
		#{const VK_BUFFER_USAGE_TRANSFER_SRC_BIT}),
	("UsageTransferDstBit",
		#{const VK_BUFFER_USAGE_TRANSFER_DST_BIT}),
	("UsageUniformTexelBufferBit",
		#{const VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT}),
	("UsageStorageTexelBufferBit",
		#{const VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT}),
	("UsageUniformBufferBit",
		#{const VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT}),
	("UsageStorageBufferBit",
		#{const VK_BUFFER_USAGE_STORAGE_BUFFER_BIT}),
	("UsageIndexBufferBit",
		#{const VK_BUFFER_USAGE_INDEX_BUFFER_BIT}),
	("UsageVertexBufferBit",
		#{const VK_BUFFER_USAGE_VERTEX_BUFFER_BIT}),
	("UsageIndirectBufferBit",
		#{const VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT}),
	("UsageShaderDeviceAddressBit",
		#{const VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT}),
	("UsageVideoDecodeSrcBitKhr",
		#{const VK_BUFFER_USAGE_VIDEO_DECODE_SRC_BIT_KHR}),
	("UsageVideoDecodeDstBitKhr",
		#{const VK_BUFFER_USAGE_VIDEO_DECODE_DST_BIT_KHR}),
	("UsageTransformFeedbackBufferBitExt",
		#{const VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT}),
	("UsageTransformFeedbackCounterBufferBitExt",
		#{const VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT}),
	("UsageConditionalRenderingBitExt",
		#{const VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT}),
	("UsageAccelerationStructureBuildInputReadOnlyBitKhr",
		#{const VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR}),
	("UsageAccelerationStructureStorageBitKhr",
		#{const VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR}),
	("UsageShaderBindingTableBitKhr",
		#{const VK_BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR}),
	("UsageVideoEncodeDstBitKhr",
		#{const VK_BUFFER_USAGE_VIDEO_ENCODE_DST_BIT_KHR}),
	("UsageVideoEncodeSrcBitKhr",
		#{const VK_BUFFER_USAGE_VIDEO_ENCODE_SRC_BIT_KHR}),
	("UsageSamplerDescriptorBufferBitExt",
		#{const VK_BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT}),
	("UsageResourceDescriptorBufferBitExt",
		#{const VK_BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT}),
	("UsagePushDescriptorsDescriptorBufferBitExt",
		#{const VK_BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT}),
	("UsageMicromapBuildInputReadOnlyBitExt",
		#{const VK_BUFFER_USAGE_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT}),
	("UsageMicromapStorageBitExt",
		#{const VK_BUFFER_USAGE_MICROMAP_STORAGE_BIT_EXT}),
	("UsageTileMemoryQcom",
		#{const VK_BUFFER_USAGE_TILE_MEMORY_QCOM}),
	("UsageRayTracingBitNv",
		#{const VK_BUFFER_USAGE_RAY_TRACING_BIT_NV}),
	("UsageShaderDeviceAddressBitExt",
		#{const VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT}),
	("UsageShaderDeviceAddressBitKhr",
		#{const VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR}),
	("UsageFlagBitsMaxEnum",
		#{const VK_BUFFER_USAGE_FLAG_BITS_MAX_ENUM}) ]

instance Default UsageFlagBits where
	def = UsageFlagsZero

type CreateFlags = CreateFlagBits
type UsageFlags = UsageFlagBits
