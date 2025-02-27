-- This file is automatically generated by the tools/makeEnum.hs
--	% stack runghc --cwd tools/ makeEnum

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-export-lists -fno-warn-tabs #-}

module Gpu.Vulkan.Attachment.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Bits
import Data.Word

#include <vulkan/vulkan.h>

enum "DescriptionFlagBits" ''#{type VkAttachmentDescriptionFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("DescriptionFlagsZero", 0),
	("DescriptionMayAliasBit",
		#{const VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT}),
	("DescriptionFlagBitsMaxEnum",
		#{const VK_ATTACHMENT_DESCRIPTION_FLAG_BITS_MAX_ENUM}) ]

enum "LoadOp" ''#{type VkAttachmentLoadOp}
		[''Show, ''Eq, ''Storable] [
	("LoadOpLoad", #{const VK_ATTACHMENT_LOAD_OP_LOAD}),
	("LoadOpClear", #{const VK_ATTACHMENT_LOAD_OP_CLEAR}),
	("LoadOpDontCare", #{const VK_ATTACHMENT_LOAD_OP_DONT_CARE}),
	("LoadOpNone", #{const VK_ATTACHMENT_LOAD_OP_NONE}),
	("LoadOpNoneExt", #{const VK_ATTACHMENT_LOAD_OP_NONE_EXT}),
	("LoadOpNoneKhr", #{const VK_ATTACHMENT_LOAD_OP_NONE_KHR}),
	("LoadOpMaxEnum", #{const VK_ATTACHMENT_LOAD_OP_MAX_ENUM}) ]

enum "StoreOp" ''#{type VkAttachmentStoreOp}
		[''Show, ''Eq, ''Storable] [
	("StoreOpStore", #{const VK_ATTACHMENT_STORE_OP_STORE}),
	("StoreOpDontCare",
		#{const VK_ATTACHMENT_STORE_OP_DONT_CARE}),
	("StoreOpNone", #{const VK_ATTACHMENT_STORE_OP_NONE}),
	("StoreOpNoneKhr", #{const VK_ATTACHMENT_STORE_OP_NONE_KHR}),
	("StoreOpNoneQcom",
		#{const VK_ATTACHMENT_STORE_OP_NONE_QCOM}),
	("StoreOpNoneExt", #{const VK_ATTACHMENT_STORE_OP_NONE_EXT}),
	("StoreOpMaxEnum", #{const VK_ATTACHMENT_STORE_OP_MAX_ENUM}) ]

type DescriptionFlags = DescriptionFlagBits
