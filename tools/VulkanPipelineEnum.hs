{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanPipelineEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFileWithDefault2 "/usr/include/vulkan/vulkan_core.h" "Pipeline.Enum"
		["Data.Bits", "Data.Word"] [
	(	(UseEnum, Nothing), [],
		(	"BindPoint", "VkPipelineBindPoint",
			["Show", "Storable"] ) ),
	(	(UseEnum, Just "StageFlagsZero"), [("StageFlagsZero", Int 0)],
		(	"StageFlagBits", "VkPipelineStageFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	(NotUseEnum, Just "StageFlags2Zero"), [("StageFlags2Zero", Int 0)],
		(	"StageFlagBits2", "VkPipelineStageFlagBits2",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	(UseEnum, Just "CreateFlagsZero"), [("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkPipelineCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) )
	] [nowdoc|
type StageFlags = StageFlagBits
type StageFlags2 = StageFlagBits2
type CreateFlags = CreateFlagBits|]
