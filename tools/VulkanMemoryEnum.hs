{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanMemoryEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Memory.Enum" ["Data.Bits", "Data.Word"] [
	(	[("PropertyFlagsZero", Int 0)],
		(	"PropertyFlagBits", "VkMemoryPropertyFlagBits",
			["Show", "Eq", "Storable", "Bits", "FiniteBits"] ) ),
	(	[("HeapFlagsZero", Int 0)],
		(	"HeapFlagBits", "VkMemoryHeapFlagBits",
			["Show", "Eq", "Storable", "Bits", "FiniteBits"] ) ) ]
	[nowdoc|
type PropertyFlags = PropertyFlagBits
type HeapFlags = HeapFlagBits|]
