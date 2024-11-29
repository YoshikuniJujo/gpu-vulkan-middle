{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanSparseEnum where

import MakeEnum

make :: IO ()
make = createFileWithDefault vulkanCore "Sparse.Enum" ["Data.Bits", "Data.Word"]
	[(	Just "MemoryBindFlagsZero", [("MemoryBindFlagsZero", Int 0)],
		("MemoryBindFlagBits", "VkSparseMemoryBindFlagBits", ["Show", "Eq", "Storable", "Bits"]) )]
	"type MemoryBindFlags = MemoryBindFlagBits"
