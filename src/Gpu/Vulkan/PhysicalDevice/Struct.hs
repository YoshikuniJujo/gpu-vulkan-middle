{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Struct where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke

import Data.Default
import System.IO.Unsafe

import Gpu.Vulkan.PhysicalDevice.Struct.Th
import qualified Gpu.Vulkan.PhysicalDevice.Core as C
import qualified Gpu.Vulkan.PhysicalDevice.Struct.Core as C

import Gpu.Vulkan.PhysicalDevice.Struct.ThTest

import Gpu.Vulkan.Enum
import Gpu.Vulkan.PNext.Middle.Internal

import Data.TypeLevel.Maybe qualified as TMaybe

vkPhysicalDeviceLimits
vkPhysicalDeviceFeatures

featuresZero :: Features
featuresZero = unsafePerformIO $ featuresFromCore <$> C.getClearedFeatures

instance Default Features where def = featuresZero

makeStructure "DescriptorIndexingFeatures"

instance Peek DescriptorIndexingFeaturesNoNext where
	peek' = (descriptorIndexingFeaturesFromCore <$>) . peek . castPtr

instance Typeable DescriptorIndexingFeaturesNoNext where
	structureType = StructureTypePhysicalDeviceDescriptorIndexingFeatures

instance Sizable DescriptorIndexingFeaturesNoNext where
	sizeOf' = sizeOf @C.DescriptorIndexingFeatures undefined
	alignment' = alignment @C.DescriptorIndexingFeatures undefined

instance WithPoked (TMaybe.M mn) => WithPoked (DescriptorIndexingFeatures mn) where
	withPoked' difs f = alloca \pcdifs -> do
		descriptorIndexingFeaturesToCore difs $ \cdifs -> poke pcdifs cdifs
		f . ptrS $ castPtr pcdifs

instance Nextable DescriptorIndexingFeatures where
	nextableSize = sizeOf @C.DescriptorIndexingFeatures undefined
	nextableType = StructureTypePhysicalDeviceDescriptorIndexingFeatures
	nextPtr p = C.descriptorIndexingFeaturesPNext <$> peek (castPtr p)
	createNextable p n =
		descriptorIndexingFeaturesFromNoNext n .
		descriptorIndexingFeaturesFromCore <$> peek (castPtr p)

descriptorIndexingFeaturesZero :: TMaybe.M mn -> DescriptorIndexingFeatures mn
descriptorIndexingFeaturesZero mn = unsafePerformIO
	$ descriptorIndexingFeaturesFromNoNext mn
	<$> descriptorIndexingFeaturesFromCore
	<$> C.getClearedDescriptorIndexingFeatures

makeStructure "Vulkan12Features"

instance Peek Vulkan12FeaturesNoNext where
	peek' = (vulkan12FeaturesFromCore <$>) . peek . castPtr

instance Typeable Vulkan12FeaturesNoNext where
	structureType = StructureTypePhysicalDeviceVulkan12Features

instance Sizable Vulkan12FeaturesNoNext where
	sizeOf' = sizeOf @C.Vulkan12Features undefined
	alignment' = alignment @C.Vulkan12Features undefined

instance WithPoked (TMaybe.M mn) => WithPoked (Vulkan12Features mn) where
	withPoked' v12fs f = alloca \pv12fs -> do
		vulkan12FeaturesToCore v12fs $ \cv12fs -> poke pv12fs cv12fs
		f . ptrS $ castPtr pv12fs

instance Nextable Vulkan12Features where
	nextableSize = sizeOf @C.Vulkan12Features undefined
	nextableType = StructureTypePhysicalDeviceVulkan12Features
	nextPtr p = C.vulkan12FeaturesPNext <$> peek (castPtr p)
	createNextable p n =
		vulkan12FeaturesFromNoNext n .
		vulkan12FeaturesFromCore <$> peek (castPtr p)

vulkan12FeaturesZero :: TMaybe.M mn -> Vulkan12Features mn
vulkan12FeaturesZero mn = unsafePerformIO $ vulkan12FeaturesFromNoNext mn
	<$> vulkan12FeaturesFromCore <$> C.getClearedVulkan12Features

makeStructure "Vulkan13Features"

instance Peek Vulkan13FeaturesNoNext where
	peek' = (vulkan13FeaturesFromCore <$>) . peek . castPtr

instance Typeable Vulkan13FeaturesNoNext where
	structureType = StructureTypePhysicalDeviceVulkan13Features

instance Sizable Vulkan13FeaturesNoNext where
	sizeOf' = sizeOf @C.Vulkan13Features undefined
	alignment' = alignment @C.Vulkan13Features undefined

instance WithPoked (TMaybe.M mn) => WithPoked (Vulkan13Features mn) where
	withPoked' v13fs f = alloca \pv13fs -> do
		vulkan13FeaturesToCore v13fs $ \cv13fs -> poke pv13fs cv13fs
		f . ptrS $ castPtr pv13fs

instance Nextable Vulkan13Features where
	nextableSize = sizeOf @C.Vulkan13Features undefined
	nextableType = StructureTypePhysicalDeviceVulkan13Features
	nextPtr p = C.vulkan13FeaturesPNext <$> peek (castPtr p)
	createNextable p n =
		vulkan13FeaturesFromNoNext n .
		vulkan13FeaturesFromCore <$> peek (castPtr p)

vulkan13FeaturesZero :: TMaybe.M mn -> Vulkan13Features mn
vulkan13FeaturesZero mn = unsafePerformIO $ vulkan13FeaturesFromNoNext mn
	<$> vulkan13FeaturesFromCore <$> C.getClearedVulkan13Features
