{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.GroupDevice.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Arrow
import Data.TypeLevel.Maybe qualified as TMaybe

import Gpu.Vulkan.PhysicalDevice.Middle.Internal qualified as PhysicalDevice
import Gpu.Vulkan.Device.GroupDevice.Core qualified as C

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoPhysicalDevices :: [PhysicalDevice.P] }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

instance WithPoked (TMaybe.M mn) => WithPoked (CreateInfo mn) where
	withPoked' ci f = alloca \pci -> do
		createInfoToCore ci $ \cci -> poke pci cci
		f . ptrS $ castPtr pci

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoPhysicalDevices = (id &&& length) -> (pds, pdc)
	} f =
	putStrLn "createInfoToCore begin" >>
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	alloca \pci -> allocaArray pdc \ppds -> do
		pokeArray ppds (phdToCore <$> pds)
		poke pci C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoPhysicalDeviceCount = fromIntegral pdc,
			C.createInfoPPhysicalDevices = ppds }
		() <$ f pci
	where
	phdToCore (PhysicalDevice.P p) = p
