{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RoleAnnotations #-}

module Gpu.Vulkan.Buffer.Middle.Internal where

import Foreign.Storable.PeekPoke
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Gpu.Vulkan.Buffer.Core qualified as C

type role MemoryBarrier2 nominal
data MemoryBarrier2 (mn :: Maybe Type)

memoryBarrier2ToCore :: WithPoked (TMaybe.M mn) =>
	MemoryBarrier2 mn -> (C.MemoryBarrier2 -> IO a) -> IO ()
