{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue.Middle.Internal where

import Gpu.Vulkan.Queue.Core qualified as C

newtype Q = Q C.Q

instance Show Q
