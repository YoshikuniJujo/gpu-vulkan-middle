{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Middle (

	-- * CREATE AND DESTROY

	create, null, destroy, R, CreateInfo(..),

	-- * BEGIN INFO

	BeginInfo(..) ) where

import Prelude hiding (null)
import Gpu.Vulkan.RenderPass.Middle.Internal
