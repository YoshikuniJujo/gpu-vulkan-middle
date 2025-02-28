{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Rendering.Middle (
	Info(..),
	AttachmentInfo(..), attachmentInfoZero, AttachmentInfoToCore ) where

import Gpu.Vulkan.Rendering.Middle.Internal
