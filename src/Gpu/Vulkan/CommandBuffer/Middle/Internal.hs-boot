{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Middle.Internal (
	C(..), SubmitInfo, submitInfoToCore
	) where

import Foreign.Storable.PeekPoke
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.IORef

import qualified Gpu.Vulkan.CommandBuffer.Core as C
import qualified Gpu.Vulkan.Pipeline.Core as Pipeline.C

data C = C {
	cPipeline :: IORef Pipeline.C.P,
	unC :: C.C }

type role SubmitInfo nominal
data SubmitInfo (s :: Maybe Type)

submitInfoToCore :: WithPoked (TMaybe.M mn) =>
	SubmitInfo mn -> (C.SubmitInfo -> IO r) -> IO ()
