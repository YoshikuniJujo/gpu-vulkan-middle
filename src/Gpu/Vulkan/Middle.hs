{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Middle (

	-- * INFO

	-- ** ApplicationInfo

	ApplicationInfo(..),
	ApiVersion,
	makeApiVersion, fromApiVersion, Variant, Major, Minor, Patch,
	apiVersion_1_0, apiVersion_1_1, apiVersion_1_2, apiVersion_1_3,

	-- ** SubmitInfo

	SubmitInfo(..), SubmitInfoListToCore,
	SubmitInfo2(..), SubmitInfo2ListToCore,

	-- * PROPERTIES

	LayerProperties(..), ExtensionProperties(..), FormatProperties(..),

	-- * PIPELINE VALUES

	-- ** ViewPort

	Viewport, pattern Viewport,
	viewportX, viewportY, viewportWidth, viewportHeight,
	viewportMinDepth, viewportMaxDepth,

	-- ** StencilOpState

	StencilOpState(..),

	-- ** ClearValue

	ClearValue(..),
	ClearValueListToCore, ClearValueToCore,

	-- *** ClearType

	ClearType(..), ClearColorType(..),

	-- *** ClearColorValue

	ClearColorValueToCore,

	-- *** ClearDepthStencilValue

	ClearDepthStencilValue, pattern ClearDepthStencilValue,
	clearDepthStencilValueDepth, clearDepthStencilValueStencil,

	-- * RECT, OFFSET AND EXTENT

	-- ** Rect

	Rect2d, pattern Rect2d, rect2dExtent, rect2dOffset,

	-- ** Offset

	Offset2d, pattern Offset2d, offset2dX, offset2dY,
	Offset3d, pattern Offset3d, offset3dX, offset3dY, offset3dZ,

	-- ** Extent

	Extent2d, pattern Extent2d, extent2dWidth, extent2dHeight,
	Extent3d, pattern Extent3d, extent3dWidth, extent3dHeight, extent3dDepth,

	-- * OTHERS

	Size(..),

	DependencyInfo(..), BlitImageInfo2(..),

	remainingMipLevels, remainingArrayLayers

	) where

import Gpu.Vulkan.Middle.Internal
