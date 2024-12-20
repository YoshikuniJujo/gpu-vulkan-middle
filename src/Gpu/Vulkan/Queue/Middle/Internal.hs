{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue.Middle.Internal (

	-- * SUBMIT AND WAIT IDLE

	submit, waitIdle, Q(..),

	-- * SPARSE RESOURCES

	bindSparse, BindSparseInfo(..), bindSparseInfoToCore

	) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke
import Control.Arrow
import Control.Monad.Cont.MiscYj
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.Base.Middle
import Gpu.Vulkan.Middle.Internal
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Fence.Middle.Internal qualified as Fence.M
import Gpu.Vulkan.Queue.Core qualified as C

import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.HeteroParList qualified as HPList
import Gpu.Vulkan.Semaphore.Middle.Internal qualified as Semaphore
import Gpu.Vulkan.Sparse.Buffer.Middle.Internal qualified as Sparse.Buffer
import Gpu.Vulkan.Sparse.Image.Middle.Internal qualified as Sparse.Image
import Gpu.Vulkan.Fence.Middle.Internal qualified as Fence

import Control.Monad
import Debug

newtype Q = Q C.Q deriving Show

submit :: SubmitInfoListToCore ns =>
	Q -> HeteroParList.PL SubmitInfo ns -> Maybe Fence.M.F -> IO ()
submit (Q q) sis mf = submitInfoListToCore sis \csis ->
	let sic = length csis in allocaArray sic \psis -> do
		pokeArray psis csis
		r <- C.submit q (fromIntegral sic) psis
			$ Fence.M.maybeFToCore mf
		throwUnlessSuccess $ Result r

waitIdle :: Q -> IO ()
waitIdle (Q q) = throwUnlessSuccess . Result =<< C.waitIdle q

bindSparse :: HPList.ToListWithCCpsM' WithPoked TMaybe.M mns =>
	Q -> HPList.PL BindSparseInfo mns -> Maybe Fence.F -> IO ()
bindSparse (Q q) is mf =
	when debug (putStrLn
		"Gpu.Vulkan.Queue.Middle.bindSparse begin") >>
	HPList.withListWithCCpsM' @_ @WithPoked @TMaybe.M is bindSparseInfoToCore \cis ->
	let cic = length cis in
	allocaArray cic \pcis ->
	pokeArray pcis cis >>
	let cf = case mf of
		Just (Fence.F f) -> f
		Nothing -> NullHandle in
	when debug (putStrLn
		"Gpu.Vulkan.Queue.Middle.bindSparse: before C.bindSparse") >>
	(throwUnlessSuccess . Result =<< C.bindSparse q (fromIntegral cic) pcis cf)

data BindSparseInfo (mn :: Maybe Type) = BindSparseInfo {
	bindSparseInfoNext :: TMaybe.M mn,
	bindSparseInfoWaitSemaphores :: [Semaphore.S],
	bindSparseInfoBufferBinds :: [Sparse.Buffer.MemoryBindInfo],
	bindSparseInfoImageOpaqueBinds :: [Sparse.Image.OpaqueMemoryBindInfo],
	bindSparseInfoImageBinds :: [Sparse.Image.MemoryBindInfo],
	bindSparseInfoSignalSemaphores :: [Semaphore.S] }

bindSparseInfoToCore :: WithPoked (TMaybe.M mn) =>
	BindSparseInfo mn -> (C.BindSparseInfo -> IO a) -> IO ()
bindSparseInfoToCore BindSparseInfo {
	bindSparseInfoNext = mnxt,
	bindSparseInfoWaitSemaphores =
		length &&& (Semaphore.unS <$>) -> (wsc, wss),
	bindSparseInfoBufferBinds = length &&& id -> (bbc, bbs),
	bindSparseInfoImageOpaqueBinds = length &&& id -> (iobc, iobs),
	bindSparseInfoImageBinds = length &&& id -> (ibc, ibs),
	bindSparseInfoSignalSemaphores =
		length &&& (Semaphore.unS <$>) -> (ssc, sss)
	} f = withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
		allocaArray wsc \pwss ->
		pokeArray pwss wss >>
		allocaArray bbc \pbbs ->
		(Sparse.Buffer.memoryBindInfoToCore `mapContM` bbs) \cbbs ->
		pokeArray pbbs cbbs >>
		allocaArray iobc \piobs ->
		(Sparse.Image.opaqueMemoryBindInfoToCore
			`mapContM` iobs) \ciobs ->
		pokeArray piobs ciobs >>
		allocaArray ibc \pibs ->
		(Sparse.Image.memoryBindInfoToCore `mapContM` ibs) \cibs ->
		pokeArray pibs cibs >>
		allocaArray ssc \psss ->
		pokeArray psss sss >>
		f C.BindSparseInfo {
			C.bindSparseInfoSType = (),
			C.bindSparseInfoPNext = pnxt',
			C.bindSparseInfoWaitSemaphoreCount = fromIntegral wsc,
			C.bindSparseInfoPWaitSemaphores = pwss,
			C.bindSparseInfoBufferBindCount = fromIntegral bbc,
			C.bindSparseInfoPBufferBinds = pbbs,
			C.bindSparseInfoImageOpaqueBindCount =
				fromIntegral iobc,
			C.bindSparseInfoPImageOpaqueBinds = piobs,
			C.bindSparseInfoImageBindCount = fromIntegral ibc,
			C.bindSparseInfoPImageBinds = pibs,
			C.bindSparseInfoSignalSemaphoreCount = fromIntegral ssc,
			C.bindSparseInfoPSignalSemaphores = psss }
