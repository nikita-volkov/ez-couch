{- 
    This is a fork of the monad-parallel package, part of the SCC project by Mario Blazevic.
    It uses the MonadTransControl class to avoid writing boilerplate instances. It also adds support
    for ResourceT.
    SCC is published under the GNU General Public License version 3.
-}

-- | This module defines classes of monads that can perform multiple computations in parallel and, more importantly,
-- combine the results of those parallel computations.
-- 
-- There are two classes exported by this module, 'Parallel' and 'Fork'. The former is more generic, but the
-- latter is easier to use: when invoking any expensive computation that could be performed in parallel, simply wrap the
-- call in 'forkExec'. The function immediately returns a handle to the running computation. The handle can be used to
-- obtain the result of the computation when needed:
--
-- @
--   do child <- forkExec expensive
--      otherStuff
--      result <- child
-- @
--
-- In this example, the computations /expensive/ and /otherStuff/ would be performed in parallel. When using the
-- 'Parallel' class, both parallel computations must be specified at once:
--
-- @
--   bindM2 (\\ childResult otherResult -> ...) expensive otherStuff
-- @
--
-- In either case, for best results the costs of the two computations should be roughly equal.
--
-- Any monad that is an instance of the 'Fork' class can also be instance of the 'Parallel' class
-- by the following rule:
-- 
-- @ bindM2 f ma mb = do {a' <- forkExec ma; b <- mb; a <- a'; f a b} @ 
--
-- When operating with monads free of side-effects, such as 'Identity' or 'Maybe', 'forkExec' is equivalent to 'return'
-- and 'bindM2' is equivalent to @ \\ f ma mb -> do {a <- ma; b <- mb; f a b} @ &#x2014; the only difference is in the
-- resource utilisation. With the 'IO' monad, on the other hand, there may be visible difference in the results because
-- the side effects of /ma/ and /mb/ may be arbitrarily reordered.

{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module Control.Monad.Parallel
   (
    -- * Classes
    Parallel(..), Fork(..),
    bindM3,
    -- * Control.Monad equivalents
    liftM2, liftM3, ap, sequence, sequence_, mapM, replicateM, replicateM_,
    -- * Default instances
    defaultForkExec, defaultBindM2, bindTrans, forkTrans, parallelIO
   )
where

import Prelude ()
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, readMVar)
import Control.Exception (SomeException, throwIO, mask, try)
import Control.Monad (Monad, (>>=), (>>), return, liftM)
import qualified Control.Monad as M
import Control.Monad.Trans.Identity (IdentityT(IdentityT, runIdentityT))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans.Error (ErrorT(ErrorT, runErrorT), Error)
import Control.Monad.Trans.List (ListT(ListT, runListT))
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT))
import Control.Parallel (par, pseq)
import Data.Either (Either(..), either)
import Data.Function (($), (.), const)
import Data.Functor.Identity (Identity)
import Data.Int (Int)
import Data.List ((++), foldr, map, replicate)
import Data.Maybe (Maybe(Just, Nothing))
import System.IO (IO)
import Control.Monad.Trans.Resource
import qualified Control.Exception.Lifted as L
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

-- | Class of types that can perform two computations in parallel and bind their results together.
class Monad m => Parallel m where
   -- | Perform two monadic computations in parallel; when they are both finished, pass the results to the function.
   -- Apart from the possible ordering of side effects, this function is equivalent to
   -- @\\f ma mb-> do {a <- ma; b <- mb; f a b}@
   bindM2 :: (a -> b -> m c) -> m a -> m b -> m c

-- | Class of monads that can fork a parallel computation.
class Monad m => Fork m where
  -- | Fork a child monadic computation to be performed in parallel with the current one.
  forkExec :: m a -> m (m a)


defaultBindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
defaultBindM2 f ma mb = let ma' = ma >>= return
                            mb' = mb >>= return
                        in ma' `par` (mb' `pseq` do {a <- ma'; b <- mb'; f a b})

defaultForkExec :: Monad m => m a -> (m (m a))
defaultForkExec e = let result = e >>= return
                    in result `par` (return result)

-- | Perform three monadic computations in parallel; when they are all finished, pass their results to the function.
bindM3 :: Parallel m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bindM3 f ma mb mc = bindM2 (\f' c-> f' c) (liftM2 f ma mb) mc

-- | Like 'Control.Monad.liftM2', but evaluating its two monadic arguments in parallel.
liftM2 :: Parallel m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = bindM2 (\a b-> return (f a b)) m1 m2

-- | Like 'Control.Monad.liftM3', but evaluating its three monadic arguments in parallel.
liftM3  :: Parallel m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 f m1 m2 m3 = bindM3 (\a b c-> return (f a b c)) m1 m2 m3

-- | Like 'Control.Monad.ap', but evaluating the function and its argument in parallel.
ap :: Parallel m => m (a -> b) -> m a -> m b
ap mf ma = bindM2 (\f a-> return (f a)) mf ma

-- | Like 'Control.Monad.sequence', but executing the actions in parallel.
sequence :: Parallel m =>[m a] -> m [a]
sequence ms = foldr k (return []) ms where
   k m m' = liftM2 (:) m m'

-- | Like 'Control.Monad.sequence_', but executing the actions in parallel.
sequence_ :: Fork m => [m a] -> m () 
sequence_ = M.mapM_ (forkExec . (>> return ()))

-- | Like 'Control.Monad.mapM', but applying the function to the individual list items in parallel.
mapM :: Parallel m => (a -> m b) -> [a] -> m [b]
mapM f list = sequence (map f list)

-- | Like 'Control.Monad.replicateM', but executing the action multiple times in parallel.
replicateM :: Parallel m => Int -> m a -> m [a]
replicateM n action = sequence (replicate n action)

-- | Like 'Control.Monad.replicateM_', but executing the action multiple times in parallel.
replicateM_ :: Fork m => Int -> m a -> m ()
replicateM_ n action = sequence_ (replicate n action)

-- | Any monad that allows the result value to be extracted, such as `Identity` or `Maybe` monad, can implement
-- `bindM2` by using `par`.
instance Parallel Identity where bindM2 = defaultBindM2
instance Parallel Maybe where bindM2 = defaultBindM2
instance Parallel [] where bindM2 = defaultBindM2

instance Parallel ((->) r) where
   bindM2 f ma mb r = let a = ma r
                          b = mb r
                      in a `par` (b `pseq` f a b r)

-- | Defines `bindM2` in terms of `forkExec`
parallelIO :: Fork m => (a -> b -> m c) -> m a -> m b -> m c
parallelIO f ma mb = do waitForB <- forkExec mb
                        a <- ma
                        b <- waitForB
                        f a b

instance Parallel IO where
  bindM2 = parallelIO

instance Parallel (ResourceT IO) where
  bindM2 = parallelIO

-- | Common bind pattern for monad transformers
bindTrans :: (MonadTransControl t, Parallel m, Monad m, Monad (t m)) => (a -> b -> t m c) -> t m a -> t m b -> t m c
bindTrans f tma tmb = liftWith (\run -> bindM2 (\a b-> run $  f' a b) (run tma) (run tmb)) >>= restoreT . return where
  f' a b = do
    a' <- restoreT $ return a
    b' <- restoreT $ return b
    f a' b'

instance Parallel m => Parallel (IdentityT m) where bindM2 = bindTrans
instance Parallel m => Parallel (MaybeT m) where bindM2 = bindTrans
instance (Parallel m, Error e) => Parallel (ErrorT e m) where bindM2 = bindTrans
instance Parallel m => Parallel (ListT m) where bindM2 = bindTrans
instance Parallel m => Parallel (ReaderT r m) where bindM2 = bindTrans

instance Fork Maybe where forkExec = defaultForkExec
instance Fork [] where forkExec = defaultForkExec

instance Fork ((->) r) where
   forkExec e = \r-> let result = e r
                     in result `par` (return result)

instance Fork IO where
   forkExec ma = do
      v <- newEmptyMVar
      _ <- mask $ \restore -> forkIO $ try (restore ma) >>= putMVar v
      return $ readMVar v >>= either (\e -> throwIO (e :: SomeException)) return

instance Fork (ResourceT IO) where
   forkExec ma = do
      v <- liftIO newEmptyMVar
      _ <- L.mask $ \restore -> resourceForkIO $ L.try (restore ma) >>= (liftIO . putMVar v)
      return $ liftIO (readMVar v) >>= either (\e -> L.throwIO (e :: SomeException)) return


forkTrans :: (MonadTransControl t, Fork m) => t m a -> t m (t m a)
forkTrans tma = liftWith $ \run -> liftM restoreT $ forkExec (run tma)
