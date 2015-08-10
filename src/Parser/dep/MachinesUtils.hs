---------------------------------------------------------------------------------------------------
-- | Some Utility Processes 
---------------------------------------------------------------------------------------------------



module MachinesUtils (

    traceExec
  , traceExec'


) where

import Control.Monad.State
import Data.Machine
import Data.Maybe

import Test.QuickCheck
import Test.QuickCheck.All 
import Test.QuickCheck.Modifiers

import RoseTree 


-- | Maps a past state to a list of possible current state in a nondeterministic computation
type History a = [(a,[a])]


-- | @Trace the execution of a process and build tree

-- | This is based on a very kludgy hack of a rosetree module
toRoseTree :: (Eq a, Ord a) => ProcessT (State (RoseTree a)) [(a,[a])] ()
toRoseTree = repeatedly $ do
  ns <- await
  t  <- get
  let t' = ns `graftAll` t in case t' of 
    Nothing -> stop
    _       -> put . fromJust $ t'

traceExec :: Ord a => [History a] -> RoseTree a
traceExec ins = flip execState Bud . runT $ source ins ~> toRoseTree

traceExec' ins seed = flip execState seed . runT $ source ins ~> toRoseTree








