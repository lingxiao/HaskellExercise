---------------------------------------------------------------------------------------------------
-- | Some useful proxies
---------------------------------------------------------------------------------------------------



module UtilsProxies (

	toConsole


) where 


import Control.Applicative
import Control.Monad
import Control.Proxy
import Control.Proxy.Trans.State
import Control.Proxy.Trans.Writer


import Test.QuickCheck
import Test.QuickCheck.All 
import Test.QuickCheck.Modifiers

import RoseTree 




{-

     Upstream | Downstream
       +-------------+
       |             |
  ()  <==           <== ()
       |  toConsole  |
  x   ==>           ==> ()
       |             |
       +-------------+

-}
toConsole :: (Show x, Proxy p) => () -> Consumer p x IO r 
toConsole () = runIdentityP . forever $ do
  x <- request ()
  lift . print $ "Log: " 
  lift . print $ x
  lift . print $ "******************"



