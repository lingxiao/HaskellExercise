---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | Module : Test Mock Application that create typeclass in javascript
-- | Creator: Xiao Ling
-- | Created: October 20th
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module TcMockTest where

import Control.Monad
import Control.Monad.Reader
import Control.Applicative

import Data.Monoid
import Data.Either
import Data.Maybe
import Data.List

import Control.Monad.State
import Text.Show.Functions

import TcMock



{- 
	A js data type
-}

-- * maybe type constructor, note its value constructor is accessible
jMaybe :: JType
jMaybe = JObj [("TYPE", JStr "Maybe"), ("just", JFunc just_ ), ("nothing", nothing_)]

-- maybe value constructor
just_ :: JType -> JType
just_ x = JObj [("TYPE", JStr "Maybe"), ("v", x)]

nothing_ :: JType
nothing_ = just_ JUndef

ja, jb :: JType
ja = just_ . JNum $ 100
jb = just_ . JNum $ 12

k :: JType -> JType
k (JNum x) = JNum $ x + 100		

unJust :: JType -> JType
unJust m = head . rights . (:[]) $ m `oo` "v"


{-
	Make Monoid
-}

-- * Defined mempty and mappend over maybe sum doubles
-- * Note both nothing and mempty are identity elements which makes no sense
-- * But this is just a mock and does not actually have to make sense
mMempty :: JType
mMempty = just_ . JNum $ 0.0

mMappend :: JType -> JType -> JType
mMappend m1 m2  = case (m1 `oo` "v", m2 `oo` "v") of 
	(Right (JNum n1), Right (JNum n2)) -> just_ . JNum $ n1 + n2 
	(Right (JUndef) , _  	      )    -> m2
	(_             , Right (JUndef))   -> m1
	_ 							       -> error ""


mons :: InstanceStore
mons = (,) "Monoid" [(,) "Maybe" [("mempty", Left . Una $ \_ -> mMempty), ("mappend", bin) ]] where bin = Right . Bin $ mMappend


{-
	Test Monoid
-}

om  = binary "mappend" 
mem = prom "Monoid" "mempty" JUndef

-- * (<>) non-mempty elements and Nothing
jab, jan, jna :: TypeClassInterface
jab = ja       `om` jb
jan = ja       `om` nothing_
jna = nothing_ `om` ja


-- * test mapend promise
jmm, jam, jma :: TypeClassInterface
jam = ja  `om` mem
jma = mem `om` ja
jmm = mem `om` mem

--ms :: [JType]
ms = flip runReader mons <$> [jab,jan,jna,jam,jma]



{-
	Make Functor

-}

mfmap :: JType -> JType -> JType
mfmap m g = if m == nothing_ then nothing_ else just_ . runJF g . unJust $ m 

funs :: InstanceStore
funs = (,) "Functor" [(,) "Maybe" [("fmap", bin)]] where bin = Right . Bin $ mfmap

fun = binary "fmap"


{-
	test functor
-}

g :: JType
g = JFunc $ \(JArr [JNum x]) -> JNum $ x + x

---- * naked run
--temp = runCo (Co mfmap) ja g


fg, fn :: TypeClassInterface
fg = fun ja g
fn = fun nothing_ g
--fp = fun (pure_ . JNum $ 11) g

--fs :: [JType]
fs = flip runReader funs <$> [fg,fn]

{-


{-
	Make Applicative
-}

mpure :: JType -> JType
mpure = just_

-- (<**>) ::  f a -> f (a -> b) -> f b
mAp :: JType -> JType -> JType
mAp m g = if g == nothing_ || m == nothing_ then nothing_ else pure_ $ runJF (unJust g) $ unJust m

aps :: InstanceStore
aps = ("Applicative", [("Maybe", Unit mpure, Co mAp, NoJoin)])


{-
	Test Applicative
-}

jga, jgn, jng, jpg, jpn, jgp, jpp :: TypeClassInterface
jga = ja `co` jg
jgn = nothing_ `co` jg
jng = ja `co` nothing_

jpg = ja `co` pg
jpn = nothing_ `co` pg
jgp = (pure_ . JNum $ 100) `co` jg 
jpp = (pure_ . JNum $ 100) `co` pg -- * this one should throw error

--as :: [JType]
as = flip runReader aps <$> [jga,jgn,jng,jpg,jgp]

jg :: JType
jg = just_ . JFunc $ \(JArr [JNum x]) -> JNum $ x + 100

pg :: JType
pg = pure_ g 


{-
	Make Monad
-}

mu :: Maybe (Maybe a) -> Maybe a
mu (Just x) = x
mu Nothing   = Nothing


fff :: Maybe a -> (a -> Maybe b) -> Maybe b
fff = flip $ \g -> mu . fmap g


-- * eta :: Monad m => a -> m a
etam :: JType -> JType
etam = mpure

-- * mu :: Monad m => m (m a) -> m a
mum :: JType -> JType
mum m = if m == nothing_ then nothing_ else unJust m 


-- * this is the derived monad interface` (=<<) = mu . fmap g` for `Maybe' only
-- * note since mfmap = flip fmap, we don't need to flip the args
-- * flatMap :: Monad m => m a -> (a -> m b) -> m b
flatMapm :: JType -> JType -> JType 
flatMapm g = mum . mfmap g

eta_ :: JType -> JType
eta_ = prom "Monad"


{-
	test monad
-}

-- * output just value
mg :: JType
mg = JFunc $ \(JArr [JNum x]) -> just_ . JNum $ x + 1000

mn :: JType
mn = JFunc $ \_ -> nothing_

-- * output a promise
mpg :: JType
mpg = JFunc $ \(JArr [JNum x]) -> pure_ . JNum $ x + 200

monas :: InstanceStore
monas = ("Monad", [("Maybe", Unit etam, Co flatMapm, Jo mum)])


m1,m2,m3,m4,m5 :: TypeClassInterface
m1  = ja       `fun` mg
m2  = ja       `fun` mn
m3  = ja       `fun` mpg 
m4  = nothing_ `fun` mg 		
m5  = nothing_ `fun` mpg        

--mas :: [JType]
mas = flip runReader monas <$> [m1,m2,m3,m4,m5]


{-
	Generic typeclass functions

-}


-- Monoid

-- * generic mempt value, is equivalement to haskell `mempty`
mem :: JType
mem = prom "Monoid" JUndef

-- * generic typeclass binary function
mappend_ :: JType -> JType -> JType
mappend_ x y  = flip runReader mons $ x `co` y


-- Functor 

-- * generic fmap
fmap_ :: JType -> JType -> JType
fmap_ x = flip runReader funs . fun x


-- Applicative

-- * equivalent to `pure` function in haskell
pure_ :: JType -> JType
pure_ = prom "Applicative"

-- (<**>)
ap_ :: JType -> JType -> JType
ap_ x = flip runReader aps . co x


-- Monad
eta :: JType -> JType
eta = prom "Monad"

flatMap :: JType -> JType -> JType
flatMap x = flip runReader monas . fun x



-}


{-
	Test Scope access

	so it is here that the reference to the store is made??
	how do I explicitly keep track of the fact that the store is bein updated?

	ja <> jb ==> runJF (fromJust $ scope `oo` "mappend") (JArr [ja, jb]) 


scope = JObj [
	
	  ("TYPE"   , JStr "Object")
	, ("ap"     , JFunc $ \JArr [g,x] -> runReader (mon g x) aps )
	, ("mappend", JFunc $ \Jarr [x,y] -> runReader (mon x y) mons)

	]

-}



























