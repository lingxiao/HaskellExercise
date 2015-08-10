{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-}  
{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | Module : Mock Application that create typeclass in javascript
-- | Creator: Xiao Ling
-- | Created: October 26th
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Core where 

import Control.Monad
import Control.Monad.Reader
import Control.Applicative

import Data.Dynamic
import Data.Monoid
import Data.Maybe
import Data.List

import Control.Monad.State
import Text.Show.Functions

-------------------------------
----- Primitive JS Type -------
-------------------------------

-- * Note even though the js object and function is created below, the application logic avoids using them
-- * since they are too unwiedly

data JType a where 
	U :: JType ()
	B :: Bool      -> JType Bool
	S :: String    -> JType String
	N :: Double    -> JType Double
	-- * Note lists can only be of one type
	L :: [JType a] -> JType [JType a]  

	-- * Note since the a in `KV` is existentialized, its field could not be pulled out!
	O :: Obj 	   -> JType Obj
	-- * This below looks redudant and look to get rid of it as soon as possible, but...
	-- * How would you store functions inside an object otherwise?
	-- * tenative decision, just don't do it
	F :: (JType a -> JType b) -> JType (JType a -> JType b)


-- * The fundamental unit of an object is a tuple mapping key to JType value forall type of a
data KV  = forall a. KV (String, JType a)
type Obj = [KV]

{-
	TypeClass implementatiosn
-}

instance Show KV where show (KV (a,b)) = a ++ ": " ++ show b

instance Show (JType a) where 
	show x = case x of 
		O o -> j $ foldr (\x y -> x ++ " " ++ y) [] $ show <$> o
		S s -> j s
		U   -> j . show $ ()
		N n -> j . show $ n
		B b -> j . show $ b 
		L l -> j . show $ l
		F f -> j . show $ f
		where j x = "[" ++ x ++ "]"

{-
	Primitive Operations
-}

unj :: JType a -> a 
unj x = case x of 
	U   -> ()
	S s -> s 
	N n -> n 
	B b -> b 
	L l -> l
	O o -> o
	F f -> f

-- * dynamic type reading
typeof :: JType a -> String
typeof x = case x of 
	U   -> "Undefined"
	S s -> "String"
	N n -> "Number"
	B b -> "Boolean"
	L l -> "List"
	O _ -> otype x
	F _ -> "Function"

-- * Note you cannot unwrap a value from KV since no function exist that works for all values of kv
-- * all values of kv. you can however pattern match on it and do oprations
otype :: JType Obj -> String
otype (O []) = "Object"
otype (O xs) = case xs !! 0 of 
	KV ("TYPE", S s) -> s
	_ 				 -> "Object"



