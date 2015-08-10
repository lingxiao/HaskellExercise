{-# LANGUAGE GADTs #-}
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | Module : Javascript Type Attemp II
-- | Creator: Xiao Ling
-- | Created: December 3rd
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module JType2 (

	JType 		-- * Note the value constructors are not exported

) where 


import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
import Text.Show.Functions

import Core2

-- * Note this is tied to core, because ADT signature is expressed by Alpha
-- * and is stored inside and js ADT object

-- * Tonights goal: figure out how you want Alpha to interct with JType 
-- * chagne Alpha as needed 

-- * SOME compleetely new idea of storing signature in Alhpa form inside ADTs
-- * think about it some ... 
-- * infact, it should store some fucntion of Alpha ... 
-- * some little state machine maybe?

-- * Decide now: 
-- * all this stuff is going to work with algebraic datatype only
-- * so the other cases can just output Left Error

-- * look at system Fc for clues
-- * ADT should be some product of Alpha and Obj? 
-- * So ADT is actually some derived type, it shouldn't be the same tier as JObj

-- * The general problem is that each js datatype now has to carry some meta information
-- * wrt to the type system hovering in the background


-- * WHWER YOU LEFT OFF: 
-- * how to model adt in js object

-- * how to model Alpha, should kind be put inside atom?
-- * what is the advantage of putting kind inside atom?

-- * There's three places ot think about
-- * env, atom, Jtype-adt

-- * what should get stored where?

-- * rember, you're not making a new lang, but rather trying to write higher order
-- * functions over js functions

{-----------------------------------------------------------------------------
	I. Data Types
------------------------------------------------------------------------------}

-- * Javscript types are either js primitives or objects
data JType 
	= U ()
	| B Bool
	| D Double
	| S String 
	| O JObj
	deriving (Show)

-- * First-class js objects and some generic js object 
data JObj where 
	Lst :: [JType] -> JObj 				 		  -- * Array
	Fun :: ([JType] -> JType) -> JObj 			  -- * Function
	Adt :: Kind -> Alpha -> JType -> JObj         -- * Algebraic data type, this needs to be revised
	Obj :: DataName -> [(String,JType)]  -> JObj  -- * Some generic javascript object
		deriving (Show)

{-------------------------------------------------------------------------------
	II. Constructors and Deconstructors
--------------------------------------------------------------------------------}

-- * Javascript-specific primitive types * --
-- * These should go in JType

--[jundef, jbool, jnum, jstring] = (\n -> An (Atm (Right . Dn $ n) meta) meta []) <$> ["Undefined", "Boolean", "Number", "String"] :: [Alpha] 
	--where meta = (K 0,[],J 0)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          


-- * So right now there's some impedance where list and tuple are first class citizens here, but not in signature
-- * this is similar to how things will be once this is impl in js

create :: DataName -> [(String, JType)] -> JType
create n = O . Obj n

lst :: [JType] -> JType
lst = O . Lst

tu :: JType -> JType -> JType
tu a b = create (Dn "Tuple") [("fst", a), ("snd", b)]

-- * create some js function
toJf :: ([JType] -> JType) -> JType
toJf = O . Fun

-- * run a js function
runJf :: JType -> [JType] -> Either Error JType
runJf (O (Fun g)) ps = Right $ g ps
runJf _ _    	     = Left "Type Error: expected Javascript function, received type: "


{-----------------------------------------------------------------------------
	III. Readers
------------------------------------------------------------------------------}


{-
	on tap
	
	typeof :: JType -> Alpha ?? should it output Alpha ? 

-}




{-----------------------------------------------------------------------------
	V. Tests
------------------------------------------------------------------------------}

f = toJf $ \_ -> U () 













