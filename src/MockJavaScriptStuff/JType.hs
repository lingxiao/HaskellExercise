---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | Module : Javascript type
-- | Creator: Xiao Ling
-- | Created: November 30th
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module JType (

	  JType (..) 

	-- * primitive functions over JType

	, runJf
	, create
	, tu

	, typeOf 
	, fstJ
	, sndJ
	, atJ
	, dot
	, t,v

	-- * functions that service the Core

	, satType
	, satConsist

) where 


import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
import Text.Show.Functions

import Core

{-----------------------------------------------------------------------------
	I. Data Types
------------------------------------------------------------------------------}

data JType 
	= U ()
	| B Bool
	| D Double
	| S String 
	| L [JType]
	| O [(String, JType)]
	| F ([JType] -> JType)
	deriving (Show)

-- * should also store datatype of its fields
-- * Maybe a  ==> Ja (K 1) [Ad "Maybe" [] [Po $ Sym 'a' (J 0) []]] (U ())

-- * should you tie this representation to parsing?
-- * what if each adt carries some metatype, ie its type signature?
-- * except are you storing too many things in here????
-- * really you want nested ADTs? should you add a special field just for JAdt?
-- * because you do want typeclasses to work for more than just ADT right?
-- * things in haskell that work on non adts: equal, Ord, Num, Enum, Show

-- * Two decision: internal representation of JAdt
-- * Shoud Adt be part of JType ?? 
-- * what if we did a more faithful modeling:
{-
	data JObj where 
		L :: [JType] -> JObj
		T :: JType -> JType -> JObj 
		F :: ([JType] -> JType) -> JObj
		A :: Kind -> [Alpha] -> JType -> JObj

	data JType = U () | B Bool | D Double | S String | O JObj

-}

data JAdt = Ja Kind [Alpha] JType  

{-----------------------------------------------------------------------------
	II. Constructor and Deconstructor
------------------------------------------------------------------------------}

-- * common object fields
t, v :: String
t  = "Type"
v = "v"
-- * unwrap string type
unS :: JType -> Maybe String
unS (S s) = Just s 
unS _  	  = Nothing

-- * run a js function
runJf :: JType -> [JType] -> Maybe JType
runJf (F g) ps = return $ g ps 
runJf _     _  = Nothing

-- * object constuctor
create :: DataType -> [(String,JType)] -> JType
create d = O . (:) (t, S d) 

-- * tuple constructor
tu :: JType -> JType -> JType
tu a b = create "Tuple" [("fst", a),("snd", b)]


{-----------------------------------------------------------------------------
	III. Accessors
------------------------------------------------------------------------------}

-- * object field accessor
dot :: JType -> String -> Either Error JType
dot x n = case x of 
	O os -> flip listToEither [ v | (n',v) <- os, n' == n ] $ "Error: Object does not have field " ++ n
	_    -> Left "Error: Not an Object"

-- * tuple accessor
fstJ, sndJ :: JType -> Maybe JType
fstJ x = either (const Nothing) Just $ x `dot` "fst"
sndJ x = either (const Nothing) Just $ x `dot` "snd"

-- * catamorphism of J object
-- * Fold over some Javascript ADT
foldJ :: (JType -> b -> b) -> b -> JType -> b
foldJ g b x = case x of 
	O os -> if os == [] then b else either (const b) (\x' -> foldJ g (g x' b) x') $ x `dot` v 
	L os -> if os == [] then b else let x' = head os in foldJ g (g x' b) x'
	_    -> b 

-- * what about paramterized adt where p > 1 ?
-- * traverse some JType object `x` that represents an ADT to specified depth `j`
-- * This fails for empty "containers", does this make sense?
-- * It means mempty value by definition cannot be type checked?!!!
-- * this is going to be a problem when you typecheck mempty values in 
-- * Strategy
-- * it make more sense to represent before hand, what the

-- * HERE, we encounter a structural issue of how to solve undefined parameterzied adts

atJ :: JType -> J -> Maybe JType
atJ x j = maybe Nothing (\mj -> go j mj x) $ maxJ x where 
	
	go j mj x = if j > mj then Nothing else if j == mj then Just x else case x of 
		L os -> if os == [] then Nothing else go j (pred mj) $ head os
		O os -> either (const Nothing) (go j $ pred mj) $ x `dot` v
		_    -> Nothing

	maxJ x = case x of 
		L os -> if os == [] then Nothing else fmap succ . maxJ $ head os 
		O os -> either (const Nothing) (fmap succ . maxJ) $ x `dot` v 
		_    -> Just $ J 0 


-- * you need to think at a more conceptual level about how types are recognized and enforced 
-- * in J adts
-- * should J adts be its own data type???????
-- * and all thesse functiosn should just work on JADT, not jtype???
-- * data JAdt = Adt DataType Kind 

-- * what you really need is som orthongonal set of prims over js adts that'll
-- * reveal all posssible info you need to know about them


-- * TODO: SOME FUNCTION THAT TRAVERSE ALL TYPES IN PARAMEZTRIZED ADT OF ARB KIND

-- * read DataType of some JType
typeOf :: JType -> DataType
typeOf x = case x of 
	F _  -> "Function"
	U _  -> "Undefined"
	B _  -> "Boolean"
	D _  -> "Number"
	S _  -> "String"
	L _  -> "Array"
	O _  -> either (const "Object") (fromJust . unS) $ x `dot` t



{-----------------------------------------------------------------------------
	IV. Functions that service the Core
------------------------------------------------------------------------------}

-- * WHERE YOU LEFT OFF: IMPLEMENT AD _ _ _ CASE
-- * REFRESH MEMORY ON WHAT'S GOIN ON HERE BY WRITING LOTS OF TESTS
-- * CONSULT COMMENT ABOVE AD FOR DETAILS

-- * TODO: change everything to maybe error 

-- * Assert some JType `x` satisfy type signature Alpha `a` in conext of some typeclass `env`
satType :: [ClassEnv a] -> Alpha -> JType -> Maybe Error
satType env a x = let sat = satTcImpl env in case a of 
	
	Pr t   		   -> let t' = typeOf x in if t' == t then Nothing else Just . errMsg t $ t'

	Po (Sy _ _ cs) -> x `sat` cs

	Lst a          -> case x of 
		L xs -> foldErr . fmap (satType env a) $ xs
		_    -> Just $ "Error: Expected List, but received " ++ typeOf x	

	Tup a a'       -> case typeOf x of 
		"Tuple" -> foldErr [satType env a . fromJust . fstJ $ x, satType env a' . fromJust . sndJ $ x]
		_       -> Just $ "Error: Expected Tuple, but received " ++ typeOf x

	-- * after satTcImpl is implmented, rewrite this using `either` function
	Pa (Sy _ j cs) cs' a' -> case x `sat` cs' of 
		Just e  -> Just e 
		Nothing -> case x `sat` cs of  
			Just e  -> Just e 
			Nothing -> let mx' = x `atJ` (pred j) in case mx' of 
				Nothing -> Just "Error: Uncaught type error "
				Just x' -> satType env a' x'


	-- * this is a bit tricky because you have to zip list of Alphas `as`
	-- * with what ever values the paramterized data type is storing!???
	-- * cases: 0 value, 1 value, 2 values ...
	-- * So this function has to know a-priori, how to traverse some arb kinded Jtype

	-- * So there needs to be some primitive function to interface with arb kinded JType
	-- * if as = [a], then get first instance of stored type and type check, assuming adt is not polymormpic
	-- * if as = a:as' for as' /= [], ex: "Tree Int Char"
	-- * then you have to grab all unique types and make sure (//) of signature set and unique set is empty set []
	-- * GrabAllUnique types appear to be a quasi-fundamental operation in this case
	-- * GrabAllUnique is clearly proprotial to time take to traverse w/e sturcutre
	-- * it makes more sense to store it? either way the informaton is reachable .. 

	-- * take an interlude and imagine how it works:
	{-
		
		data Both a b = B a b a

		x = B 12 'c' 20 :: Both Int Char

		-- * there need to be something that stores type info
		-- * Note there's some template way to store value,  it actually doesnt make senes
		-- * all types needs to be store too, so it's just a matter of finding it in the list
		jx = [("Type", "Both"), ("Kind", k 2), (v, (...)), (P "a", S "Int"), (P "b", S "Char")]

	-}

	Ad n cs as     -> case satTcImpl env x cs of 
		Just e  -> Just e 
		Nothing -> case as of 
			[] -> Nothing
			as -> Just "paramterized adt type checking not implemented"

	where foldErr es = let err = catMaybes es in if err == [] then Nothing else Just $ foldr (++) "" err


-- * check `j`th layer of `x` has the same type as `j'`th layer of `y`
satConsist :: (J,J) -> JType -> JType -> Maybe Error
satConsist (j,j') x y = case (x `atJ` j, y `atJ` j') of 
	(Just x', Just y') -> if typeOf x' == typeOf y' then Nothing else Just "Type mismatch"
	_  				   -> Just "Uncaught TypeError" 


-- * this guy here has deep depedancies on inner working of `ClassEnv`
-- * Check if `x` implements all typeclasses specified by `cs`
satTcImpl :: [ClassEnv a] -> JType -> ClassPred -> Maybe Error
satTcImpl env x cs = Nothing
-- * Left "Error: satTcImpl not implemented" 
-- * if cs' == [] then Right x else Left $ "Error: typeclasses " ++ (foldr (\a b -> a ++ ", " ++ b) "" cs') ++ " Not implemented"
-- * where cs' = fmap fst cs \\ fromTc x 


-- * read typeclass implementation of js object
fromTc :: [ClassEnv a] -> JType -> [ClassName]
fromTc env x = error "fromTc not implemented"

errMsg :: DataType -> DataType -> Error
errMsg d d' = "Error: expected datatype " ++ d ++ ", but received " ++ d'

{-----------------------------------------------------------------------------
	V. Typeclass implemenation
------------------------------------------------------------------------------}

instance Eq JType where 
	x == x' = case (x,x') of 
		(U _, U _)  -> True
		(B b, B b') -> b == b'
		(D d, D d') -> d == d'
		(S s, S s') -> s == s'
		(L l, L l') -> l == l'
		(O o, O o') -> o == o'
		_  			-> False



