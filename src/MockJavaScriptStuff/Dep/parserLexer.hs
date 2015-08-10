{-# LANGUAGE ImpredicativeTypes #-} {-# LANGUAGE NoMonomorphismRestriction #-}
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Play with bad solution part iii
-- | Creator: Xiao Ling
-- | Created: November 17th
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Play3 where 

import Prelude hiding (foldr)

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.Trans.Either

import Data.Tuple
import Data.Maybe
import Data.Foldable
import Data.Semigroup
import Data.Traversable
import Data.List hiding (foldr)
import Data.Monoid hiding ((<>))
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Show.Functions

import Types
import ParserT
import ParserCombinator
import ParserUtils
import Coroutine
import Strategy3


{-----------------------------------------------------------------------------
   I. Primitives Types
------------------------------------------------------------------------------}

type Error       = String					 	-- * Error message
type DataType    = String 						-- * Name of js data type
type Signature   = String 						-- * unparsed function signature

type FuncName    = String 
type ClassName   = String
data Kind        = K Int deriving (Show, Eq, Ord)	-- * * or * -> * or * -> * -> * or ..

type ClassId     = (ClassName, Kind) 			-- * Name and kind of a list of classes

type ClassPred   = [ClassId] 			 		-- * List of existing classes and their kinds

{-----------------------------------------------------------------------------
   II. Signature Tree
------------------------------------------------------------------------------}

-- * Some polymorphic type indexed within a larger nested form at level Y
-- * Sym carries some context where data type represented by `Char` maybe drawn from some set typeclasses of some `Kind`s
data Sym = S Char Y [ClassId] deriving (Show, Eq)						 

-- * Alphabet
data Alpha    
	= Po Sym    				-- * polymorphic type, maybe unparametrized typeclass    "a" or "Monoid m => m"
	| Pr String 			    -- * primitive type          						     "Bool"
	| Lst Alpha 			    -- * list				   							     "[a]" 
	| Tup Alpha Alpha 			-- * tuple				   							     "(a,b)"
	| Pa Sym [ClassId] Alpha    -- * paramed type where the whole nested form maybe drawn from some typeclass "(Monoid (t (m a)), Monad m) => t (m a)"
	deriving (Show, Eq)

-- * SigAlpha is either some unparsed string, or fully parsed `Alpha`
-- * Note paraemters and results that are functions are not parsed
type SigAlpha = Either Signature Alpha      

-- * An unbalanced tree where the structure encodes proper form of function curring
data Tree a = L a | a `Send` Tree a

{-----------------------------------------------------------------------------
   III. Equality Matrix
------------------------------------------------------------------------------}

-- * `X`: location of a particular `Alpha` within the `SigTree`
-- * `Y`: depth of a container's layer in "this signature counting from inside out. Ie, in "f (m a)", a is 0 and f is 2
type X  = Int
type Y  = Int 

-- * Yth datatype in Xth parameter is the same as Yth' type in Xth' parameter 
type EqMatrix = [(,) (X,Y) (X,Y)]  	  	

{-----------------------------------------------------------------------------
	IV. Typeclass Implementation
------------------------------------------------------------------------------}

instance Show a => Show (Tree a) where
	show t = case t of 
		L a      -> show a 
		Send a b -> show a ++ "\n" ++ "->" ++ "\n" ++ show b 

instance Semigroup (Tree a) where
	t1 <> t2 = case (t1,t2) of 
		(L a, _)           -> Send a t2
		(Send a t1', t2)   -> Send a $ t1' <> t2

instance Functor Tree where
	fmap g t = case t of
		L a      -> L . g $ a
		Send a t -> Send (g a) $ g <$> t 

instance Applicative Tree where 
	pure 	  = L 
	t1 <*> t2 = case (t1,t2) of 
		(L g, _)                 -> g <$> t2
		(Send g t1', L a)        -> (L . g $ a) <> (t1' <*> t2)
		(Send g t1', Send a t2') -> (L . g $ a) <> (g <$> t2') <> (t1' <*> t2')

instance Foldable Tree where
	foldr g z (L a)      = g a z
	foldr g z (Send a t) = foldr g (g a z) t 

instance Traversable Tree where 
	traverse g (L a)      = L <$> g a
	traverse g (Send a t) = Send <$> (g a) <*> (traverse g t)

{-----------------------------------------------------------------------------
	V. Primitive Functions over Data Types
------------------------------------------------------------------------------}

-- * check two sets of `ClassId` have consistent `Kind` for the same `ClassName`
consistId :: [ClassId] -> [ClassId] -> Maybe Error
consistId cs cs' = let err n n' k k' = "expected kind " ++ show k ++ " for class " ++ n ++ ", but recieved kind " ++ show k' in 
	case [ err n n' k k' | (n,k) <- cs, (n',k') <- cs', n == n', k /= k'] of
		[] -> Nothing
		xs -> Just $ "Error: " ++ foldr (\a b -> if b == [] then a else a ++ ". In addition, " ++ b) "" xs  


-- * `uAlpha . pAlpha` is identity of a single `Singature` alphet
uAlpha :: Alpha -> Signature
uAlpha a = let pp (S c _ _) = c in case a of 
		Pr p     -> p 
		Po c     -> pure . pp $ c
		Lst a    -> "[" ++ uAlpha a ++ "]"
		Tup a b  -> "(" ++ uAlpha a ++ "," ++ uAlpha b ++ ")"
		Pa c _ s -> case s of 
			Pa _ _ _ -> pure (pp c) ++ " (" ++ uAlpha s ++ ")"
			_        -> pure (pp c) ++ " " ++ uAlpha s 


-- * Annote each layer of parameterized type with index `Kth`
idxAlpha :: Alpha -> Alpha 
idxAlpha sa = let n = maxk sa in go sa n where
	go sa k = case sa of 
		Pa (S c _ mc) mc' s -> Pa (S c k mc) mc' . go s $ pred k 
		_  					-> sa
	maxk = trav 0 where 
		trav k sa = case sa of 
			Pa (S c x mc) mc' s -> flip trav s $ succ k
			_ 					-> k

-- * Take the cross product of all `Sym` within two `Alpha`
-- * This is conceptually similar to `collect <$> a1 `apSym` a2` if `Alpha` is parametrized over `Sym`,
-- * where `apSym` is some applicative-like interface over a subset of `Alpha` and `collect` gathers `Char`, `Y` and `[ClassId]`
xSym :: Alpha -> Alpha -> [((Char,Y,[ClassId]),(Char,Y,[ClassId]))]
xSym a = S.toList . S.fromList . cross a where 
	a `cross` b = case (a,b) of 
		(Po (S c k cs) 		 ,  Po (S c' k' cs')     ) -> pure ((c,k,cs),(c',k',cs')) 
		(Po (S c k cs)       ,  Pa (S c' k' cs') _ b') -> ((c,k,cs), (c',k',cs')) : a `cross` b'
		(Pa (S c k cs) _ a'  ,  Po (S c' k' cs')     ) -> ((c,k,cs), (c',k',cs')) : a'`cross` b
		(Pa (S c k cs) _ a'  ,  Pa (S c' k' cs') _ b') -> ((c,k,cs), (c',k',cs')) : a `cross` b' <> a' `cross` b
		_  										       -> mempty

-- * `foldr (++) [] $ uAlpha <$> fromTree tree` is identity of some `Singature` free of typeclass predicates
fromTree :: Tree a -> [a]
fromTree t = case t of 
	L a       -> pure a
	Send a t  -> a : fromTree t










-- * TO_CONSIDER: now that alpha is moved out of this package, think about all the prim combinators that'll shorten the code here 
-- * and make their intent more clear

-- * Think about how to decouple representation of the tree from strategy
-- * note AST should not be exported per name suggests
-- * the ast is too complex and there's a lot of de-facto modifications going on
-- * why does such a simple little language require SO MUCH code that's hard to follow?
-- * move all error messages into some reader environment?

{-----------------------------------------------------------------------------
	I. Data Type and Function over Data Type
------------------------------------------------------------------------------}

-- * Map Alpha to of `ClassName` denoting typeclass it could be draw from
type TcDict = [(Signature,[ClassName])]

-- * Given `Signature` and `Kind`, find appropriate `ClassId` drawn from `TcDict`
classPred :: TcDict -> Kind -> Signature -> ClassPred
classPred tm k s = flip (,) k <$> join [ cn | (s', cn) <- tm, s == s' ]

sym :: TcDict -> Kind -> Char -> Sym 
sym tm k c = S c 0 . classPred tm k $ pure c

{-----------------------------------------------------------------------------
	II. Alpha Parser :: Signature -> Alpha
------------------------------------------------------------------------------}

pAlpha :: TcDict -> Parser Char Alpha
pAlpha tm = choice [pa, lst, tup, po, pr, adhoc] where 
	pr  = Pr <$> pPrims
	pa  = idxAlpha <$> pPa tm
	po  = (Po . sym tm (K 0)) <$> lowerLetter
	lst = fmap Lst . bracket $ pAlpha tm
	tup = parens $ (\a _ b -> Tup a b) <$> pAlpha tm <*> (pure <$> (char ',') <|> commaSpc) <*> pAlpha tm

-- * Parse nested alphabet of form "f (m (t a))"
-- * Each `Sym` in the `Alpha` is given appropriate `Kind` at parse time while `Kth` value is default to 0
pPa :: TcDict -> Parser Char Alpha
pPa tm = pa <$> lowerLetter <*> space <*> (parens (pAlpha tm) <|> pAlpha tm) 
	where pa a _ b = Pa (sym tm (K 1) a) (classPred tm (K 0) $ uAlpha b) b

-- * Only these primitives are allowed
pPrims :: Parser Char String
pPrims = choice . fmap string $ ["Char", "Bool", "Double", "()"]

-- * Wildcards
adhoc :: Parser Char Alpha
adhoc = choice [str] where 
	str = (const . Lst . Pr $ "Char") <$> string "String"

{-----------------------------------------------------------------------------------
	III. Signature Parser :: Signature -> Tree (Either Signature Alpha)
------------------------------------------------------------------------------------}

-- * TODO: functions that return functions are not properly parsed
-- * Putting random words in singature throttles it

pSignature :: Parser Char (Tree SigAlpha)
pSignature = (pPredSig >>= \tc -> space >>= \_ -> pFunSig tc) <|> pFunSig mempty

-- * Parse function signature main
-- * If the parameter of a function is a function, then the parameter is `unparsed`
pFunSig :: TcDict -> Parser Char (Tree SigAlpha)
pFunSig tm = (\a _ b -> Send a b) <$> pSigAlpha <*> pArr <*> pFunSig tm <|> L . Right <$> pAlpha tm where 
	unparsed = (\a b c -> a ++ " " ++ b ++ " " ++ c) <$> alpha <*> pArr <*> unparsed <|> alpha
	pSigAlpha = Right <$> pAlpha tm <|> Left <$> parens unparsed
	alpha    = uAlpha <$> pAlpha tm 
	pArr     = betweenS . string $ "->"

-- * Represent a list of `Typeclass` and its `Kind` as claimed by typeclass predicate
pPredSig :: Parser Char TcDict
pPredSig = toMap <$> (one <|> oneP <|> manyP) where 
	spcArr   = string " =>"
	one      = const . pure <$> pTc <*> spcArr
	oneP     = const <$> parens (pure <$> pTc) <*> spcArr 
	manyP    = const <$> (parens $ pTc `sepBy` commaSpc) <*> spcArr 
	pTc      = (\a _ b -> (b,a)) <$> name <*> space <*> (parens (uAlpha <$> pPa []) <|> (pure <$> lowerLetter))
	toMap xs = M.toList . M.fromListWith (++) $ [(k, [v]) | (k, v) <- xs]

{-----------------------------------------------------------------------------------
	IV. Alpha Type Checker :: Alpha -> Alpha
------------------------------------------------------------------------------------}

-- * Internal consistency where give any two `Alpha`, check `Sym` with the same char have the same kind derived from syntax of the `Signature`
satConsist :: Alpha -> Alpha -> Maybe Error
satConsist x y = if xs == [] then Nothing else Just xs where 
		xs = foldr (\a b -> a ++ ". " ++ b) [] . catMaybes $ uncurry sat <$> x `xSym` y  
		sat (c,k,cs) (c',k',cs') = if c == c then consistId cs cs' else Nothing 

-- * TODO: imporve error message
-- * Accuracy where given some `Alpha` and `ClassEnv` of existing classes and true kinds, ensure parsed typeclass is in scope and of the correct kind
satAccu :: ClassPred -> Alpha -> Maybe Error
satAccu env s = case s of 
	Po (S _ k tc)      -> toErr . sat env $ tc
	Pa (S _ _ tc) _ s' -> maybe (satAccu env s') Just $ toErr . sat env $ tc
	_ 				   -> Nothing
	where 
		sat env xs = ((fst <$> xs) \\ (fst <$> xs'), xs'') where   
			xs'  = [ (n',k')| (n,_) <- env, (n',k') <- xs , n == n'         ] 	
			xs'' = [ n'     | (n,k) <- env, (n',k') <- xs', n == n', k /= k']	   

		toErr (os,ks) = case (os,ks) of 
			([],[]) -> Nothing
			(os,[]) -> Just $ "Error : typeclasses " ++ (foldr (\a b -> a ++ " " ++ b) "" os) ++ "out of scope"
			([],ks) -> Just $ "Error : typeclasses " ++ (foldr (\a b -> a ++ " " ++ b) "" ks) ++ "have the wrong kind"
			(os,ks) -> toErr (os,[]) <> Just ". In addition, " <> toErr ([],ks)


{-------------------------------------------------------------------------------------
	V. Signature Type Checker :: Tree SigAlpha -> Tree SigAlpha
--------------------------------------------------------------------------------------}

typeChecker :: Tree SigAlpha -> EitherT Error (Reader ClassPred) (Tree SigAlpha)
typeChecker t = lift ask >>= \env -> eitherT (accuracy env t) >> eitherT (consistency t) >> return t 
	where  
		eitherT errs   = if errs == [] then return t else left $ foldr (<>) mempty errs
		accuracy env t = foldr (\x es -> either (const es) (maybe es (flip (:) es) . satAccu env) x) mempty t
	 	consistency  t = catMaybes . S.toList . S.fromList $ sat <$> t' <*> t' where 
	 		t' = fromTree t
			sat (Right x) (Right y) = satConsist x y
			sat _ _ 				= Nothing	

{-------------------------------------------------------------------------------------
	VI. Build Equality Constraint  :: Tree SigAlpha -> EqConstr
--------------------------------------------------------------------------------------}

-- * this pipeline: parse -> typeChecker -> foldAndLabel -> generateEqMatrix -> (labledTree, EqMatrix) -> Strategy a

-- * Pairwise compare all nodes in tree and build EqMatrix
eqMatrix :: Tree SigAlpha -> EqMatrix
eqMatrix t = join $ mat <$> t' <*> t'
	where 
		t' = reverse . fst . Data.List.foldl (\(cs,i) c -> ((c,i):cs, succ i)) ([],0) . fromTree $ t
		mat (a,x) (a',x') = case (a,a') of 
			(Right p, Right q) -> catMaybes $ (\((c,k,_), (c',k',_)) -> if c == c' then Just ((x,k),(x',k')) else Nothing) <$> p `xSym` q 
			_				   -> []


{-------------------------------------------------------------------------------------
	VII. Pipeline :: Signature -> (Tree SigAlpha, EqConstr)
--------------------------------------------------------------------------------------}

-- * This whole thing is some nested `Either` branches of computation that could fail or yield some result
-- * note this whole thing should be some computational context, storing a bunch of things like: list form of the tree
-- * Also note there's practically no point in having `Tree` when all operations you want to do is over the list anwyyas
parseLex :: Signature -> (Tree SigAlpha, EqMatrix)
parseLex s = undefined where 
	t  = let Right (x,_) = runP pSignature s in x

{-----------------------------------------------------------------------------
	Appendix
------------------------------------------------------------------------------}

-- * Parameters * -- 

tenv :: TcDict
tenv = (\(a,b) -> (a,pure b)) <$> [("f", "Functor"), ("m", "Monad"), ("t", "Traversable"),("a", "Monoid"), ("f (m a)", "Monoid"), ("m a", "Monoid")]

cenv :: ClassPred
cenv = [("Functor", K 1), ("Monad", K 1), ("Monoid", K 0)]

cpred :: [ClassId]
cpred = [("Functor", K 1), ("Monoid", K 1), ("Xclass", K 1)]

-- * some tree 
t = let Right (x,_) = runP pSignature "a -> b -> (c -> d) -> Bool" in x


-- * test satConsist * -- 

a,c :: Alpha
a = let Right (x,_) = runP (pAlpha tenv) "m a" in x
c = let Right (x,_) = runP (pAlpha [("m", ["Monoid"])]) "m" in x

-- * test satAccu * -- 

s1 = satAccu cenv a  																					-- * pass
s2 = satAccu cenv x where x = let Right (x,_) = runP (pAlpha [("x", ["Monoid", "Xclass"])]) "x" in x 	-- * out of scope
s3 = satAccu cenv x where x = let Right (x,_) = runP (pAlpha [("f", pure "Functor")]) "f" in x 			-- * wrong kind




af i = S 'f' i $ [("Functor", K 1)]  -- * f _
am i = S 'm' i $ [("Monad", K 1)]    -- * m _


ae i = S 'm' i $ [("Monad", K 2)]
a_   = Po . S 'a' 0 $ [("Monoid", K 1)]

a0 = Po . S 'b' 0 $ [] 				 -- * b
a1 = Po . S 'a' 0 $ [("Monoid", K 0)] 				 -- * a
a2 = Pa (af 1) [] a1				 -- * f a
a3 = Pa (af 2) [] a2				 -- * f (f a)
a4 = Pa (am 1) [] a1 				 -- * m a
a5 = Pa (am 1) [] a0				 -- * m b
a6 = Pa (am 2) [] a2 				 -- * m (f a)
a7 = Pa (ae 0) [] a_ 				 -- * m a   where both m and a have wrong kind

a11 = aMatr (a1,0) (a1,1)   		 -- * a, a
a12 = aMatr (a1,0) (a2,1)   		 -- * a, f a
a13 = aMatr (a1,0) (a3,1)   		 -- * a, f (f a)
a22 = aMatr (a2,0) (a2,1)   		 -- * f a, f a
a23 = aMatr (a2,0) (a3,1)   		 -- * f a, f (f a)
a25 = aMatr (a2,0) (a5,1) 			 -- * f a, m b
a36 = aMatr (a3,0) (a6,1)    	     -- * f (f a), m (f a)


-- * functions that was rolled up into bigger ones * --


-- * Label the nodes pre-order
label :: Tree SigAlpha -> Tree (SigAlpha, X)
label = flip evalState 0 . traverse mark where mark c = get >>= \i -> modify (+1) >> return (c,i)


-- * Pairwise compare two `Right` nodes and build `EqMatrix`
aMatr :: (Alpha,X) -> (Alpha,X) -> EqMatrix
aMatr (p,x) (q,x') = catMaybes $ (\((c,k,_), (c',k',_)) -> if c == c' then Just ((x,k),(x',k')) else Nothing) <$> p `xSym` q 





-- * test satAccu'
--se1 = let Right (x,_) = runP pSignature "Functor f => f a" in satAccu' cenv x 									-- * satisfy
--se2 = let Right (x,_) = runP pSignature "Cat n => n a" in satAccu' cenv x 										-- * class not in scope
--se3 = let Right (x,_) = runP pSignature "Monad m => m" in satAccu' cenv x 										-- * wrong kind


-- * test satConsist' * -- 

--t1 = let (Right (y,_)) = runP pSignature "(Functor f) => f a" in satConsist' y 
--t2 = let (Right (y,_)) = runP pSignature "(Functor f) => f a -> f" in satConsist' y 

--satAccu' :: Monad m => ClassPred -> Tree SigAlpha -> EitherT Error m (Tree SigAlpha)
--satAccu' env t = if errs == [] then return t else left $ foldr (<>) mempty errs
	--where errs = foldr (\x es -> either (const es) (maybe es (flip (:) es) . satAccu env) x) [] t

--satConsist' :: Monad m => Tree SigAlpha -> EitherT Error m (Tree SigAlpha)
--satConsist' t = if errs == [] then return t else left . foldr (<>) mempty $ errs where 
--	t'   = fromTree t
--	errs = catMaybes . S.toList . S.fromList $ g <$> t' <*> t'  
--	g (Right x) (Right y) = satConsist x y
--	g _ _ 				  = Nothing	


-- * test aMatr










