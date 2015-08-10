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

module SigParser ( 

	sigParser

) where 

import Control.Monad
import Control.Monad.Reader
import Control.Applicative

import Data.List 
import Data.Maybe
import Data.Monoid 
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Show.Functions

import ParserT
import ParserCombinator
import ParserUtils
import Core


-- * TO CONSIDER: 
-- * now that alpha is moved out of this package, think about all the prim combinators that'll shorten the code here 
-- * and make their intent more clear
-- * Think about how to decouple representation of the tree from strategy
-- * move all error messages into some reader environment?

{-----------------------------------------------------------------------------
	I. Data Type and Functions Servicing the Core
------------------------------------------------------------------------------}

-- * Map some singature unit x to a list of typeclass x maybe drawn from
type TcDict = [(Signature,[ClassName])]

-- * Given `Signature` and `Kind`, find appropriate list of `ClassId` from the set `TcDict`
classPred :: TcDict -> Kind -> Signature -> ClassPred
classPred tm k s = flip (,) k <$> join [ cn | (s', cn) <- tm, s == s' ]

-- * Prim functions over datatype from Core.hs * --

-- * symbol constructor
sym :: TcDict -> Kind -> Char -> Sym 
sym tm k c = Sy c (J 0) . classPred tm k $ pure c


-- * check two sets of `ClassId` have consistent `Kind` for the same `ClassName`
consistPred :: ClassPred -> ClassPred -> Maybe Error
consistPred cs cs' = let err n n' k k' = "expected kind " ++ show k ++ " for class " ++ n ++ ", but recieved kind " ++ show k' in 
	case [ err n n' k k' | (n,k) <- cs, (n',k') <- cs', n == n', k /= k'] of
		[] -> Nothing
		xs -> Just $ "Error: " ++ foldr (\a b -> if b == [] then a else a ++ ". In addition, " ++ b) "" xs  


-- * Note: this idx thing breaks parameterzied ADTs for p > 1
-- * Annote each layer of parameterized type with index `Kth`
idxAlpha :: Alpha -> Alpha 
idxAlpha sa = go sa (maxk sa) where
	go sa k = case sa of 
		Pa (Sy c _ mc) mc' s -> Pa (Sy c k mc) mc' . go s $ pred k 
		_  					-> sa
	maxk = trav 0 where 
		trav k sa = case sa of 
			Pa (Sy c x mc) mc' s -> flip trav s $ succ k
			_ 					-> k

-- * Take the cross product of all `Sym` within two `Alpha`
-- * This is conceptually similar to `collect <$> a1 `apSym` a2` if `Alpha` is parametrized over `Sym`,
-- * where `apSym` is some applicative-like interface over a subset of `Alpha` and `collect` gathers `Char`, `Y` and `[ClassId]`
xSym :: Alpha -> Alpha -> [((String,J,ClassPred),(String,J,ClassPred))]
xSym a = S.toList . S.fromList . x a where 
	a `x` b = case (a,b) of 
		(Po s 	    ,  Po s'     ) -> pure (fromSym s, fromSym s')
		(Po s       ,  Pa s' _ b') -> (fromSym s, fromSym s') : a `x` b'
		(Pa s _ a'  ,  Po s'     ) -> (fromSym s, fromSym s') : a'`x` b
		(Pa s _ a'  ,  Pa s' _ b') -> (fromSym s, fromSym s') : a `x` b' <> a' `x` b
		_  						   -> mempty
	fromSym (Sy c j cs) = (pure c, j, cs)


-- * Beta constructor
betaR :: Alpha -> Beta
betaR = (,) (I 0) . Right

betaL :: Signature -> Beta
betaL = (,) (I 0) . Left

{-----------------------------------------------------------------------------
	II. Alpha Parser :: Signature -> Alpha
------------------------------------------------------------------------------}
  
-- * Main parser for a single signature "word"
pAlpha :: TcDict -> Parser Char Alpha
pAlpha tm = ps <|> parens ps where 
	ps  = choice [par, lst, tup, pPol tm, prm, adhoc, pAdt tm]  
	prm = fmap Pr $ choice . fmap string $ ["Char", "Boolean", "Number", "Undefined"]
	tup = parens $ (\a _ b -> Tup a b) <$> pAlpha tm <*> (pure <$> (char ',') <|> commaSpc) <*> pAlpha tm
	lst = fmap Lst . bracket $ pAlpha tm
	par = idxAlpha <$> pPar tm

-- * Sub-Parsers * --

-- * Parse some polymorphic type
pPol :: TcDict -> Parser Char Alpha
pPol tm = Po . sym tm k0 <$> lowerLetter  			

-- * Parse nested alphabet of form "f (m (t a))"
-- * Each `Sym` in the `Alpha` is given appropriate `Kind` at parse time while `Kth` value is default to 0
pPar :: TcDict -> Parser Char Alpha
pPar tm = pa <$> lowerLetter <*> space <*> (parens (pAlpha tm) <|> pAlpha tm) where 
	pa a _ b  = Pa (sym tm k1 a) (preds a b) b
	preds a b = let b' = uAlpha b in join $ classPred tm k0 <$> [a : " " ++ b', a : " (" ++ b' ++ ")"]

-- * Parse algebraic datatype of arbitrary kind
pAdt :: TcDict -> Parser Char Alpha
pAdt tm = adt <$> name <*> many1 (flip const <$> space <*> (pPol tm <|> pAlpha tm)) <|> flip adt [] <$> name 
	where adt n as = Ad n (classPred tm k0 . uAlpha $ Ad n [] as) as

-- * Wildcards, such as prelude synonyms
adhoc :: Parser Char Alpha
adhoc = choice [str] where 
	str = (const . Lst . Pr $ "Char") <$> string "String"

{-----------------------------------------------------------------------------------
	III. Signature Parser :: Signature -> SigTree
------------------------------------------------------------------------------------}

-- * TODO: functions that return functions are not properly parsed
-- * Putting random words in singature throttles it
pSignature :: Parser Char SigTree
pSignature = fmap index $ (pPredSig >>= \tc -> space >>= \_ -> pFnSig tc) <|> pFnSig mempty where 
	index = reverse . fst . foldl (\(cs,i) (_,a) -> ((i,a) : cs, succ i)) ([], I 0)

-- * Parse function signature main
-- * If the parameter of a function is a function, then the parameter is `unparsed`
pFnSig :: TcDict -> Parser Char SigTree
pFnSig tm = (\a _ b -> a:b) <$> pSigAlpha <*> pArr <*> pFnSig tm <|> pure . betaR <$> pAlpha tm where 
	unparsed  = (\a b c -> a ++ " " ++ b ++ " " ++ c) <$> alpha <*> pArr <*> unparsed <|> alpha
	pSigAlpha = betaR <$> pAlpha tm <|> betaL <$> parens unparsed
	alpha     = uAlpha <$> pAlpha []
	pArr      = string " -> " 

-- * NOTE: this does not parse adt drawn from typecass in tc predicate
-- * Represent a list of `Typeclass` and its `Kind` as claimed by typeclass predicate
pPredSig :: Parser Char TcDict
pPredSig = toMap <$> (one <|> oneP <|> manyP) where 
	one      = const . pure <$> pTc <*> spcArr
	oneP     = const <$> parens (pure <$> pTc) <*> spcArr 
	manyP    = const <$> (parens $ pTc `sepBy` commaSpc) <*> spcArr 
	pTc      = (\a _ b -> (b,a)) <$> name <*> space <*> (parens (uAlpha <$> pPar []) <|> (pure <$> lowerLetter) <|> parens (uAlpha <$> pAdt []))
	toMap xs = M.toList . M.fromListWith (++) $ [(k, [v]) | (k, v) <- xs]
	spcArr   = string " =>"

{-----------------------------------------------------------------------------------
	IV. Alpha Analyzer :: Alpha -> Maybe Error
------------------------------------------------------------------------------------}

-- * Note sections IV - VI doesnt exactly belong  here, and could be factored out of `sigParser` into its own process
-- * Also note given predicate p  = t <- Class, there's no check whether p is true 

-- * Internal consistency 
-- * Given any two `Alpha`, check `Sym` with the same char have the same kind 
-- * Note the kind is derived from syntax of the `Signature`
tConsist :: Alpha -> Alpha -> Maybe Error
tConsist x y = if xs == [] then Nothing else Just xs where 
		xs = foldr (\a b -> a ++ ". " ++ b) [] . catMaybes $ uncurry sat <$> x `xSym` y  
		sat (c,k,cs) (c',k',cs') = if c == c then consistPred cs cs' else Nothing 


-- * Accuracy 
-- * Given some `Alpha` and `ClassPred` of existing classes and true kinds
-- * assert parsed typeclass is in scope and of the correct kind
tAccu :: ClassPred -> Alpha -> Maybe Error
tAccu env s = case s of 
	Po (Sy _ k tc)        -> tc `sat` env
	Ad _ tc as            -> maybe (maptAccu as) Just $ tc `sat` env
	Pa (Sy _ _ tc) tc' s' -> maybe (maybe (tAccu env s') Just $ tc' `sat` env) Just $ tc `sat` env
	_ 				      -> Nothing
	where 
		sat tc env  = toErr . sat' env $ tc

		maptAccu as = case catMaybes $ tAccu env <$> as of 
			[] -> Nothing
			es -> Just $ foldr (\a b -> a ++ "; " ++ b) "" es

		sat' env xs = ((fst <$> xs) \\ (fst <$> xs'), xs'') where   
			xs'  = [ (n',k')| (n,_) <- env, (n',k') <- xs , n == n'         ] 	
			xs'' = [ n'     | (n,k) <- env, (n',k') <- xs', n == n', k /= k']	   

		toErr (os,ks) = case (os,ks) of 
			([],[]) -> Nothing
			(os,[]) -> Just $ "Error : typeclasses " ++ (foldr (\a b -> a ++ " " ++ b) "" os) ++ "out of scope"
			([],ks) -> Just $ "Error : typeclasses " ++ (foldr (\a b -> a ++ " " ++ b) "" ks) ++ "have the wrong kind"
			(os,ks) -> toErr (os,[]) <> Just ". In addition, " <> toErr ([],ks)


-- * Existence of algebraic datatype
-- * Given some `Alpha` and `AdtEnv`, assert parsed adt is in scope
tAdt :: AdtEnv a -> Alpha -> Maybe Error
tAdt env s = Nothing

{-------------------------------------------------------------------------------------
	V. Signature Analyzer :: SigTree -> Either Error SigTree
--------------------------------------------------------------------------------------}

-- * Check all ADT and typeclasses named in signature tree are in scope, and all parametric 
-- * types represented by the same symbol are of the same kind
analyzer :: ClassPred -> AdtEnv a -> SigTree -> Either Error SigTree
analyzer cenv aenv t = adtScope >> accuracy >> consistency >> return t 
	where 
		adtScope     = foldErr . tAdt  $ aenv
		accuracy     = foldErr . tAccu $ cenv
	 	consistency  = toEither . catMaybes . S.toList . S.fromList $ sat <$> t <*> t where 
			sat (_,Right x) (_,Right y) = tConsist x y
			sat _ _ 				    = Nothing	
		toEither es  = if es == [] then return t else Left $ foldr (<>) mempty es
		foldErr g    = toEither $ foldr (\(_,x) es -> either (const es) (maybe es (flip (:) es) . g) x) mempty t

{-------------------------------------------------------------------------------------
	VI. Build Equality Constraint  :: SigTree -> EqConstr
--------------------------------------------------------------------------------------}

-- * Pairwise compare all nodes in signature tree and build EqMatrix
eqMatrix :: SigTree -> EqMatrix
eqMatrix t = join $ mat <$> t <*> t where 
	mat (x,Right p) (x',Right q) = catMaybes $ (\((c,k,_), (c',k',_)) -> if c == c' then Just ((x,k),(x',k')) else Nothing) <$> p `xSym` q 
	mat _ _ 					  = []

{-------------------------------------------------------------------------------------
	VII. Parse, Analyze, eqMatrix :: Signature -> (SigTree, EqConstr)
--------------------------------------------------------------------------------------}

-- * Main Parser/Analyzer computation
sigParser :: ParserT Char (ReaderT ClassPred (Reader (AdtEnv a))) (SigTree, EqMatrix)
sigParser = do
	t    <- pSignature
	cenv <- lift ask 
	aenv <- lift . lift $ ask
	either failP (const . return $ (t, eqMatrix t)) $ analyzer cenv aenv t



