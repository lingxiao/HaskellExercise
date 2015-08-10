{- 
	Learn Combinators
	
	Created: April 12th

	tutorial source: http://www.cs.tufts.edu/~nr/cs257/archive/doaitse-swierstra/combinator-parsing-tutorial.pdf
-}

import Control.Applicative
import Control.Monad
import Data.Array
import Data.List
import Test.QuickCheck


----------------
-- Data Types --
----------------

data Symbol = String

data Token = Identifier -- terminal symbol used in parser
	| Ident String 		-- token constructed by scanner
	| Number Int
	| If Symbol
	| Then Symbol


-- | Accomadate our identifier and the one parsed
instance Eq Token where
	( Ident _ ) == Identifier = True


-- | The parser accepts a list of strings that changes state
-- | Since parsing is ambigous, the parser return a list of possible results
-- | each one is represented by a tuple of the tree built, and the list with a change of state
newtype Parser s t = Parser ( [s] -> [(t, [s])] )

-- | unwrap parser 
runParser :: Parser s t -> [s] -> [(t,[s])]
runParser ( Parser p ) = p



-----------------------
-- Parser Components --
-----------------------

pSym :: Eq a => a -> Parser a a 
pSym a = Parser $ \stream -> case stream of 
	(s:ss) | s == a -> [(s,ss)]
	otherwise       -> []

pIdent :: Parser Token Token
pIdent = pSym Identifier

-- | Note this is a legacy function from tutorial, it's replaced by 'pure' below
pReturn :: a -> Parser s a
pReturn a = Parser $ \stream -> [(a, stream)]

pFail :: Parser s a
pFail = Parser $ const []


-------------------------------------------------------
-- Combine Components With Functors and Applicatives --
-------------------------------------------------------

instance Functor ( Parser s ) where
	fmap g ( Parser p ) = Parser $ \ss -> fmap (\(a,b) -> (g a, b)) $ p ss


instance Applicative ( Parser s ) where
	pure a 				    = Parser $ \ss -> [(a,ss)]
	Parser p1 <*> Parser p2 = Parser $ \ss -> [ (v1 v2, ss2) | (v1, ss1) <- p1 ss, (v2, ss2) <- p2 ss1 ]


-- | Choice, also found in Control.Applicative.Alternative
( <|> ) :: Parser s a -> Parser s a -> Parser s a
Parser p1 <|> Parser p2 = Parser $ \ss -> p1 ss ++ p2 ss

-- | fmap, as an infix operator
( <$> ) :: (a -> b) -> Parser s a -> Parser s b
g <$> parser   = pure g <*> parser



-----------------------------------------------
-- Test Functors and Applicative definations --
-----------------------------------------------

-- fmap --
trivialf a = 'z'

mapret = runParser ( fmap trivialf ( pReturn 'a' ) ) "hello" :: [(Char, [Char])]

-- <$> --
mapret' = runParser ( trivialf Main.<$> ( pReturn 'a' ) ) "hello" :: [(Char, [Char])]


-- <*> --
-- | write atomic parser
plett :: Parser Char Char
plett = Parser $ \stream -> case stream of
	(s:ss) | s == 'a'  -> [(s,ss)]
	otherwise          -> []

-- | Use applicatives to create a more complex parser
plett2 :: Parser Char [Char]
plett2 = ( pure (:) <*> plett ) <*> ( pure (\x -> [x]) <*> plett )


-- | Non-idiomatically, rewrite another 'atomic' parser that does something slightly diff than first
plett2' :: Parser Char [Char]
plett2' = Parser $ \stream -> case stream of
	(s1:s2:ss) | s1 == 'a' && s2 == 'a' -> [([s1,s2],ss)]
	otherwise -> []























--instance Monad ( Parser s ) where
--	return a         = Parser $ \stream -> [( a, stream )]
--	(Parser p) >>= g = undefined

