{-
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
-}


{-# LANGUAGE FlexibleInstances #-}  
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE ImpredicativeTypes #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE UnicodeSyntax #-}



module Parser (
    Parser
  , runP
  , getC
  , satisfy

  , withAuto   -- * note this actually belongs in utils.hs, but implementation prevents it from so 

  ) where

import Control.Monad
import Control.Applicative 
import Control.Monad.State
import Control.Monad.Identity

import Control.Monad.Trans
import Control.Monad.Identity


import Utils
import Data.Monoid
import Data.Maybe

import Data.Machine 
import Data.Char

import Transducer
import Type
import Variants


import Control.Monad.ListT
import Data.List

---------------
-- Data Type --
--------------- 

-- | note 'a' denote type of elements inside the list
newtype Parser a b = P { runP :: [a] -> [(b, [a])] }

---------------
-- Typeclass --
---------------

instance Functor (Parser a) where
  fmap g p = P $ \as -> (\(a,b) -> (g a, b)) <$> runP p as 


instance Applicative (Parser a) where
  pure a     = P $ \as -> [(a,as)]
  p1 <*> p2  = P $ \as -> do
    (g, as')  <- runP p1 as
    (x, as'') <- runP p2 as'
    return ( g x, as'' )r


instance Monad (Parser a) where
  return x  = P $ \as -> [ (x, as) ]
  
  p1 >>= g  = P $ \as -> do 
    (a,as') <- runP p1 as
    runP (g a) as'
  
  fail _    = P $ \_   ->  []


instance Alternative (Parser a) where
  empty     = P $ const []
  p1 <|> p2 = P $ \as -> case runP p1 as of 
    (x:xs) -> x:xs
    []     -> runP p2 as


-- | mappend is taking over for choose
instance Monoid (Parser a b) where
  mempty = empty
  p1 `mappend` p2 = P $ \as -> runP p1 as ++ runP p2 as


-------------------------
-- Primitive Functions --
-------------------------

-- | Return the next character
getC :: Parser a a
getC = P $ \as -> case as of 
  (x:xs) -> [(x,xs)]
  []     -> []

-- | Return the next character if it satisfies the given predicate
satisfy :: (a -> Bool) -> Parser a a
satisfy p = do 
  c <- getC
  if (p c) then return c else fail "End of input"



p :: Parser Char Int
p = do 
  c  <- getC
  c' <- getC
  return $ length [c,c']




-----------------------------------------------------------------------------------------------------
------------------------- Prelim mockup of final `with` parser --------------------------------------
-----------------------------------------------------------------------------------------------------

-- | critique: how is this different from just a regular recurive function running a scanner like function?

-- | note how terminal condition is brought inside, this type should compose with `satisfy` somehow
withh :: (Eq a, Monoid b) => (a -> Bool) -> ([a] -> [b]) -> Parser a b
withh c t = P $ go c t
  where go c t as = flip (,) (as \\ seen) <$> bs where (seen,_,bs) = execState (runT $ source as ~> scanner c t) ([],[],[])


type Scanned a b = State (([a],[a],[b]))


-- * Critique: this scanner is a subroutine w/i withh, and does not account for something like 
-- * a stateful function, even though the whole thing gets turned into a ProcessT to end with
scanner :: (Eq a, Monoid b) => (a -> Bool) -> ([a] -> [b]) -> ProcessT (Scanned a b) a ()
scanner c t = repeatedly $ do
  a <- await
  modify $ \(seen,buffer,bs) -> (seen ++ [a], buffer ++ [a], bs)
  (_,as,_) <- get
  case (c a, t as) of 
    (True, _) -> stop
    (_, [])   -> yield ()
    (_, rs)   -> modify (\(seen,buffer,bs) -> (seen, [], (<>) <$> bs <*> rs <||> bs <||> rs)) >> yield ()



nfst :: String -> [String]
nfst []       = []
nfst "phone"  = ["root-phone-Noun"]
nfst "s"      = ["Plural", "Third-Person"]
nfst _        = []




-- | alternate design like above, instead of using coroutineT (State), just use regular State monad
withh' :: (Eq a, Monoid b) => (a -> Bool) -> ([a] -> [b]) -> Parser a b
withh' p t = P $ \as -> let (_,bs,xs) = execState (scanner' p t as) ([],[],[]) in flip (,) xs <$> bs


-- | difference with scanner:
-- | x: explicit pattern match as opposed to await
-- | xs: can acess vs cannot access

-- | similarities: 
-- | in context of withh, both are InFact, subroutines, so both require compelete access to `as`,
-- | both have to be implemented here since it requires acess to a
scanner' _ _ []     = return ()
scanner' p t (x:xs) = do
  modify $ \(as,bs,_) -> (as++[x],bs,xs)
  (as,_,_) <- get
  case p x of
    True -> return ()
    _    -> case t as of 
      [] -> scanner' p t xs
      rs -> modify (\(as,bs,rest) -> ([], (<>) <$> bs <*> rs <||> bs <||> rs, rest)) >> scanner' p t xs


top' = scanner' (=='#') nfst "phones# works!" 


rs = execState top' ([],[],[])

w = withh' (=='#') nfst 



---------- * find a better approximation of a good abstraction attemp 1 * -------------


-- * priority: which bit of logic in scanner' or scanner belong in Parser.hs? which can be refactored elsewhere?


-- * with a signature like: withProcess :: Process a b -> Parser a b, we can remove the need for execss logic


-- | CRITIQUE: two way leaky abstraction
-- | note this puts fine and unreasonable restrictions on how underlying State monad works
withPr :: Eq a => ProcessT (State ([a], [a], [b])) a b -> Parser a b
withPr m = P $ go m 
  where go m as = let (_,as',bs) = execState (runT $ source as ~> m) ([],[],[]) in flip (,) (as \\ as') <$> bs

m = scanner (=='#') nfst :: ProcessT (Scanned Char String) Char ()



---------- * find a better approximation of a good abstraction attemp 2 * -------------

-- * To keep it flexible, still put in, `ProcessT m a b` with a seed value `c`?
-- * so with :: ProcessT m a b -> c -> Parser a b



-- * Or how about just : transduce :: NFST f a b -> Parser a b ??  -> This is the cleanest solution!!!


-- * inside transduce, NFST is lifted into a `runMany :: NFST f a b -> ProcessT (StateT ([a],[a],[b]) [] ()) a b` Process
-- * where (_,as',bs) = execState (runT $ source as ~> runMany nfst) ([],[],[])
-- * where as'' = as \\ as'
-- * and flip (,) as'' <$> bs to get type-approriate results of parsing


-- * note using `runMany' :: NFST f a b -> [a] -> StateT ([a],[a],[b]) [] ()` monad stack appear to do the job too
-- * here (_,as',bs) = execState (..) ([],[],[])
-- * and flip (,) as' <$> bs to get parser results

-- * Decision: does this make a decision on when to stop parsing the stream of as?
-- * unless the logic is in here somehow, it cannot stop since the entire stream is given
-- * using process, i can have something like stopP that `stop` on terminal condition???

-- * fundamental flaw, the entire input stream is Handed over to the scanner, so the division of labor
-- * is broken


-- * coroutine version of scannerT
-- * note this thingy also smells like leaky abstraction
scannerC :: (Applicative f, Monoid (f b)) => NFST f [a] b -> (a -> Bool) -> ProcessT (StateT [a] []) a (f b)
scannerC m = construct . plan m where 
  plan m p = do
    a <- await
    case p a of 
      True -> stop
      _    -> do 
        lift . modify $ (++[a])
        as <- lift get
        case step m (pure as) of      -- * ideally the parser should `map Just` onto stream before giving it to scanner
          [] -> plan m p
          rs -> do 
            (b,_,m') <- lift . lift $ rs
            lift . put $ []
            yield b
            plan m' p


sim'' :: ProcessT (StateT [Char] []) Char (Maybe FeatureMatrix)
sim'' = scannerC fst1 (=='#')

a1 = flip evalStateT [] $ runT $ source "phones# rest of sentence" ~> sim''


-- * StateT vs ProcessT
-- * `scannerC` does not see the whole stream, but `source as` does
-- * `scannerC` does not build the [b] on the fly, instead yielding it for later use
-- * `scannerC` require as \\ as' to get remaining stream, but `scannerT` returns it imediately
-- * `scannerC` could potentially become a util function from Transducer package, though use case seem very narrow

scannerT :: (Applicative f, Monoid (f b)) => NFST f [a] b -> (a -> Bool) -> [a] -> StateT ([a],[f b]) [] [a]
scannerT m p []     = return []
scannerT m p (x:xs) = do
  case p x of                     -- * this bit here belongs in parser
    True -> return xs
    _    -> do                    -- * rest of it seem to belong with transducer
      modify $ \(as,bs) -> (as <> pure x,bs)
      (as,_) <- get
      case step m (pure as) of 
        [] -> scannerT m p xs
        rs -> do 
          (b,_,m') <- lift rs
          modify $ \(_,bs) -> ([], (<>) <$> bs <*> [b] <||> bs <||> [b])
          scannerT m' p xs



sim' :: [([Char], ([Char], [Maybe FeatureMatrix]))]
sim' = flip runStateT ([],[]) $ scannerT fst1 (=='#') "phones# rest of sentence"  



type Buffer a = [a]
type Result f b = [f b]
type Remainder a = [a]


-- | similar to scanF below but type of output in list 
scannerT' :: (Applicative f, Monoid (f b)) => 
  NFST f [a] b  -> 
  (a -> Bool)   -> 
  [a]           -> 
  StateT [a] (StateT [f b] []) [a]
scannerT' m p []     = return []
scannerT' m p (x:xs) = do 
  case p x of 
    True -> return xs
    _    -> do 
      modify (<> pure x)
      as <- get
      case step m (pure as) of 
        [] -> scannerT' m p xs
        rs -> do 
          (b,_,m') <- lift . lift $ rs
          lift . modify $ \bs -> (<>) <$> bs <*> [b] <||> bs <||> [b]
          put []
          scannerT' m' p xs


sim2 :: [([Maybe FeatureMatrix], String)]
sim2 = [ (bs, as) | (as,bs) <- flip runStateT [] . flip evalStateT [] $ scannerT' fst1 (=='#') "phones# rest of sentence"]





---------- * find a better approximation of a good abstraction attemp 3, this could be the final attempt * -------------

                               
-- * note signature unecesarily complex and begs you to see its implementation
withAuto :: (Applicative f, Monoid (f b)) => NFST f [a] b -> (a -> Bool) -> Parser a (f b)
withAuto m p = P $ fmap flipTuple . flip runStateT mempty . flip evalStateT mempty . scanF m p 


-- | This is basically a non-composable, imperitive code hidden inside a "functional" parser
scanF :: (Applicative f, Monoid (f b)) => NFST f [a] b -> (a -> Bool) -> [a] -> StateT [a] (StateT (f b) []) [a]
scanF m p []     = return []
scanF m p (x:xs) = do 
  case p x of 
    True -> return xs
    _    -> do 
      modify (<> pure x)
      as <- get
      case step m (pure as) of 
        [] -> scanF m p xs
        rs -> do 
          (b,_,m') <- lift . lift $ rs
          lift . modify $ (<> b)
          put []
          scanF m' p xs

ppp = runP (withAuto fst1 (=='#')) "phones# working!!!"














---------- * find a better approximation of a good abstraction attemp 4, dice up the logic into combinators -----------

-- * Sets of logics in scanF:

  -- * logic of when to stop feeding chars into scanner
  -- * logic of accumulating buffer
  -- * logic of steping transducer
  -- * logic of checking outputs and directing flow of compuations
  -- * logic of multiple compuations
  -- * logic of accumulating outputs

-- * parser

  -- * logic of when to stop feeding into scanner
  -- * logic of checking outputs and directing flow of compuations


  -- * out: char
  -- * in: Done [b] | Await (char -> m Char b)

-- * some context automata is lifted into?
  -- * logic of accumulating buffer
  -- * logic of steping transducer
  -- * logic of multiple computations  
  -- * logic of accumulating outputs

  -- * out: Done [b] | some continuation value
  -- * in:  char | '#'

-- * goals:
  -- * make it so this combinator does not have to sit inside Parser.hs
  -- * some of this is not exactly coroutines?



-- * the layers
-- * parser that stops at char
-- * parser that runs w/ some coroutine? or run w/ transducer


-- * the generic problem: parser that runs w/ coroutine



-- * IDEA A.
-- * what if intermediately, dump as into a `source as ~> bufferAccumulator ~>` machine
-- * the machine then combines w/ some (context automata :: Process a b) brought into the parser

-- * does that mean... parser can take in a `Process [a] [b]` as param and thus decouple some logic
-- * but, the problem of getting a is just pushed upstream.


-- * IDEA B.
-- * fixed point of problem: getC is the ONLY way to get a character out of stream

-- * unless some other primitve type getC
{-

  only way to get char:

  scanner :: Process [a] b -> Parser a b 
  scanner p = do 
    c <- getC
    -- * some type here that flicker between get more char and finally Done [b]
    -- * observe unless p itself goes through transition, scanner cannot recurse with new version of p -> no in spirit of coroutines 

    
  the other fixed point: 

  assert this is the only sensible process context FST can be `lifted` into:

  autoT nounTransducer :: ProcessT [] (f a) (f b)



  The Problem:

  there needs to be some recursive construct that builds buffer incrementally

-}


--buffer :: String -> Process Char String

buffer :: [a] -> Process a [a]
buffer = construct . plan where
  plan xs = do 
    x <- await
    let xs' = xs <> pure x
    yield xs'
    plan xs'


-- * Challenges
  -- | HOW do you build source "phone" to begin with???  -> first priority
  -- | NEED bi-directional flow of information somehow to flush buffer

-- | it seems like source itself is the parser, which is NOT a coroutine instance

-- | note here the buffer is not flushed since there's no knowlege of downstream stuff
-- | `source` dummies a parser that is recursively `getC` and putting into buffer
-- | `buffer` is a construct in the parser `withCoroutine` as well
-- | `fst1` is brought in via parameter
try = source "phone" ~> (fmap Just $ buffer []) ~> (autoT fst1)


---------- * make recursive getc part -----------


-- * to keep it moving, write
-- * fixed point of thought: `c <- getC` line needs to be run multiple times
-- * fixed point of thought: the machine terminates when some signal is given???
-- * fiexed points of thought: the machine have to run once?
-- * or rather just runs forever and <> all results

-- * here m should be `autoT fst`, but note the machine itself does not change state, the transducer "inside" does
-- * but we shouldn't have to know about that
prelim m = do 
  c <- getC
  return ()


-- * what is the fundamental unit of work??? it should be: put char into buffer and run machine
-- * then can scale w/ some combinator that builds progressively larger buffers and run machine
-- * then add some clause that flushes buffer when `True`
-- * this construct runs forever


-- * Does the above sound like some sort of branching computation that itself could be modeled?





--------------------------- Assert Cannot implement bi-directional stuff with machines -----------------------------


-- * understand pipes' bidirectional stuff, can it do both ways w/i same construct??

-- * make parser a moand transformer

-- * roll own bidirectional coroutine stuff











{-

  this construct might provide a clue

  many1 :: Parser a b -> Parser a [b]
  many1 p = do 
    x  <- p 
    xs <- many p 
    return $ x:xs

-}

------------------------------- An Automata for testing ---------------------------------------------------

type FeatureMatrix = String

fstTable1 :: NFSTtable Maybe String FeatureMatrix
fstTable1 (I 0) (Just "phone") = [(Just "Root-Phone-", I 2)]
fstTable1 (I 2) (Just "s")     = [(Just "Plural-Noun", I 3),(Just "ThirdPerson-Verb", I 4)]
fstTable1 (I 2) (Just "#")     = [(Just "", F 1)]
fstTable1 (I 3) (Just "#")     = [(Just "", F 1)]
fstTable1 (I 4) (Just "#")     = [(Just "", F 1)]
fstTable1 _     _              = []

fst1 :: NFST Maybe String FeatureMatrix
fst1 = toNFST fstTable1


-- | this is an example of how the parser will feed a buffer into Process (fst1)
-- | note the buffer is "flushed" after "phone" runs successfully
test = runT $ (Just <$> source ["p", "ph", "pho", "phon", "phone", "s"]) ~> autoT fst1


r :: [Maybe FeatureMatrix]
r = foldr (<>) mempty <$> runNFST fstTable1 $ Just <$> ["phone", "s","#"]


r' = runNFST fstTable1 $ Just <$> ["phone", "s","#"]

-----------------------------------------------------------------------------------------------------
------------------------- Use Stride as crutch to model computation ----------------------------------
-----------------------------------------------------------------------------------------------------

--simple :: (Applicative f) => NFST f a b -> [a] -> StateT [b] [] ()
--simple :: NFST Maybe String FeatureMatrix -> [String] -> StateT [] [] FeatureMatrix
--simple m []     = return ()
--simple m (x:xs) = do
--  case step m (pure x) of
--    [] -> simple m xs
--    rs -> do 
--      (b,_,m') <- lift rs
--      modify (++[b])
--      simple m' xs


--sim = flip execStateT [] $ simple fst1 ["phone", "s", "#"]


-- | [b] is a local state that store intermediate results -> VERY VERY HACKY, what if there's 2 states? 3 states?
-- | note there's no reason to have [a] -> Stride a b right now

-- | could make this into a monad transformer that could have arb deep state monad stack, or any monad
data Stride a b = Partial [b] ([a] -> Stride a b) | Done [b]

-- | maybe extract [b] from Done
eDone :: Stride a b -> [b]
eDone (Done bs) = bs
eDone (Partial bs _) = bs 


instance (Show a, Show b) => Show (Stride a b) where
  show (Done bs)    = "Done " ++ show bs
  show (Partial bs f) = "Partial " ++ show bs ++ " <Stride>"

instance Functor (Stride a) where
  fmap g (Done bs)    = Done $ g <$> bs
  fmap g (Partial bs k) = Partial (g <$> bs) $ (fmap g) . k


-- | note, does this make sense? two threads of Stride a b now talk to each other essentially with mappend??!!!
instance Monoid (Stride a b) where
  mempty = Done []
  s1 `mappend` s2 = case (s1,s2) of
    (Done bs, Done cs)       -> Done $ bs ++ cs
    (Done bs, Partial cs k)    -> Partial (bs ++ cs) k   -- * this makes no sense! why would the threads combine?
    (Partial bs k, Done cs)    -> Partial (bs ++ cs) k   -- * same here 
    (Partial bs k, Partial cs g) -> Partial (bs ++ cs) $ \as -> (k `mappend` g) as  -- * even worse



-- | m b -> (b -> m c) -> m c
-- | (Done [b]) -> (b -> Done [c]) -> Done [c]

-- | Is this even a monad????
-- | does implementation of Partial make sense?

-- | left identity: return a >>= g  == g a    -> yes by def
-- | right identity:  m >>= return  == m      -> yes by cases check
-- | associativity: (m >>= f) >>= g == m >>= (\x -> f x >>= g)  -> check for all cases
instance Monad (Stride a) where
  return b = Done [b] 
  s >>= g  = case s of
    Done bs    -> ap g bs
    Partial bs k -> Partial (eDone $ ap g bs) ((>>=g) . k) 
    where ap g xs = foldr (<>) mempty $ g <$> xs


fm b = Done $ [b <> b, b <> b <> b]
gm b = Done $ [length b]

fm' b = Partial [b, b <> b] $ \as -> Done as
gm' b = Partial [length b] $ \as -> Done as



-- | the gist of what's going on here is that the `Parser` is the top routine, and we're dropping
-- | in a subroutine that's the suspension functor `Stride a b`

-- | Good: the [b] is already inside parser value constructor
-- | Bad: seems less pure and modular
-- | have to convert transducer into suspension functor somehow


-- | alternatively, can have both `Parser` and `Transducer` are routines lifted into some `Coroutine` monadic context

-- | Good: more pure impl of parser, thus more modular
-- | bad: how do I feed information back into parser?


-- | with takes a subroutine that transcribes a buffer of a to [b]
with :: Monoid b => ([a] -> Stride a b) -> Parser a b
with f = P $ \xs -> go f xs [] [] where 
  go _ [] _ bs      = [] `tuple` bs 
  go f (x:xs) as bs = let as' = as ++ [x] in case f as' of   
    (Done rs)     -> xs `tuple` (newr rs bs)
    (Partial rs' k) -> case rs' of 
      [] -> go k xs as' bs                                     
      _  -> go k xs [] $ newr rs' bs  
  newr rs bs  = (<>) <$> bs <*> rs <||> rs <||> bs
  tuple xs bs = flip (,) xs <$> bs

 
-- | Dummy transducer
-- | OBSERVE: this function is stateless
f :: String -> Stride Char String
f []     = Done []
f "run"  = Partial ["root-run-verb-"] f      
f "s"    = Partial ["3rdPerson-"] f           
f "ning" = Partial ["progressive-"] f           
f "#"    = Done ["#"]                 
f _      = Partial [] f                  


g :: String -> Stride Char String
g []       = Done []
g "phone"  = Partial ["root-phone-Noun"] g
g "s"      = Partial ["Plural-"] g 
g "#"      = Done ["#"]      
g _        = Partial [] g        


verb = with f
noun = with g

phrase = (<>) <$> noun <*> verb



------------------------- Now remove Explicity reference to Stride ----------------------------------

-- | proposal 1: change ([a] -> Stride a b) to (Monad m => [a] -> m a b)  
     -- this cannot work since there's explict pattern matching
{-

  this somewhat "object oriented", some monad m might keep local state or have any side effect


  Monad m => Stride m a b = Partial [a] -> Stride (m a b) | Done [b]

  but still: I need to pattern match on value constructor of Stride

  what if... remake the parser so it's a coroutine

  currently, parser :: [a] -> [(b,[a])]
  
  if instead, parser :: [a] -> Result a b where

  Result a b = 
    Done [(b,[a])] |  -- * mapped some sets of [a] to b
                      -- * yield some [a] to a coroutine, Partialing [b]

  in english:

  a parser either return a list of result [b]
  or a partially applied function that takes in a buffer [a] and yields result [b]

  question: what to do with intermediate results ? who is responsible for storing it

  choice: parser OR coroutine. decison: coroutine, other wise it'd be a subroutine


  advantage: consistent
  disadvantage: any function that goes inside parser has to know about Stride value constructor and obey it
      so leaky abstraction

  
-}



-- | proposal 3: have some sort of unified way to interface with it, make value constructor abstract 
-- | in other words, try again with machines or Monad.Coroutine

{-

  lift both parser and transducer into coroutine 

  assumption: know how to feed result of transducer back to parser. so the flow is

  [a] -> PARSER -> a -> NFST ..-> request | [b] -> PARSER 


  alternatively, parser just feeds chars down stream dumbly, except end of word when it feeds #

  downstream, transducer accumulate in buffer and run, taking products of results when possible

  finally when terminal condition hit, transducer output list of results... but to where????

    option a: back to parser, can't since it's not doable (that I know of)
    option b: downstream to somewhere else, but now conditional on this being true, how do you build up from this unit

    so that `phrase = P <$> noun <*> verb`

    does what I want? 

    Is this the question I should be asking??

  if I mocked some parser-lexer where verbPhrase = auto nounP ~> auto nounFST :: Process a b

  could this be used as an applicative?

  if I `run verbPhrase "door drops on me"` I get back .. ??

  I should get back [(NP (Noun "door" - Sg) (Verb "drop" - Third)), "on me"]

  
  Alternative way to frame it:

  convert "door drops on me" to `ws = ["door#", "drops#", "on#", "me#"] :: [String]`
      -- mini critique: this goes through the document twice, once to ++"#", once to parse-lex
      -- how is this even done? this should be done by parser!!!!

  then do `source ws ~> nounPhrase` is one task. Again ws could be arbitrarly long   -> WRONG

  
  CURRENT FLOW:

  "door drops one me"  ---to parser -> PARSER -> 'd', 'o', 'o', 'r', '#' --- to transducer -> TRANDUCER
  
  "d" -accum && run -> "do" -accum && run -> "doo" -accum && run -> "door" - accum && run --> "#" 
  --accum and final run -> ( "door#" -> NOUN "door" Pl) 

    what is lost is notion of continous parsing. Observe

  ```
  p1 <*> p2  = P $ \as -> do
    (g, as')  <- runP p1 as
    (x, as'') <- runP p2 as'
    return ( g x, as'' )

  ```

  question: what happens when m1 <*> m2 ? where `m1, m2 :: Process a b`

-}

type Lexer a b = ProcessT (State ([b], [a])) a ()

minip :: Process Char Char
minip = repeatedly $ do
  a <- await
  case isSpace a of
    True -> yield '#'
    _    -> yield a


minit :: (String -> [String]) -> ProcessT (State String) Char [String]
minit t = repeatedly $ do
  a <- await
  lift . modify $ (++[a])
  as <- lift get
  case t as of 
    [] -> yield []
    bs -> yield bs >> (lift . put $ [])
  return ()


minit2 :: (String -> [String]) -> Lexer Char String
minit2 t = repeatedly $ do
  a <- await
  modify $ \(rs,as) -> (rs,as++[a])
  (_,as') <- get
  case t as' of 
    [] -> return ()
    bs -> modify $ \(rs,_) -> ((<>) <$> rs <*> bs <||> rs <||> bs, [])
  return ()


hn :: String -> [String]
hn []       = []
hn "phone"  = ["Root-phone-Noun-"]
hn "s"      = ["Plural", "Sg"] 
hn "#"      = ["#"]      
hn _        = []

hv :: String -> [String]
hv []       = []
hv "run"  = ["Root-Run-Verb-"]
hv "s"    = ["Third-Person"]
hv "#"    = ["#"]      
hv _      = []


nounp = minip ~> minit2 hn :: Lexer Char String
verbp = minip ~> minit2 hv :: Lexer Char String


-- | partially replicated runP except runP :: [a] -> [(b,[a])]
-- | but this is [a] -> ([b], [a])
-- | now replicate typeclass interface? define instance of this using prelude stuff??
runParser :: Monoid b => Lexer a b -> [a] -> ([b],[a])
runParser p xs = flip execState ([],[]) . runT $ source xs ~> p




-- | poor man's non composable applicative
-- | utlimately, still best to have coF :: Parser a b -> Process a b -> Parser a b
verbPhr xs = ((<>) <$> ns <*> vs <||> ns <||> vs, xs'')
  where 
    (vs,xs'') = runParser verbp xs'
    (ns,xs') = runParser nounp xs 




-- | let (bs, as) = runParser p xs in (g <$> bs) 
mapP :: (b -> c) -> Lexer a b -> Lexer a c
mapP g p = undefined


-- | dummy (<*>)
-- | 
combo :: Monoid b => Lexer a b -> Lexer a b -> Lexer a b
combo l1 l2 = undefined


-- | at a high level: does it make sense to replicate a type with a specific instance of another type?
-- | at a practical level: can this be done using prelude instances?
-- | this appears to be a dead end unless there's some clever/unknown type-foo




-- | marginal utiltity over withf??? cant' think of any
withProcess :: Process a b -> Parser a b -> Parser a b
withProcess p = undefined






-- | dummy g <$> l1 <*> l2, combine <$> and <*> into one function
--prod :: Lexer a b -> Lexer a b -> Lexer a b
--prod = undefined


-- | to run this,like : minit h ~> combine 
-- | you need Zoom instance of lense thats's missing
-- | with that you can then drive it with source [1..6] ~> p1 _1 ~ p2 _2
--combine :: ProcessT (State [String]) [String] ()
--combine = repeatedly $ do
--  bs <- await
--  rs <- lift get
--  let rs' = (<>) <$> rs <*> bs <||> rs <||> bs
--  lift . put $ rs'
--  return ()




-------------------------------------- try withf again, using answer from SO -------------------------------------------












--------------------------------------------- shananigans to be deleted ---------------------------------------------
{-

  -- * put this here for now
  --import Transducer   
  --import Variants
  --import Type
  --import Data.Machine
  --import Control.Monad.Trans


  -- | Critique: two functions as params is getting out of hand
  -- | especially since first function is esetnially satisfy
  -- | accumulate a list of inputs until boolean condition p met, then runs function g that 
  -- | nondeterministically maps list of inputs to list of outputs
  untilP :: (a -> Bool) -> ([a] -> [b]) -> Parser a b
  untilP p g = P $ \as -> go as []
    where 
      go []     _  = []
      go (x:xs) ys = case p x of 
        False -> go xs $ ys ++ [x]
        True  -> flip (,) xs <$> g ys


  -- | now refactor everything out of here
  scanner :: Functor f => Monoid b => (a -> Bool) -> NFST f [a] b -> Parser a b
  scanner p m = P $ \as -> (\(b,as) -> (fromJust b, as)) <$> scan p m as [] []


  scan :: Functor f => Monoid b => (a -> Bool) -> NFST f [a] b -> [a] -> [a] -> [(Symbol b, [a])] -> [(Symbol b, [a])]
  scan _ _ []     _     bs = bs
  scan p m (a:as) chunk bs = case p a of 
    True -> bs
    _    -> let chunk' = chunk ++ [a] in case Stride m (pure chunk') of 
      [] -> scan p m as chunk' bs
      rs -> do
        (b,_,m') <- rs
        scan p m' as [] $ (\(a,_) -> (a <> b,as)) <$> bs <||> [(b,as)]


  -- | total cheat: just put g into parser, bascially a constructor
  -- | should just get HOF of type [a] -> [b]
  parseWith :: ([a] -> [(b,[a])]) -> Parser a b
  parseWith g = P $ \as -> g as


  pp :: Parser Char String
  pp = parseWith $ \as -> (\(a,bs) -> (fromJust a, bs)) <$> scan p m as [] [] 


  m2f :: NFSTtable Maybe String String
  m2f (I 0) (Just "_1") = [(Just "0_1 ", F 1),(Just "0_0", I 0)]
  m2f (I 0) (Just "_0") = [(Just "0_0 ", I 0),(Just "0_1", F 1)]
  m2f (F 1) (Just "_0") = [(Just "1_0 ", I 0)]
  m2f (F 1) (Just "_1") = [(Just "1_1 ", F 1)]
  m2f _ _               = []

  m :: NFST Maybe String String
  m = toNFST m2f 

  p :: Char -> Bool 
  p = (==) '#'

-}



-- | which logic belongs where?

-- | parser logic: popping a char from stream
-- | apply boolean predicate to char
-- | mapping remaining inputs onto results of comptation

-- | somewhow stacking predicates together?

-- | domain logic: a -> Bool, [a] -> [b]

-- | 
{-
  what the parser is doing:

  let a char through -> dump into coroutine -> pause
  -> get result from coroutine and interact w/ existing result -> resume -> let char through


  so process is either [a] -> [(b,[a])] or pause a or resume [b]   -- * initial parser design


  data Parstream a b = Failed String | Result [b] | Partial ([a] -> Parstream a b)  -- * morally equivalent to autoparser
    

  instance Functor (Parstream a b) where 
    fma _ Failed String = Failed String
    fmap g (Result bs) = Result $ g <$> bs
    fmap g (Partial f) = Partial $ g . f
  

  data ParserIncr = PI { runPI :: Parstream a b }       ??? what is this parser trying to do? 


  --- * Learn the process of thought with machines

  -- Note: A 'Machine' is usually constructed from 'Plan', so it does not need to be CPS'd.
  data Stride k o r
    = Stop
    | Yield o r
    | forall t. Partial (t -> r) (k t) r

  instance Functor (Stride k o) where
    fmap _ Stop = Stop
    fmap f (Yield o k) = Yield o (f k)
    fmap f (Partial g kg fg) = Partial (f . g) kg (f fg)

  -- | A 'MachineT' reads from a number of inputs and may yield results before stopping
  -- with monadic side-effects.
  newtype MachineT m k o = MachineT { runMachineT :: m (Stride k o (MachineT m k o)) }


-}




{-
want this:

problem: returns a process not a parser

see if we can still do  phrase = toPh <$> noun <*> verb :: Process String Morpheme

as opposed to Parser String Morpheme

source ["hello world"] ~> autoP p ~> autoT t

the way to make it into a parser is to have type: ParserT (ProcessT []) a b

so have to make parser a monad transformer

-}
{-

autoT :: NFST a b -> ProcessT [] (Symbol a) (Symbol b)
autoT = construct . plan where
    plan m = do 
        a <- Partial
        case Stride m a of
            []  -> stop
            xs  -> do
                (b,_,m') <- lift xs
                yield b
                plan m'
-}










































