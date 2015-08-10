{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- | A class for lax monoids
-----------------------------------------------------------------------------

module LaxMonoid where


import Control.Applicative


class (Applicative f) => (LaxMonid m, LaxMonid n) where
	lmempty  :: m
	lmappend :: f m -> f n -> f (m, n)
	lmconcat :: [f m] -> m
	lmconcat = foldr lmappend lmempty



---- | Note these should be moved elsewhere
--lmappend :: (Applicative f) => f a -> f b -> f (a, b)
--lmappend fa fb = (,) <$> fa <*> fb

--lzero :: (Applicative f) => f ()
--lzero = pure ()




--class LaxMonoid a where
--        lmempty  :: a
--        -- ^ Identity of 'mappend'
--        lmappend :: a -> a -> a
--        -- ^ An associative operation
--        lmconcat :: [a] -> a

--        -- ^ Fold a list using the monoid.
--        -- For most types, the default definition for 'mconcat' will be
--        -- used, but the function is included in the class definition so
--        -- that an optimized version can be provided for specific types.

--        mconcat = foldr mappend mempty

--infixr 6 <>

---- | An infix synonym for 'mappend'.
--(<>) :: Monoid m => m -> m -> m
--(<>) = mappend
--{-# INLINE (<>) #-}
