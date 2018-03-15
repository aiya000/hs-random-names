{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Please see 'System.Random.NameCase'
module System.Random.NameCase.Combinators
  ( elementsR
  , errorOnUnexpected
  , VisibleChar (..)
  , visibleChars
  , UpperChar (..)
  , upperChars
  , LowerChar (..)
  , lowerChars
  , AlphaNumChar (..)
  , alphaNumChars
  , pattern AlphaNums
  , unAlphaNums
  , SymbolChar (..)
  , symbolChars
  , pattern Symbols
  , unSymbols
  , List' (..)
  , Text' (..)
  ) where

import Control.Arrow ((>>>))
import Data.Char (ord)
import Data.Text (Text)
import Safe (atMay)
import System.Random (Random(..), RandomGen)
import Test.QuickCheck (Arbitrary(..), elements)
import qualified Data.Text as T

-- |
-- Like 'Test.QuickCheck.elements' and System.Random.randomR'.
-- Get a element from `[a]`,
-- using a value of `'Random' Int`, the `(a, a)`, and the `a -> Int`.
--
-- `elementsR visibleChars ord (x, y)`
elementsR :: RandomGen g => [a] -> (a -> Int) -> (a, a) -> g -> (a, g)
elementsR zs trans (x, y) gen =
  let (i, nextGen) = random gen
      index = abs (i + trans x + trans y) `mod` length zs
  in case zs `atMay` index of
          Nothing -> errorOnUnexpected "elementsR" $ "the access is broken with the index " ++ show index
          Just z  -> (z, nextGen)

-- |
-- Break the program.
-- Notify that an error on unexpected conditions.
errorOnUnexpected :: String -> String -> a
errorOnUnexpected hint msg = error $ "fatal error! (" ++ hint ++ "), an unexpected condition is detected: " ++ msg

random3 :: (Random a, Random b, Random c, RandomGen g) => g -> (a, b, c)
random3 g = 
  let (x, h) = random g
      (y, i) = random h
      (z, _) = random i
  in (x, y, z)


-- |
-- Similar to 'VisibleChar,
-- but only visible and ascii characters are allowed
-- in the 'Random' and the 'Arbitrary' instances
newtype VisibleChar = VisibleChar
  { unVisibleChar :: Char
  } deriving (Show)

-- | All of 'VisibleChar' (the table)
visibleChars :: [VisibleChar]
visibleChars = map VisibleChar
  [ '!' , '"' , '#' , '$' , '%'
  , '&' , '\'' , '(' , ')' , '*'
  , '+' , ',' , '-' , '.' , '/'
  , '0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8' , '9'
  , ':' , ';' , '<' , '=' , '>' , '?' , '@'
  , 'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' , 'H' , 'I' , 'J' , 'K' , 'L' , 'M' , 'N' , 'O' , 'P' , 'Q' , 'R' , 'S' , 'T' , 'U' , 'V' , 'W' , 'X' , 'Y' , 'Z'
  , '[' , '\\' , ']' , '^' , '_' , '`'
  , 'a' , 'b' , 'c' , 'd' , 'e' , 'f' , 'g' , 'h' , 'i' , 'j' , 'k' , 'l' , 'm' , 'n' , 'o' , 'p' , 'q' , 'r' , 's' , 't' , 'u' , 'v' , 'w' , 'x' , 'y' , 'z'
  , '{' , '|' , '}' , '~'
  ]

instance Bounded VisibleChar where
  minBound = head visibleChars
  maxBound = last visibleChars

instance Random VisibleChar where
  randomR = elementsR visibleChars $ ord . unVisibleChar
  random  = randomR (minBound, maxBound)

instance Arbitrary VisibleChar where
  arbitrary = elements visibleChars


-- |
-- A..Z at the 'Random' and 'Arbitrary' instance
-- (a subset of 'VisibleChar')
newtype UpperChar = UpperChar
  { unUpperChar :: Char
  } deriving (Show)

-- | A subset of 'visibleChars'
upperChars :: [UpperChar]
upperChars = map UpperChar
  [ 'A' , 'B' , 'C' , 'D' , 'E'
  , 'F' , 'G' , 'H' , 'I' , 'J'
  , 'K' , 'L' , 'M' , 'N' , 'O'
  , 'P' , 'Q' , 'R' , 'S' , 'T'
  , 'U' , 'V' , 'W' , 'X' , 'Y'
  , 'Z'
  ]

instance Bounded UpperChar where
  minBound = head upperChars
  maxBound = last upperChars

instance Random UpperChar where
  randomR = elementsR upperChars $ ord . unUpperChar
  random  = randomR (minBound, maxBound)

instance Arbitrary UpperChar where
  arbitrary = elements upperChars


-- | 'a'..'z' at the 'Random' and 'Arbitrary' instance
newtype LowerChar = LowerChar
  { unLowerChar :: Char
  } deriving (Show)

-- | A subset of 'visibleChars'
lowerChars :: [LowerChar]
lowerChars = map LowerChar
  [ 'a' , 'b' , 'c' , 'd' , 'e'
  , 'f' , 'g' , 'h' , 'i' , 'j'
  , 'k' , 'l' , 'm' , 'n' , 'o'
  , 'p' , 'q' , 'r' , 's' , 't'
  , 'u' , 'v' , 'w' , 'x' , 'y'
  , 'z'
  ]

instance Bounded LowerChar where
  minBound = head lowerChars
  maxBound = last lowerChars

instance Random LowerChar where
  randomR = elementsR lowerChars $ ord . unLowerChar
  random  = randomR (minBound, maxBound)

instance Arbitrary LowerChar where
  arbitrary = elements lowerChars


-- | A..Z, a..z, or 0..9 at the 'Random' and 'Arbitrary' instance
newtype AlphaNumChar = AlphaNumChar
  { unAlphaNumChar :: Char
  } deriving (Show)

-- | A subset of 'visibleChars'
alphaNumChars :: [AlphaNumChar]
alphaNumChars = map AlphaNumChar
  [ '0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8' , '9'
  , 'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' , 'H' , 'I' , 'J' , 'K' , 'L' , 'M' , 'N' , 'O' , 'P' , 'Q' , 'R' , 'S' , 'T' , 'U' , 'V' , 'W' , 'X' , 'Y' , 'Z'
  , 'a' , 'b' , 'c' , 'd' , 'e' , 'f' , 'g' , 'h' , 'i' , 'j' , 'k' , 'l' , 'm' , 'n' , 'o' , 'p' , 'q' , 'r' , 's' , 't' , 'u' , 'v' , 'w' , 'x' , 'y' , 'z'
  ]

instance Bounded AlphaNumChar where
  minBound = head alphaNumChars
  maxBound = last alphaNumChars

instance Random AlphaNumChar where
  randomR = elementsR alphaNumChars $ ord . unAlphaNumChar
  random  = randomR (minBound, maxBound)

instance Arbitrary AlphaNumChar where
  arbitrary = elements alphaNumChars

{-@ AlphaNums :: {xs : [Char] | all isAlphaNum xs} -> List' AlphaNumChar @-}
-- |
-- >>> xs = AlphaNums ['A', 'b', '3']
-- >>> :t xs
-- List' AlphaNumChar
--
-- >>> AlphaNums xs' = xs
-- >>> :t xs'
-- [Char]
pattern AlphaNums :: [Char] -> List' AlphaNumChar
pattern AlphaNums xs <- (unAlphaNums -> xs)
  where
    AlphaNums xs = List' $ fmap AlphaNumChar xs

{-@ unAlphaNums :: List' AlphaNumChar -> {xs : [Char] | all isAlphaNum xs} @-}
-- |
-- The dual of 'AlphaNums',
-- This is imposed the same restriction of 'AlphaNums' to the return type
unAlphaNums :: List' AlphaNumChar -> [Char]
unAlphaNums = unList' . fmap unAlphaNumChar


-- | symbolic characters at the 'Random' and 'Arbitrary' instance
newtype SymbolChar = SymbolChar
  { unSymbolChar :: Char
  } deriving (Show)

-- | A subset of 'visibleChars'
symbolChars :: [SymbolChar]
symbolChars = map SymbolChar
  [ '!' , '"' , '#' , '$' , '%'
  , '&' , '\'' , '(' , ')' , '*'
  , '+' , ',' , '-' , '.' , '/'
  , ':' , ';' , '<' , '=' , '>' , '?' , '@'
  , '[' , '\\' , ']' , '^' , '_' , '`'
  , '{' , '|' , '}' , '~'
  ]

instance Bounded SymbolChar where
  minBound = head symbolChars
  maxBound = last symbolChars

instance Random SymbolChar where
  randomR = elementsR symbolChars $ ord . unSymbolChar
  random  = randomR (minBound, maxBound)

instance Arbitrary SymbolChar where
  arbitrary = elements symbolChars

-- | Similar to 'AlphaNums'
pattern Symbols :: [Char] -> List' SymbolChar
pattern Symbols xs <- (unSymbols -> xs)
  where
    Symbols xs = List' $ fmap SymbolChar xs

unSymbols :: List' SymbolChar -> [Char]
unSymbols = unList' . fmap unSymbolChar


-- | A workaround for the 'Random' orphan instance
newtype List' a = List'
  { unList' :: [a]
  } deriving (Functor)

instance (Bounded a, Random a) => Random (List' a) where
  -- |
  -- The random list.
  -- But the length is under the 16,
  -- because it is expected to use as identifiers (e.g. 'PascalName', 'CamelName')
  randomR :: forall g. RandomGen g => (List' a, List' a) -> g -> (List' a, g)
  randomR (List' xs, List' ys) gen =
    let (i, j, k) = random3 gen
        (i', j', k') = (i `mod` length xs, j `mod` length ys, k `mod` 16)
        (x, y) = (xs `atMay` i', ys `atMay` j')
        (_, nextGen) = random gen :: (Int, g)
    in case (x, y) of
            (Nothing, _) -> errorOnUnexpected "Random (List' a)" $ "the access is broken with the index " ++ show i'
            (_, Nothing) -> errorOnUnexpected "Random (List' a)" $ "the access is broken with the index " ++ show j'
            (Just x', Just y') -> (List' . take k' $ randomRs (x', y') gen, nextGen)
  random = randomR (minBound, maxBound)

instance Bounded a => Bounded (List' a) where
  -- | The singleton with 'minBound' of `a`
  minBound = List' [minBound]
  -- | The "finite" 'List'' with 'maxBound's of `a` (size 10000)
  maxBound = List' $ replicate limitedSize maxBound
    where
      -- In 'Test.QuickCheck.IdTags' module, I may want finite lists
      limitedSize = 10000


-- | A workaround for the 'Random' orphan instance
newtype Text' = Text'
  { unText' :: Text
  }

instance Bounded Text' where
  minBound =
    let List' xs = minBound
    in Text' $ T.pack xs
  maxBound =
    let List' xs = maxBound
    in Text' $ T.pack xs

instance Random Text' where
  randomR (unText' >>> T.unpack -> x, unText' >>> T.unpack -> y) gen =
    let (List' str, nextGen) = randomR (List' x, List' y) gen
    in (Text' $ T.pack str, nextGen)
  random = randomR (minBound, maxBound)
