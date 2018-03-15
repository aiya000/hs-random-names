{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Expose newtypes of 'Text' to make program tokens
--
-- In this module,
-- We represent the very easy operational semantics on the documents with ↓
--
-- small = a..z
--
-- large = A..Z
--
-- num = 0..9
--
-- alpha = small | large
--
-- alphaNum = alpha | num
--
-- symbol = ! | " | # | $ | %
--        | & | \ | ( | ) | *
--        | + | | | ' | - | . | /
--        | : | ; | < | = | > | ? | @
--        | [ | \ | ] | ^ | _ | `
--        | { | | | } | ~
module System.Random.NameCase
  ( PascalName (..)
  , CamelName (..)
  , SignName (..)
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
import Data.Semigroup (Semigroup)
import Data.String (IsString)
import Data.Text (Text)
import System.Random (Random(..))
import System.Random.NameCase.Combinators
import Test.QuickCheck (Arbitrary(..))
import qualified Data.Text as T

-- |
-- Like "Konoko", "Sugar", and "Foo22OGA11RRR" at the 'Random' and 'Arbitrary' instance
--
-- this = large ':' {alphaNum}
newtype PascalName = PascalName
  { unPascalName :: Text
  } deriving (Show, Semigroup, IsString)

instance Bounded PascalName where
  minBound =
    let x  = unUpperChar minBound
        xs = T.unpack $ unText' minBound
    in PascalName $ T.pack (x:xs)
  maxBound =
    let x  = unUpperChar maxBound
        xs = T.unpack $ unText' maxBound
    in PascalName $ T.pack (x:xs)

instance Random PascalName where
  randomR (unPascalName >>> T.unpack -> "", PascalName y) gen
    = randomR (PascalName "A", PascalName y) gen
  randomR (PascalName x, unPascalName >>> T.unpack -> "") gen
    = randomR (PascalName x, PascalName "A") gen
  randomR (unPascalName >>> T.unpack -> (x:xs), unPascalName >>> T.unpack -> (y:ys)) gen =
    let (UpperChar z, nextGen) = randomR (UpperChar x, UpperChar y) gen
        (AlphaNums zs, nextGen') = randomR (AlphaNums xs, AlphaNums ys) nextGen
    in (PascalName $ T.pack (z:zs), nextGen')
  -- A below pattern is already covered by an above view pattern
  randomR (_, _) _ = error "fatal error: usually, this is not passed through (at Random PascalName)"
  random = randomR (minBound, maxBound)

instance Arbitrary PascalName where
  arbitrary = do
    UpperChar x <- arbitrary
    xs <- map unAlphaNumChar <$> arbitrary
    return . PascalName $ T.pack (x:xs)


-- |
-- Like "a", "abc" at the 'Arbitrary' 'Random' and instance
--
-- this = small ':' {alphaNum}
newtype CamelName = CamelName
  { unCamelName :: Text
  } deriving (Show, Semigroup, IsString)

instance Bounded CamelName where
  minBound =
    let x  = unLowerChar minBound
        xs = T.unpack $ unText' minBound
    in CamelName $ T.pack (x:xs)
  maxBound =
    let x  = unLowerChar maxBound
        xs = T.unpack $ unText' maxBound
    in CamelName $ T.pack (x:xs)

instance Random CamelName where
  randomR (unCamelName >>> T.unpack -> "", CamelName y) gen
    = randomR (CamelName "a", CamelName y) gen
  randomR (CamelName x, unCamelName >>> T.unpack -> "") gen
    = randomR (CamelName x, CamelName "a") gen
  randomR (unCamelName >>> T.unpack -> (x:xs), unCamelName >>> T.unpack -> (y:ys)) gen =
    let (LowerChar z, nextGen) = randomR (LowerChar x, LowerChar y) gen
        (AlphaNums zs, nextGen') = randomR (AlphaNums xs, AlphaNums ys) nextGen
    in (CamelName $ T.pack (z:zs), nextGen')
  -- A below pattern is already covered by an above view pattern
  randomR (_, _) _ = error "fatal error: usually, this is not passed through (at Random CamelName)"
  random = randomR (minBound, maxBound)

instance Arbitrary CamelName where
  arbitrary = do
    LowerChar x <- arbitrary
    xs <- map unAlphaNumChar <$> arbitrary
    return . CamelName $ T.pack (x:xs)


-- |
-- Like "<>", "<|>", and "<<<??>!!>!\@-~|" at the 'Random' and 'Arbitrary' instance
--
-- this = symbol ':' {symbol}
newtype SignName = SignName
  { unSignName :: Text
  } deriving (Show, Semigroup, IsString)

instance Bounded SignName where
  minBound =
    let x  = unSymbolChar minBound
        xs = T.unpack $ unText' minBound
    in SignName $ T.pack (x:xs)
  maxBound =
    let x  = unSymbolChar maxBound
        xs = T.unpack $ unText' maxBound
    in SignName $ T.pack (x:xs)

instance Random SignName where
  randomR (unSignName >>> T.unpack -> "", SignName y) gen
    = randomR (SignName "<", SignName y) gen
  randomR (SignName x, unSignName >>> T.unpack -> "") gen
    = randomR (SignName x, SignName ">") gen
  randomR (unSignName >>> T.unpack -> (x:xs), unSignName >>> T.unpack -> (y:ys)) gen =
    let (SymbolChar z, nextGen) = randomR (SymbolChar x, SymbolChar y) gen
        (Symbols zs, nextGen') = randomR (Symbols xs, Symbols ys) nextGen
    in (SignName $ T.pack (z:zs), nextGen')
  -- A below pattern is already covered by an above view pattern
  randomR (_, _) _ = errorOnUnexpected "Random SignName" "normally, this is not passed through"
  random = randomR (minBound, maxBound)

instance Arbitrary SignName where
  arbitrary = do
    SymbolChar x <- arbitrary
    xs <- map unSymbolChar <$> arbitrary
    return . SignName $ T.pack (x:xs)
