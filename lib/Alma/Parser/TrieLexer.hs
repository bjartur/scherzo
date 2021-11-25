{-# LANGUAGE MultiWayIf #-}

-- | Parse a music token using a CharTrie

module Alma.Parser.TrieLexer
    (parseWithTrie)
where

import Control.Applicative
import Data.List.NonEmpty qualified as NL
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Text.Megaparsec

import Alma.Parser.CharTrie as CT
import Alma.Parser.Types

parseWithTrie :: forall a. CT.CharTrie a -> Parser a
{-# INLINEABLE parseWithTrie #-}
parseWithTrie !trie
    | CT.null trie = error $! "Programming error: Cannot parse with no tokens"
    | otherwise = go Nothing trie
  where
    go :: Maybe a -> CT.CharTrie a -> Parser a
    {-# INLINEABLE go #-}
    go !val !t = do
        ch <- lookAhead anySingle
        case CT.lookup ch t of
            Just (nextVal, nextTrie) ->
                anySingle *> go (nextVal <|> val) nextTrie
            Nothing ->
                case val of
                    Just result -> pure $! result
                    Nothing -> errorOut ch t
    errorOut :: Char -> CT.CharTrie a -> Parser a
    {-# INLINEABLE errorOut #-}
    errorOut !ch !t
        = failure (Just $! Tokens $! ch :| []) expectedTokens
      where
        expectedTokens :: Set.Set (ErrorItem Char)
        {-# INLINEABLE expectedTokens #-}
        expectedTokens = if
            | CT.null t -> error $! "Programming error: Dead end in parsing trie"
            | otherwise -> Set.singleton $! Tokens $! NL.fromList $! CT.keys t
