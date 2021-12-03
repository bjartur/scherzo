{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Parser configuration management

module Alma.Parser.Config
    (readConfigFile)
where

import Data.Kind (Type)
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Tuple (swap)
import Dhall (inputFile, auto)

import Alma.Parser.CharTrie as CT

type ParserStrings :: Type
type ParserStrings = Map.HashMap T.Text T.Text

mapToCharTrie :: ParserStrings -> CT.CharTrie T.Text
mapToCharTrie = CT.fromList . map swap . Map.toList

readConfigFile :: FilePath -> IO (CT.CharTrie T.Text)
readConfigFile file = do
    val <- inputFile auto file
    pure $! mapToCharTrie val
