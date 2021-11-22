{-# LANGUAGE OverloadedStrings #-}

module Main
    (main)
where

import Alma.Parser
import Data.ByteString as BS
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Text.Megaparsec

main :: IO ()
main = do
    T.putStrLn "Type thine program."
    input <- decodeUtf8 <$> BS.getContents
    parseTest parseMusic input
