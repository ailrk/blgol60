{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Pretty.Simple (pTraceM)
import Parser qualified
import Paths_test (getDataFileName)
import System.IO
import Test.Hspec (hspec, runIO)
import Text.Parsec qualified as Parsec
import Text.Pretty.Simple (pHPrint, pPrint, pPrintForceColor)


main :: IO ()
main = do
  testParser "data/test/1_adam.a60"
  testParser "data/test/1_comment.a60"
  -- n <- Parsec.runParserT Parser.comment () "test1" "comment asdljasl asd;"
  -- pHPrint stderr n
  -- testParser "data/test/1_eva.a60"
  -- testParser "data/test/2_adamar.a60"
  -- testParser "data/test/2_array.a60"
  -- testParser "data/test/2_ininteger.a60"
  -- testParser "data/test/2_outinteger.a60"
  -- testParser "data/test/3_boolean.a60"
  -- testParser "data/test/3_inreal.a60"
  -- testParser "data/test/3_outreal.a60"
  -- testParser "data/test/4_for.a60"
  -- testParser "data/test/4_for2.a60"
  -- testParser "data/test/4_goto.a60"
  -- testParser "data/test/4_if.a60"
  -- testParser "data/test/4_while.a60"
  -- testParser "data/test/5_twoBlocks.a60"
  -- testParser "data/test/5_typelessProc.a60"
  -- testParser "data/test/6_intProc.a60"
  -- testParser "data/test/6_realProc.a60"
  -- testParser "data/test/7_inchar.a60"
  -- testParser "data/test/7_length.a60"
  -- testParser "data/test/7_outchar.a60"
  -- testParser "data/test/7_string.a60"
  -- testParser "data/test/7_terminator.a60"
  -- testParser "data/test/8_fileRead.a60"
  -- testParser "data/test/8_fileWrite.a60"
  pure ()


testParser :: FilePath -> IO ()
testParser file = do
  f <- Text.pack <$> (Paths_test.getDataFileName file >>= readFile)
  n <- Parsec.runParserT Parser.program () "test1" f
  pTraceM file
  pTraceM (show n)
  pTraceM "\n"
  pure ()
