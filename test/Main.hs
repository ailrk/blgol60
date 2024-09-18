{-# LANGUAGE OverloadedStrings #-}

module Main where

import AST (Stmt)
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Pretty.Simple (pTraceM)
import Parser qualified
import Paths_test (getDataFileName)
import System.IO
import Test.Hspec
import Text.Parsec qualified as Parsec
import Text.Pretty.Simple (pHPrint, pPrint, pPrintForceColor)


main :: IO ()
main = hspec $ do
  parserTest


parserTest :: Spec
parserTest = describe "parser tests" $ do
  testParser "data/test/1_adam.a60"
  testParser "data/test/1_comment.a60"

  where
    errorParsers = do
      testParser "data/test/1_eva.a60"
      testParser "data/test/2_adamar.a60"
      testParser "data/test/2_array.a60"
      testParser "data/test/2_outinteger.a60"
      testParser "data/test/3_inreal.a60"
      testParser "data/test/3_outreal.a60"
      testParser "data/test/4_for.a60"
      testParser "data/test/4_for2.a60"
      testParser "data/test/4_goto.a60"
      testParser "data/test/4_while.a60"
      testParser "data/test/5_twoBlocks.a60"
      testParser "data/test/5_typelessProc.a60"
      testParser "data/test/6_intProc.a60"
      testParser "data/test/6_realProc.a60"
      testParser "data/test/7_length.a60"
      testParser "data/test/7_outchar.a60"
      testParser "data/test/7_string.a60"
      testParser "data/test/7_terminator.a60"
      testParser "data/test/8_fileRead.a60"
      testParser "data/test/8_fileWrite.a60"

    hangParsers = do
      testParser "data/test/2_ininteger.a60"
      testParser "data/test/3_boolean.a60"
      testParser "data/test/4_if.a60"
      testParser "data/test/7_inchar.a60"



testParser :: FilePath -> Spec
testParser file = it ("should parse " ++ show file) $ do
  og <- Text.pack <$> (Paths_test.getDataFileName file >>= readFile)
  expected <- read @Stmt <$> (Paths_test.getDataFileName (file <> ".expect") >>= readFile)
  eParsed <- Parsec.runParserT Parser.program () "test parser" og

  case eParsed of
    Left err -> expectationFailure ("failed to parse " ++ show err)
    Right parsed -> expected `shouldBe` parsed
