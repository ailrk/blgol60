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
import Text.Pretty.Simple (pHPrint, pPrint, pPrintForceColor)


main :: IO ()
main = print "test"
