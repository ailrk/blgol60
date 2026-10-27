module Main (main) where

import Algol60
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Text.IO qualified as Text.IO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (runParserT, errorBundlePretty)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inp, out] -> do
      src <- Text.IO.readFile inp
      ctx <- newContext
      asm <- runReaderT (runCompilerM (compile inp src)) ctx
      Text.IO.writeFile out asm
    _ -> putStrLn "usage: algol60 input.a60 output.s" >> exitFailure

compile :: FilePath -> _ -> CompilerM _
compile inp src = do
  parsed <- runParserT program inp src
  case parsed of
    Left err  -> liftIO (putStrLn (errorBundlePretty err) >> exitFailure)
    Right ast -> compileProgram ast
