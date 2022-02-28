{-# LANGUAGE OverloadedStrings #-}
module Test where

-- auxiliary testing module.

import           AST
import           Data.HashMap.Lazy  as HM
import           Data.Text          as T
import           Parser
import           Text.Parsec        as P
import           Text.Pretty.Simple

exprTest :: HM.HashMap Int ((T.Text -> IO ()), T.Text)
exprTest = HM.fromList
  [ (1, (runp variablep,"variable[1 + 3 * 2]"))
  , (2, (runp variablep, "variable"))
  , (4, (runp designationalExpressionp, "D[2 + 3 / 5]"))
  , (3, (runp arithmeticExpressionp, "1 + 3 * 2 / 3"))
  , (5, (runp booleanExpressionp, "if true and false then 1 + 3 * 2 else 4 < 10"))
  , (6, (runp stringp, "`some string here'"))
  , (7, (runp booleanExpressionp, "not a >= 3 or 1 < 5"))
  , (8, (runp booleanExpressionp, "if not b > 3 then c >= 2 else c < 4"))
  , (9, (runp expressionp, "foo(1, 2, b, if 1 + 3 > 10 then true else false)"))
  ]
  where
    runp p s = P.runPT p () "-" s >>= pPrint

runtest m k = let (action, s) = m HM.! k
               in do { putStrLn $ "=== " ++ show k ++ " ================";
                       putStrLn $ "  " ++ show s;
                       putStrLn "";
                       action s;
                       putStrLn "";
                     }

runAll :: HM.HashMap Int ((T.Text -> IO ()), T.Text) -> IO ()
runAll m = sequence_ [ runtest m k | (k, _) <- HM.toList m]
