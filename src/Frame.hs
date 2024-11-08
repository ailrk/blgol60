{-# LANGUAGE FunctionalDependencies #-}

module Frame
  ( Frame(..)
  )
where

import Temp qualified
import Tree qualified


data Access
  = InFram Int
  | InReg Int
  deriving (Eq, Show)


type Register = Text


class Frame frame access | frame -> access, access -> frame where
  newFrame           :: Temp.Label -> [Bool] -> frame
  frameName          :: frame -> Temp.Label
  frameFormals       :: frame -> [access]
  frameAllocateLocal :: frame -> Bool -> access
  frameString        :: frame -> Tree.Label -> String -> String

  frameFP            :: frame -> Temp.Temp
  frameSP            :: frame -> Temp.Temp

  frameExpr          :: access -> Tree.EXPR -> Tree.EXPR

  frameRegisters     :: frame -> [Register]
  frameWordsize      :: frame -> Int

  callerSaves        :: frame -> [Temp.Temp]
  calleeSaves        :: frame -> [Temp.Temp]

  processEntryExit1  :: frame -> Tree.STMT -> Tree.STMT
