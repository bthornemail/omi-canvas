{-# LANGUAGE NoImplicitPrelude #-}

module Entry where

import OMI.Kernel
import OMI.Core
import OMI.Relation
import OMI.Lisp
import OMI.Canvas

testInput :: [Byte]
testInput =
  byteLP : byteH : byteE : byteL : byteL : byteO : byteSpace :
  byteW : byteO : byteR : byteL : byteD : byteRP : []

byteH :: Byte
byteH = mkByte O I O O O I O O

byteE :: Byte
byteE = mkByte O I O O O I O I

byteL :: Byte
byteL = mkByte O I O O I I O O

byteO :: Byte
byteO = mkByte O I O O I I I I

byteW :: Byte
byteW = mkByte O I O O O I I O

byteR :: Byte
byteR = mkByte O I O O O O O O

byteD :: Byte
byteD = mkByte O I O O O O I O

parseResult :: [SExpr]
parseResult = parseBytes testInput
