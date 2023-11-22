module ParserTests where

import LoxParser
import LoxSyntax
import ParserLib qualified as P
import Test.QuickCheck as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

-- pretty :: (BP.PP a) => a -> String
-- pretty = PP.render . BP.pp

-- prop_roundtrip_val :: Value -> Bool
-- prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

-- prop_roundtrip_exp :: Expression -> Bool
-- prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

-- prop_roundtrip_stat :: Statement -> Bool
-- prop_roundtrip_stat s = P.parse statementP (pretty s) == Right s

-- qc :: IO ()
-- qc = do
--   putStrLn "roundtrip_val"
--   QC.quickCheck prop_roundtrip_val
--   putStrLn "roundtrip_exp"
--   QC.quickCheck prop_roundtrip_exp
--   putStrLn "roundtrip_stat"
--   QC.quickCheck prop_roundtrip_stat