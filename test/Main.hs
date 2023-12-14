import ParserTests qualified
import Test.HUnit (Assertion, Test (TestList), runTestTT, (~:), (~?=))
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Running parser unit tests..."
  ParserTests.test_all
  putStrLn "Running parser roundtrip tests..."
  ParserTests.roundtrip_qc
