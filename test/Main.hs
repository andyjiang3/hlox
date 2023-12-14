import ParserTests
import Test.HUnit (Assertion, Test (TestList), runTestTT, (~:), (~?=))
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Running roundtrip tests..."
  roundtripQc
