import ParserTests qualified
import StepperTests qualified
import Test.HUnit (Assertion, Test (TestList), runTestTT, (~:), (~?=))
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Running parser unit tests..."
  ParserTests.test_all
  putStrLn "Running parser roundtrip tests..."
  ParserTests.roundtrip_qc
  putStrLn "Running stepper unit tests..."
  StepperTests.test_all
  putStrLn "Running stepper full programs tests..."
  StepperTests.test_programs
  putStrLn "Running stepper property tests..."
  StepperTests.qc
