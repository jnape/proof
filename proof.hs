module Proof where

import Data.List (partition)

type Evaluator a = (a -> a -> Bool)
type Expectation a = (Evaluator a, a)
data Test a = Expect a (Expectation a)
type Suite a = [Test a]
type Passed = [String]
type Failed = [String]

to :: Evaluator a -> a -> Expectation a
to evaluator expected = (evaluator, expected)

be :: Eq a => a -> a -> Bool
be = (==)

notBe :: Eq a => a -> a -> Bool
notBe = (/=)

toBe :: Eq a => a -> Expectation a
toBe expected = to be expected

toNotBe :: Eq a => a -> Expectation a
toNotBe expected = to notBe expected

toBeFalse :: Expectation Bool
toBeFalse = toBe False

toBeTrue :: Expectation Bool
toBeTrue = toBe True

runTest :: Show a => Test a -> String
runTest (Expect actual (evaluator, expected))
	| evaluator expected actual = "Passed"
	| otherwise                 = "Failed: expected <" ++ (show expected) ++ "> but was <" ++ (show actual) ++ ">"

runSuite :: Show a => Suite a -> String
runSuite suite = printResults $ partition passed $ map runTest suite
	where passed result = result == "Passed"

printResults :: (Passed, Failed) -> String
printResults (passed, failed) =
	let
		numPassed = length passed
		numFailed = length failed
	in
		unlines [
			unlines failed,
			"Tests Run:\t" ++ (show (numPassed + numFailed)),
			"Passed:\t\t" ++ (show numPassed),
			"Failed:\t\t" ++ (show numFailed)
		]