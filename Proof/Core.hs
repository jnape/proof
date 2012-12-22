module Proof.Core where

import Data.List (partition)
import Proof.Matcher
import Proof.Message.Format

type Passed = [String]
type Failed = [String]
type Test = [String]
type Suite = [[String]]

expect :: (Show actual, Show expected) =>  actual -> Matcher actual expected -> expected -> String
expect actual (failMessage, evaluator) expected
	| passed    = "Passed"
	| otherwise = "Failed: " ++ (format failMessage [("@expected@", show expected), ("@actual@", show actual)])
		where passed = evaluator actual expected

printResults :: (Passed, Failed) -> IO()
printResults (passed, failed) =
	let
		numPassed = length passed
		numFailed = length failed
	in
		(putStrLn . unlines) [
			if (length failed == 0) then "All tests passed.\n" else "There were test failures:\n\n" ++ unlines failed,
			"Passed:\t\t" ++ (show numPassed),
			"Failed:\t\t" ++ (show numFailed),
			"Tests Run:\t" ++ (show (numPassed + numFailed))
		]

runTests :: [String] -> IO()
runTests tests = (printResults . partition (=="Passed")) tests

runSuite :: Suite -> IO()
runSuite suite = (printResults.merge) $ map (partition (=="Passed")) suite
	where merge xs = foldl1 (\(p, f) (p', f') -> (p++p', f++f')) xs