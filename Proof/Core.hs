module Proof.Core where

import Data.List (partition)

type Passed = [String]
type Failed = [String]
type Test = [String]
type Suite = [[String]]
type Evaluator a b = a -> b -> Bool
type FailMessage = String
type Matcher a b = (FailMessage, Evaluator a b)

expect :: (Show expected, Show actual) =>  actual -> Matcher expected actual -> expected -> String
expect actual (failMessage, evaluator) expected
	| passed    = "Passed"
	| otherwise = "Failed: " ++ (format failMessage [("@expected@", show expected), ("@actual@", show actual)])
		where passed = evaluator expected actual

toRoundTo :: Matcher Int Float
toRoundTo = ("Expected <@actual@> to round to <@expected@>", (\expected actual -> (round actual) == expected))

toBe :: Eq a => Matcher a a
toBe = ("Expected <@actual@> to be <@expected@>", (==))

toNotBe :: Eq a => Matcher a a
toNotBe = ("Expected <@actual@> to not be <@expected@>", (/=))

printResults :: (Passed, Failed) -> IO()
printResults (passed, failed) =
	let
		numPassed = length passed
		numFailed = length failed
	in
		(putStrLn . unlines) [
			unlines failed,
			"Tests Run:\t" ++ (show (numPassed + numFailed)),
			"Passed:\t\t" ++ (show numPassed),
			"Failed:\t\t" ++ (show numFailed)
		]

runTests :: [String] -> IO()
runTests tests = (printResults . partition (=="Passed")) tests

runSuite :: Suite -> IO()
runSuite suite = (printResults.merge) $ map (partition (=="Passed")) suite
	where merge xs = foldl1 (\(p, f) (p', f') -> (p++p', f++f')) xs

type Substitution = (String, String)

format :: String -> [Substitution] -> String
format ""    _ = ""
format text [] = text
format text (x:xs) = format (substitute text x) xs
	where
		substitute ""    _ = ""
		substitute text substitution@(find, repl)
			| text `startsWith` find = repl ++ substitute (drop (length find) text) substitution
			| otherwise              = (head text) : substitute (tail text) substitution
				where text `startsWith` find = take (length find) text == find

main :: IO ()
main = 
	runSuite
	[
		[
			expect 2.3 toRoundTo 2,
			expect "a" toBe "a",
			expect 1 toRoundTo 1,
			expect 1 toBe 1,
			expect 3 toRoundTo 3,
			expect True toNotBe False
		]
	]