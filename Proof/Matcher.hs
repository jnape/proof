module Proof.Matcher where

type FailMessage = String
type Evaluator actual expected = actual -> expected -> Bool
type Matcher actual expected = (FailMessage, Evaluator actual expected)

toBe :: Eq a => Matcher a a
toBe = ("Expected <@actual@> to be <@expected@>", (==))

toNotBe :: Eq a => Matcher a a
toNotBe = ("Expected <@actual@> to not be <@expected@>", (/=))

toStartWith :: Eq a => Matcher [a] [a]
toStartWith = (
		"Expected @actual@ to start with @expected@",
		(\actual expected -> take (length expected) actual == expected)
	)

toEndWith :: Eq a => Matcher [a] [a]
toEndWith = (
		"Expected @actual@ to end with @expected@",
		(\actual expected -> drop ((length actual) - (length expected)) actual == expected)
	)

toBeLessThan :: Ord a => Matcher a a
toBeLessThan = ("Expected @actual@ to be < @expected@", (<))

toBeLessThanOrEqualTo :: Ord a => Matcher a a
toBeLessThanOrEqualTo = ("Expected @actual@ to be <= @expected@", (<=))

toBeGreaterThanOrEqualTo :: Ord a => Matcher a a
toBeGreaterThanOrEqualTo = ("Expected @actual@ to be >= @expected@", (>=))

toBeGreaterThan :: Ord a => Matcher a a
toBeGreaterThan = ("Expected @actual@ to be > @expected@", (>))

toRoundTo :: Matcher Float Int
toRoundTo = ("Expected <@actual@> to round to <@expected@>", (\actual expected -> (round actual) == expected))