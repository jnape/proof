module Proof.Matcher (
	FailMessage, Evaluator, Matcher,

	toBe,
	toNotBe,
	
	toRoundTo
) where

type FailMessage = String
type Evaluator a b = a -> b -> Bool
type Matcher a b = (FailMessage, Evaluator a b)

toBe :: Eq a => Matcher a a
toBe = ("Expected <@actual@> to be <@expected@>", (==))

toNotBe :: Eq a => Matcher a a
toNotBe = ("Expected <@actual@> to not be <@expected@>", (/=))

toRoundTo :: Matcher Int Float
toRoundTo = ("Expected <@actual@> to round to <@expected@>", (\expected actual -> (round actual) == expected))