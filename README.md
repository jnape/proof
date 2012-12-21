Proof
=====

Unit Testing framework for Haskell that doesn't suck

Example Usage:

```haskell
module Main where

import Proof.Core
import Proof.Matcher

main :: IO ()
main = runTests
	[
		expect 2.3 toRoundTo 2,
		expect "a" toBe "a",
		expect 1 toRoundTo 1,
		expect 1 toBe 1,
		expect 3 toRoundTo 3,
		expect True toNotBe False
	]
```