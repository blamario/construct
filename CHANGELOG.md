# Revision history for construct

## 0.3.2 -- 2024-12-15

* Added combinators `manyTill` and `sequence`
* Slightly improved documentation

## 0.3.1.2 -- 2023-12-19

* Fixed Cabal warnings
* Increment the `bytestring`, `text`, `monoid-sublasses`, and `rank2classes` dependencies' upper bounds

## 0.3.1.1 -- 2022-10-03

* Increment the `monoid-sublasses` and `input-parsers` dependencies' upper bounds

## 0.3.1 -- 2022-03-25

* Increment the `text` dependency upper bound
* Move the Template Haskell splices so they compile with GHC 9

## 0.3.0.2 -- 2021-03-22

* Increment the attoparsec dependency upper bounds

## 0.3.0.1 -- 2021-03-07

* Increment the dependencies' upper bounds

## 0.3 -- 2020-07-18

* Import the Input[Char]Parser classes from the `input-parsers` package

## 0.2.0.1 -- 2020-03-08

* Incremented the upper bound for the `rank2classes` dependency

## 0.2 -- 2020-01-27

* Updated for `base-4.13`
* Eliminated the `Monoid/Semigroup (n s)` constraints
* Added `recordWith`
* Fixed the URI example and test
  * Constrained the URI format serialization to ByteString
  * Switched the URI parser to Attoparsec

## 0.1 -- 2020-01-22

* First version. Released on an unsuspecting world.
