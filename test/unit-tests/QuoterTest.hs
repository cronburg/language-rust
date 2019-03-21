{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module QuoterTest (quoterSuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Quote
import Language.Rust.Syntax
import Language.Rust.Data.Position (Span(..))
import Language.Rust.Parser (parse', inputStreamFromString)
import Language.Rust.Pretty (ResolveFail(..), pretty)

import Data.Functor ( ($>) )

makeInt :: Int -> Expr Span
makeInt s = parse' (inputStreamFromString $ show s)

rustStmt :: Int -> Stmt Span
rustStmt i = [stmt| let foo = ${ makeInt i }$; |]

mkMain :: Int -> Stmt () -> Test
mkMain i expect = testCase
  (case pretty expect of
    Left (ResolveFail _ msg) -> error msg
    Right expect' -> show expect')
  $ expect @=? (rustStmt i $> ())

quoterSuite :: Test
quoterSuite = testGroup "quoter suite"
  [ mkMain 5 $ Local (IdentP (ByValue Immutable) "foo" Nothing ()) Nothing (Just (Lit [] (Int Dec 5 Unsuffixed ()) ())) [] ()
  ]

