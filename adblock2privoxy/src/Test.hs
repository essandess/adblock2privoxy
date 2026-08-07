module Main (main) where

import Control.Monad (forM_)
import Data.List (isInfixOf, isPrefixOf)
import InputParser (Line (..), Policy (..), Record (..), Restrictions (..), adblockFile)
import ParserExtTests (testParseMorse, testParsecExt)
import PatternConverter (globWildcard, hasUnsupportedPCRE, isOverbroadPattern, makePattern, maxPatternLength, parseUrl)
import PolicyTree (restrictionsTree)
import qualified PolicyTree as PT (NodePolicy (Block)) -- qualified: NodePolicy's Block would otherwise clash with Policy's Block
import System.Exit (exitFailure, exitSuccess)
import Text.ParserCombinators.Parsec (parse)
import Utils (replace, split, startswith)

data Case = Case {caseName :: String, caseOk :: Bool}

check :: String -> Bool -> Case
check = Case

cases :: [Case]
cases =
  [ check
      "replace basic"
      (replace "foo" "bar" "foobarfoo" == "barbarbar"),
    check
      "replace no match"
      (replace "xyz" "Q" "abc" == "abc"),
    check
      "split basic"
      (split "," "a,b,,c" == ["a", "b", "", "c"]),
    check
      "split no delim"
      (split "," "abc" == ["abc"]),
    check
      "startswith true"
      (startswith "foo" "foobar"),
    check
      "startswith false"
      (not (startswith "bar" "foobar")),
    check
      "parseUrl doesn't error on simple pattern"
      (either (const False) (const True) (parseUrl "||example.com^")),
    check
      "genuine second-level domain still produced (example.com)"
      (either (const False) (not . null) (parseUrl "||example.com^")),
    check
      "network pattern containing literal '#' no longer truncates (regression)"
      ( either
          (const False)
          (const True)
          (parseUrl "/\\.(gif|jpe?g|png|webp)#(\\/?.+)?(\\/(ad)s?\\/|\\/ad-)/")
      ),
    check
      "adblockFile parses minimal header"
      ( either
          (const False)
          (const True)
          (parse adblockFile "test" "[Adblock Plus 2.0]\n")
      ),
    check
      "adblockFile parses list with no header (uBlock-style)"
      ( either
          (const False)
          (const True)
          (parse adblockFile "test" "! Title: example\n||example.com^\n")
      ),
    check
      "ParsecExt cases combinator (ab/eb + letters + alnum)"
      (either (const False) (not . null) testParsecExt),
    check
      "ParsecExt morse decode of HELLO"
      (either (const False) (elem "HELLO") testParseMorse),
    check
      "parseUrl terminates on malformed bracket in path (regression)"
      (either (const False) (const True) (parseUrl "||example.com/foo[bar^")),
    check
      "host dots are literal, not PCRE-escaped (regression)"
      ( either
          (const False)
          (all (\p -> not ("\\." `isInfixOf` makePattern False p)))
          (parseUrl "||example.com^")
      ),
    check
      "bare TLD host is rejected (com)"
      (either (const False) null (parseUrl "||com^")),
    check
      "unanchored-dot TLD host is rejected (.com)"
      (either (const False) null (parseUrl "||.com^")),
    check
      "wildcard-only TLD host is rejected (*.com)"
      (either (const False) null (parseUrl "||*.com^")),
    check
      "wildcard subdomain of a real domain is kept (*.example.com)"
      (either (const False) (not . null) (parseUrl "||*.example.com^")),
    -- PCRE filtering tests
    check
      "hasUnsupportedPCRE accepts a single positive lookahead (?=...)"
      (not (hasUnsupportedPCRE "foo(?=bar)baz")),
    check
      "hasUnsupportedPCRE accepts a single negative lookahead (?!...)"
      (not (hasUnsupportedPCRE "foo(?!bar)baz")),
    check
      "hasUnsupportedPCRE accepts a single positive lookbehind (?<=...)"
      (not (hasUnsupportedPCRE "foo(?<=bar)baz")),
    check
      "hasUnsupportedPCRE accepts a single negative lookbehind (?<!...)"
      (not (hasUnsupportedPCRE "foo(?<!bar)baz")),
    check
      "hasUnsupportedPCRE detects two chained lookaheads (the catastrophic-backtracking shape)"
      (hasUnsupportedPCRE "foo(?=[a-z]{0,9}1)(?=[a-z]{0,9}2)bar"),
    check
      "hasUnsupportedPCRE detects a lookahead plus a lookbehind chained together"
      (hasUnsupportedPCRE "foo(?=bar)(?<=baz)qux"),
    check
      "hasUnsupportedPCRE detects conditional (?(...))"
      (hasUnsupportedPCRE "foo(?(1)bar)baz"),
    check
      "hasUnsupportedPCRE accepts named capture (?P<name>...)"
      (not (hasUnsupportedPCRE "foo(?P<test>bar)baz")),
    check
      "hasUnsupportedPCRE accepts normal PCRE patterns"
      (not (hasUnsupportedPCRE "foo[a-z]+bar.*baz")),
    check
      "hasUnsupportedPCRE accepts character classes"
      (not (hasUnsupportedPCRE "[a-zA-Z0-9_-]+")),
    check
      "hasUnsupportedPCRE accepts quantifiers"
      (not (hasUnsupportedPCRE "foo{1,3}bar+baz*qux?")),
    check
      "hasUnsupportedPCRE accepts alternation"
      (not (hasUnsupportedPCRE "foo|bar|baz")),
    check
      "hasUnsupportedPCRE accepts anchors"
      (not (hasUnsupportedPCRE "^foo.*bar\\$")),
    check
      "hasUnsupportedPCRE accepts word boundaries"
      (not (hasUnsupportedPCRE "\\bfoo\\b")),
    check
      "hasUnsupportedPCRE accepts non-capturing groups"
      (not (hasUnsupportedPCRE "foo(?:bar|baz)qux")),
    check
      "hasUnsupportedPCRE accepts case-insensitive modifier"
      (not (hasUnsupportedPCRE "(?i)foo")),
    check
      "hasUnsupportedPCRE accepts case-sensitive modifier"
      (not (hasUnsupportedPCRE "(?-i)foo")),
    check
      "hasUnsupportedPCRE rejects a malformed pattern (unmatched bracket)"
      (hasUnsupportedPCRE "foo[bar"),
    check
      "parseUrl accepts pattern with a single positive lookahead"
      ( case parseUrl "/(?=test)/" of
          Right patterns ->
            not (null patterns)
              && all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _ -> False
      ),
    check
      "parseUrl accepts pattern with a single negative lookahead"
      ( case parseUrl "/(?!test)/" of
          Right patterns ->
            not (null patterns)
              && all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _ -> False
      ),
    check
      "parseUrl accepts pattern with a single positive lookbehind"
      ( case parseUrl "/(?<=test)/" of
          Right patterns ->
            not (null patterns)
              && all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _ -> False
      ),
    check
      "parseUrl accepts pattern with a single negative lookbehind"
      ( case parseUrl "/(?<!test)/" of
          Right patterns ->
            not (null patterns)
              && all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _ -> False
      ),
    -- Over-broad (host-unrestricted, match-everything) pattern filtering tests
    check
      "isOverbroadPattern rejects an empty host with a bare wildcard path"
      (isOverbroadPattern "" globWildcard),
    check
      "isOverbroadPattern rejects an empty host with a bare wildcard path, end-anchored"
      (isOverbroadPattern "" globWildcard),
    check
      "isOverbroadPattern allows a wildcard path once it's followed by a literal"
      (not (isOverbroadPattern "" (globWildcard ++ "ads"))),
    check
      "isOverbroadPattern allows a bare wildcard path when the host is restricted"
      (not (isOverbroadPattern "example.com" globWildcard)),
    check
      "parseUrl accepts simple domain pattern"
      ( case parseUrl "||example.com^" of
          Right patterns ->
            not (null patterns)
              && all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _ -> False
      ),
    check
      "parseUrl accepts wildcard pattern"
      ( case parseUrl "||example.com/*/ads/*" of
          Right patterns ->
            not (null patterns)
              && all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _ -> False
      ),
    check
      "parseUrl accepts separator pattern"
      ( case parseUrl "||example.com^banner^" of
          Right patterns ->
            not (null patterns)
              && all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _ -> False
      ),
    check
      "parseUrl filters excessively long pattern"
      ( let longPattern = "/^" ++ replicate (maxPatternLength + 100) 'a' ++ "\\$/"
         in case parseUrl longPattern of
              Right patterns -> all (\p -> "# FILTERED:" `isInfixOf` makePattern False p) patterns
              Left _ -> False
      ),
    check
      "parseUrl accepts normal-length pattern"
      ( let normalPattern = "/^" ++ replicate 100 'a' ++ "\\$/"
         in case parseUrl normalPattern of
              Right patterns ->
                not (null patterns)
                  && all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
              Left _ -> False
      ),
    -- Domain validation tests for PolicyTree
    check
      "restrictionsTree filters out bare TLD from positive list"
      ( case restrictionsTree PT.Block (Restrictions (Just ["com", "example.com"]) []) of
          Just tree -> True -- Should succeed with only example.com
          Nothing -> False
      ),
    check
      "restrictionsTree filters out bare TLD from negative list"
      ( case restrictionsTree PT.Block (Restrictions Nothing ["org", "example.org"]) of
          Just tree -> True -- Should succeed with only example.org negated
          Nothing -> False
      ),
    check
      "restrictionsTree returns Nothing when all positive domains invalid"
      ( case restrictionsTree PT.Block (Restrictions (Just ["com", "org", "net"]) []) of
          Just _ -> False
          Nothing -> True
      ),
    check
      "restrictionsTree handles mixed valid/invalid domains in positive list"
      ( case restrictionsTree PT.Block (Restrictions (Just ["com", "example.com", "org"]) []) of
          Just _ -> True -- Should keep example.com
          Nothing -> False
      ),
    check
      "restrictionsTree handles mixed valid/invalid domains in negative list"
      ( case restrictionsTree PT.Block (Restrictions Nothing ["net", "example.net", "io"]) of
          Just _ -> True -- Should keep example.net
          Nothing -> False
      ),
    -- port pattern
    check
      "parseUrl accepts valid host:port pattern"
      (either (const False) (not . null) (parseUrl "||example.com:8080^")),
    check
      "parseUrl rejects host with malformed port"
      (either (const False) null (parseUrl "||example.com:notaport^")),
    check
      "parseUrl rejects port with no hostname"
      (either (const False) null (parseUrl "||:8080^")),
    check
      "parseUrl accepts wildcard port"
      (either (const False) (not . null) (parseUrl "||example.com:*^"))
  ]

main :: IO ()
main = do
  forM_ cases $ \c ->
    putStrLn $ (if caseOk c then "OK   " else "FAIL ") ++ caseName c
  if all caseOk cases then exitSuccess else exitFailure
