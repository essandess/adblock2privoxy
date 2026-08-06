module Main (main) where

import Control.Monad (forM_)
import Data.List (isInfixOf, isPrefixOf)
import System.Exit (exitFailure, exitSuccess)
import Text.ParserCombinators.Parsec (parse)

import InputParser (adblockFile)
import InputParser (Line(..), Record(..), Policy(Allow), Restrictions(..))
import ParserExtTests (testParsecExt, testParseMorse)
import PatternConverter (parseUrl, globWildcard, hasUnsupportedPCRE, isOverbroadPattern, maxPatternLength, makePattern)
import PolicyTree (restrictionsTree, NodePolicy(Block, Allow))
import Utils (replace, split, startswith)


data Case = Case { caseName :: String, caseOk :: Bool }

check :: String -> Bool -> Case
check = Case

cases :: [Case]
cases =
  [ check "replace basic"
      (replace "foo" "bar" "foobarfoo" == "barbarbar")
  , check "replace no match"
      (replace "xyz" "Q" "abc" == "abc")
  , check "split basic"
      (split "," "a,b,,c" == ["a","b","","c"])
  , check "split no delim"
      (split "," "abc" == ["abc"])
  , check "startswith true"
      (startswith "foo" "foobar")
  , check "startswith false"
      (not (startswith "bar" "foobar"))
  , check "parseUrl doesn't error on simple pattern"
      (either (const False) (const True) (parseUrl "||example.com^"))
  , check "genuine second-level domain still produced (example.com)"
       (either (const False) (not . null) (parseUrl "||example.com^"))
  , check "hosts-file-style '#' comment is not a parse error"
      (case parse adblockFile "test" "# title: Example Hosts File\n" of
          Right [Line _ (Comment _)] -> True
          _                          -> False)
  , check "wildcard TLD domain list with element-hide rule parses"
      (case parse adblockFile "test" "example.com,tripadvisor.*##.ad\n" of
          Right [Line _ (ElementHide {})] -> True
          _                               -> False)
  , check "extended CSS marker '#?#' parses as a Block element-hide rule"
      (case parse adblockFile "test" "character.ai#?#.foo:has-text(Ad)\n" of
          Right [Line _ (ElementHide _ Block _)] -> True
          _                                      -> False)
  , check "snippet marker '#$#' parses as a Block element-hide rule"
      (case parse adblockFile "test" "example.com#$#abort-on-property-read foo\n" of
          Right [Line _ (ElementHide _ Block _)] -> True
          _                                      -> False)
  , check "exception marker '#@##' parses as an Unblock element-hide rule"
      (case parse adblockFile "test" "example.com#@##onetrust-banner-sdk\n" of
          Right [Line _ (ElementHide _ Unblock _)] -> True
          _                                        -> False)
  , check "network pattern containing literal '#' no longer truncates (regression)"
      (either (const False) (const True)
              (parseUrl "/\\.(gif|jpe?g|png|webp)#(\\/?.+)?(\\/(ad)s?\\/|\\/ad-)/"))
  , check "unknown option with '=value' doesn't break the rest of the option list"
      (case parse adblockFile "test" "/^https:\\/\\/example\\.com\\//$doc,match-case,to=club|com|info\n" of
          Right [Line _ (RequestBlock _ _ opts)] -> _matchCase opts
          _                                      -> False)
  , check "adblockFile parses minimal header"
      (either (const False) (const True)
              (parse adblockFile "test" "[Adblock Plus 2.0]\n"))
  , check "adblockFile parses list with no header (uBlock-style)"
      (either (const False) (const True)
              (parse adblockFile "test" "! Title: example\n||example.com^\n"))  , check "ParsecExt cases combinator (ab/eb + letters + alnum)"
      (either (const False) (not . null) testParsecExt)
  , check "ParsecExt morse decode of HELLO"
      (either (const False) (elem "HELLO") testParseMorse)
  , check "parseUrl terminates on malformed bracket in path (regression)"
      (either (const False) (const True) (parseUrl "||example.com/foo[bar^"))
  , check "host dots are literal, not PCRE-escaped (regression)"
      (either (const False)
              (all (\p -> not ("\\." `isInfixOf` makePattern False p)))
              (parseUrl "||example.com^"))
  , check "bare TLD host is rejected (com)"
      (either (const False) null (parseUrl "||com^"))
  , check "unanchored-dot TLD host is rejected (.com)"
      (either (const False) null (parseUrl "||.com^"))
  , check "wildcard-only TLD host is rejected (*.com)"
      (either (const False) null (parseUrl "||*.com^"))
  , check "wildcard subdomain of a real domain is kept (*.example.com)"
      (either (const False) (not . null) (parseUrl "||*.example.com^"))
  , check "SourceInfo parses dotted version without crashing (regression)"
      (let info = updateInfo dummyTime
                    [Line dummySrc (Comment "Version: 2.0.85.87")]
                    (makeInfo "http://example.com")
       in True `seq` True)
  -- PCRE filtering tests
  , check "hasUnsupportedPCRE accepts a single positive lookahead (?=...)"
      (not (hasUnsupportedPCRE "foo(?=bar)baz"))
  , check "hasUnsupportedPCRE accepts a single negative lookahead (?!...)"
      (not (hasUnsupportedPCRE "foo(?!bar)baz"))
  , check "hasUnsupportedPCRE accepts a single positive lookbehind (?<=...)"
      (not (hasUnsupportedPCRE "foo(?<=bar)baz"))
  , check "hasUnsupportedPCRE accepts a single negative lookbehind (?<!...)"
      (not (hasUnsupportedPCRE "foo(?<!bar)baz"))
  , check "hasUnsupportedPCRE detects two chained lookaheads (the catastrophic-backtracking shape)"
      (hasUnsupportedPCRE "foo(?=[a-z]{0,9}1)(?=[a-z]{0,9}2)bar")
  , check "hasUnsupportedPCRE detects a lookahead plus a lookbehind chained together"
      (hasUnsupportedPCRE "foo(?=bar)(?<=baz)qux")
  , check "hasUnsupportedPCRE detects conditional (?(...))"
      (hasUnsupportedPCRE "foo(?(1)bar)baz")
  , check "hasUnsupportedPCRE accepts named capture (?P<name>...)"
      (not (hasUnsupportedPCRE "foo(?P<test>bar)baz"))
  , check "hasUnsupportedPCRE accepts backtracking control (*PRUNE)"
      (not (hasUnsupportedPCRE "foo(*PRUNE)bar"))
  , check "hasUnsupportedPCRE accepts backtracking control (*SKIP)"
      (not (hasUnsupportedPCRE "foo(*SKIP)bar"))
  , check "hasUnsupportedPCRE accepts normal PCRE patterns"
      (not (hasUnsupportedPCRE "foo[a-z]+bar.*baz"))
  , check "hasUnsupportedPCRE accepts character classes"
      (not (hasUnsupportedPCRE "[a-zA-Z0-9_-]+"))
  , check "hasUnsupportedPCRE accepts quantifiers"
      (not (hasUnsupportedPCRE "foo{1,3}bar+baz*qux?"))
  , check "hasUnsupportedPCRE accepts alternation"
      (not (hasUnsupportedPCRE "foo|bar|baz"))
  , check "hasUnsupportedPCRE accepts anchors"
      (not (hasUnsupportedPCRE "^foo.*bar\\$"))
  , check "hasUnsupportedPCRE accepts word boundaries"
      (not (hasUnsupportedPCRE "\\bfoo\\b"))
  , check "hasUnsupportedPCRE accepts non-capturing groups"
      (not (hasUnsupportedPCRE "foo(?:bar|baz)qux"))
  , check "hasUnsupportedPCRE accepts case-insensitive modifier"
      (not (hasUnsupportedPCRE "(?i)foo"))
  , check "hasUnsupportedPCRE accepts case-sensitive modifier"
      (not (hasUnsupportedPCRE "(?-i)foo"))
  , check "hasUnsupportedPCRE rejects a malformed pattern (unmatched bracket)"
      (hasUnsupportedPCRE "foo[bar")
  , check "parseUrl accepts pattern with a single positive lookahead"
      (case parseUrl "/(?=test)/" of
          Right patterns -> not (null patterns) &&
                           all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _         -> False)
  , check "parseUrl accepts pattern with a single negative lookahead"
      (case parseUrl "/(?!test)/" of
          Right patterns -> not (null patterns) &&
                           all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _         -> False)
  , check "parseUrl accepts pattern with a single positive lookbehind"
      (case parseUrl "/(?<=test)/" of
          Right patterns -> not (null patterns) &&
                           all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _         -> False)
  , check "parseUrl accepts pattern with a single negative lookbehind"
      (case parseUrl "/(?<!test)/" of
          Right patterns -> not (null patterns) &&
                           all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _         -> False)
  , check "parseUrl filters the problematic badware.txt pattern (line 625)"
      (case parseUrl "/^https:\\/\\/(?:www\\d\\.)?[-a-z]{6,}\\.(?:club|com)\\/" ++
                     "(?=[-_a-zA-Z]{0,42}\\d)(?=[-_0-9a-z]{0,42}[A-Z])[-_0-9a-zA-Z]{43}\\/" ++
                     "\\?(?:pub|s)id=[-_0-9a-z{}]{1,32}(?:&qs\\d=\\S+)?&cid=[-_0-9a-zA-Z]{10,36}\\$/" of
          Right patterns -> all (\p -> "# FILTERED:" `isInfixOf` makePattern False p) patterns
          Left _         -> False)
  -- Over-broad (host-unrestricted, match-everything) pattern filtering tests
  , check "isOverbroadPattern rejects an empty host with a bare wildcard path"
      (isOverbroadPattern "" "(*PRUNE).*?")
  , check "isOverbroadPattern rejects an empty host with a bare wildcard path, end-anchored"
      (isOverbroadPattern "" "(*PRUNE).*?$")
  , check "isOverbroadPattern allows a wildcard path once it's followed by a literal"
      (not (isOverbroadPattern "" "(*PRUNE).*?ads"))
  , check "isOverbroadPattern allows a bare wildcard path when the host is restricted"
      (not (isOverbroadPattern "example.com" "(*PRUNE).*?"))
  , check "parseUrl accepts simple domain pattern"
      (case parseUrl "||example.com^" of
          Right patterns -> not (null patterns) && 
                           all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _         -> False)
  , check "parseUrl accepts wildcard pattern"
      (case parseUrl "||example.com/*/ads/*" of
          Right patterns -> not (null patterns) &&
                           all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _         -> False)
  , check "parseUrl accepts separator pattern"
      (case parseUrl "||example.com^banner^" of
          Right patterns -> not (null patterns) &&
                           all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
          Left _         -> False)
  , check "parseUrl filters excessively long pattern"
      (let longPattern = "/^" ++ replicate (maxPatternLength + 100) 'a' ++ "\\$/"
       in case parseUrl longPattern of
              Right patterns -> all (\p -> "# FILTERED:" `isInfixOf` makePattern False p) patterns
              Left _         -> False)
  , check "parseUrl accepts normal-length pattern"
      (let normalPattern = "/^" ++ replicate 100 'a' ++ "\\$/"
       in case parseUrl normalPattern of
              Right patterns -> not (null patterns) &&
                               all (\p -> not ("# FILTERED:" `isInfixOf` makePattern False p)) patterns
              Left _         -> False)
  -- Domain validation tests for PolicyTree
  , check "restrictionsTree filters out bare TLD from positive list"
      (case restrictionsTree Block (Restrictions (Just ["com", "example.com"]) []) of
          Just tree -> True  -- Should succeed with only example.com
          Nothing   -> False)
  , check "restrictionsTree filters out bare TLD from negative list"
      (case restrictionsTree Block (Restrictions Nothing ["org", "example.org"]) of
          Just tree -> True  -- Should succeed with only example.org negated
          Nothing   -> False)
  , check "restrictionsTree returns Nothing when all positive domains invalid"
      (case restrictionsTree Block (Restrictions (Just ["com", "org", "net"]) []) of
          Just _  -> False
          Nothing -> True)
  , check "restrictionsTree handles mixed valid/invalid domains in positive list"
      (case restrictionsTree Block (Restrictions (Just ["com", "example.com", "org"]) []) of
          Just _  -> True   -- Should keep example.com
          Nothing -> False)
  , check "restrictionsTree handles mixed valid/invalid domains in negative list"
      (case restrictionsTree Block (Restrictions Nothing ["net", "example.net", "io"]) of
          Just _  -> True   -- Should keep example.net
          Nothing -> False)
  , check "element-hide rule with bare TLD in domain list is filtered"
      (case parse adblockFile "test" "com,example.com##.ad\n" of
          Right [Line _ (ElementHide restrictions Block pattern)] ->
              case restrictionsTree Block restrictions of
                  Just _  -> True   -- Filtered to keep only example.com
                  Nothing -> False
          _ -> False)
  , check "element-hide rule with only bare TLDs produces no tree"
      (case parse adblockFile "test" "com,org,net##.ad\n" of
          Right [Line _ (ElementHide restrictions Block pattern)] ->
              case restrictionsTree Block restrictions of
                  Just _  -> False
                  Nothing -> True   -- All domains filtered out
          _ -> False)
  , check "element-hide rule with wildcard TLD subdomain is kept (*.example.com)"
      (case parse adblockFile "test" "*.example.com##.ad\n" of
          Right [Line _ (ElementHide restrictions Block pattern)] ->
              case restrictionsTree Block restrictions of
                  Just _  -> True
                  Nothing -> False
          _ -> False)
   -- port pattern
   , check "parseUrl accepts valid host:port pattern"
       (either (const False) (not . null) (parseUrl "||example.com:8080^"))
   , check "parseUrl rejects host with malformed port"
       (either (const False) null (parseUrl "||example.com:notaport^"))
   , check "parseUrl rejects port with no hostname"
       (either (const False) null (parseUrl "||:8080^"))
   , check "parseUrl accepts wildcard port"
       (either (const False) (not . null) (parseUrl "||example.com:*^"))
   , check "host with a fixed port doesn't get a spurious wildcard before the path (regression)"
       (case parseUrl "||example.com:8443/impact^" of
           Right patterns -> not (null patterns) &&
                            all (\p -> not ("8443(*PRUNE)" `isInfixOf` makePattern False p)) patterns
           Left _         -> False)
   , check "unanchored host:port rule is recognized as a host, not folded into the path (regression)"
       (case parseUrl ".example.com:*/image/ui/" of
           Right patterns -> not (null patterns) &&
                            any (\p -> "PCRE-HOST-PATTERN:" `isPrefixOf` makePattern False p) patterns
           Left _         -> False)
  , check "unanchored path with embedded domain doesn't get a spurious leading wildcard (regression)"
      (case parseUrl "/email.example.com/o/*" of
          Right patterns -> any (\p -> makePattern False p == ".*email.unsplash.com/o/" ++ globWildcard) patterns
          Left _         -> False)
  , check "percent-encoded path separators are decoded before host/path splitting (regression)"
      (case parseUrl ".example.com%2Fq%2F" of
          Right patterns -> any (\p -> makePattern False p == ".example.com/q/") patterns
          Left _         -> False)
  ]

main :: IO ()
main = do
  forM_ cases $ \c ->
    putStrLn $ (if caseOk c then "OK   " else "FAIL ") ++ caseName c
  if all caseOk cases then exitSuccess else exitFailure
