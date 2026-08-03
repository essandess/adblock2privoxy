{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module PatternConverter (
makePattern,
parseUrl,
-- expose utility functions for maintenance and testing
UrlPattern(..),
SideBind(..),
globWildcard,
maxPatternLength,
bindStart,
bindEnd,
queryEnd,
unparsedHostPrefix,
repairUnparsedHost,
splitClosingSlash,
postfilter,
urlParts,
makeUrl,
makeUrls
) where
import InputParser
import Control.Applicative hiding (many)
import Control.Monad (guard)
import Text.ParserCombinators.Parsec hiding (Line, (<|>))
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Char (isAlphaNum, isDigit)
import Data.Maybe
import ParsecExt
import Utils
import Control.Exception (evaluate, handle)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import Text.Regex.Pcre2 (SomePcre2Exception, match)
import Network.URI (unEscapeString)


-- https://github.com/AdguardTeam/AdGuardHome/wiki/Hosts-Blocklists
-- https://www.privoxy.org/user-manual/actions-file.html

data UrlPattern = UrlPattern {
                   _bindStart :: SideBind,
                   _proto :: String,
                   _host :: String,
                   _query :: String,
                   _bindEnd :: SideBind,
                   _regex :: Bool }
              deriving (Show, Eq)

data SideBind = Hard | Soft | None deriving (Show, Eq)

-- Lookaround markers PCRE2 accepts fine on their own.
lookaroundMarkers :: [String]
lookaroundMarkers = ["(?=", "(?!", "(?<=", "(?<!"]

-- Count non-overlapping occurrences of a substring.
countOccurrences :: String -> String -> Int
countOccurrences needle haystack = length (filter (needle `isPrefixOf`) (tails haystack))

-- Two or more chained lookarounds is the shape that can explode Privoxy's backtracking; one alone is fine.
hasMultipleLookarounds :: String -> Bool
hasMultipleLookarounds pattern = sum [countOccurrences m pattern | m <- lookaroundMarkers] >= 2

-- True if PCRE2 rejects the pattern outright, or it has the multi-lookaround shape.
hasUnsupportedPCRE :: String -> Bool
hasUnsupportedPCRE pattern = hasMultipleLookarounds pattern || unsafePerformIO
    (handle @SomePcre2Exception (\_ -> return True) $ do
        _ <- evaluate (match (T.pack pattern) T.empty :: Maybe T.Text)
        return False)
{-# NOINLINE hasUnsupportedPCRE #-}

-- Maximum reasonable pattern length for Privoxy
maxPatternLength :: Int
maxPatternLength = 8000

-- efficient non-backtracking wildcard match for glob
{- http://blogs.perl.org/users/mauke/2017/05/converting-glob-patterns-to-efficient-regexes-in-perl-and-javascript.html -}
globWildcard :: String
globWildcard = "(*PRUNE).*?"

-- A host-unrestricted pattern whose query is just the wildcard expansion matches every URL.
isOverbroadPattern :: String -> String -> Bool
isOverbroadPattern host' query' = null host' && query' `elem` [globWildcard, globWildcard ++ "$"]

makePattern :: Bool -> UrlPattern -> Pattern
makePattern matchCase (UrlPattern bindStart proto host query bindEnd isRegex)
            | query' == "" = host'
            -- Filter out patterns with unsupported PCRE
            | hasUnsupportedPCRE query' =
                "# FILTERED: Pattern contains PCRE features unsupported by Privoxy (lookahead/lookbehind/etc.)"
            -- Filter out rules that match every path on every host
            | isOverbroadPattern host' query' =
                "# FILTERED: Pattern is over-broad (matches every path)"
            -- Filter out excessively long patterns
            | length fullPattern > maxPatternLength =
                "# FILTERED: Pattern exceeds maximum length (" ++ show maxPatternLength ++ " chars)"
            | otherwise    = host' ++ separator' ++ query'
    where
        fullPattern = host' ++ separator' ++ query'
        separator'
            | matchCase = "/(?-i)"
            | otherwise = "/"
        -- Privoxy host pattern with FEATURE_PCRE_HOST_PATTERNS
        host' = case host of
                    "" -> ""
                    -- Drop unnecessary wildcard for Privoxy hosts
                    "*" -> ""
                    _  -> let (domainPart, portPart) = splitHostPort host
                              -- A port is a fixed, non-continuable token: don't glue the
                              -- subdomain-continuation glob onto it, or it lands between
                              -- the port and the path instead of terminating the host.
                              domainGlob = if null portPart then changeLast domainPart else domainPart
                              hostGlob = changeFirst domainGlob ++ portPart
                          in if hostNeedsPCRE hostGlob
                             then "PCRE-HOST-PATTERN:" ++ pcreHostPattern hostGlob
                             else hostGlob
                    where
                    -- A parsed host has at most one ':', marking an explicit port.
                    splitHostPort s = case break (== ':') s of
                        (domain, port@(':':_)) -> (domain, port)
                        _                      -> (s, "")

                    changeLast []                                   = []
                    changeLast ['.', '*']
                        | query' == ""                              = "."
                        | otherwise                                 = ".*"
                    changeLast ['*', '.']
                        | query' == ""                              = "*."
                        | otherwise                                 = "*.*"
                    changeLast [lst]
                        | lst == '|' || lst `elem` hostSeparators   =  []
                        | lst == '*' && query' == ""                =  "*."
                        | lst == '*' && query' /= ""                =  "*"
                        | otherwise                                 =  lst : []
                    changeLast (c:cs) = c : changeLast cs

                    changeFirst []    = []
                    changeFirst (first:cs)
                        | first == '*'                       = '.' :  '*'  : cs
                        | first == '.' || bindStart == Hard || proto /= ""  = first : cs
                        | bindEnd == Hard                    = '.' : '*' : first : cs
                        | bindStart == Soft                  = '.' : first : cs
                        | otherwise                          = '.' : '*' : first : cs

        query' = case query of
                    ""     -> ""
                    (start:other) ->
                              if isRegex then query
                              else case query of
                                '*' : '/' : other' -> replaceQuery '/' other' True True
                                '*' : '^' : other' -> replaceQuery '^' other' True True
                                -- a literal leading '/' is its own anchor
                                -- "./" is just as anchoring as a bare leading '/'
                                _                  -> replaceQuery start other (bindStart == None && host == "" && not (isPathAnchored query)) False
                               where
                                isPathAnchored q = "/" `isPrefixOf` q || "./" `isPrefixOf` q
                              where
                                replaceQuery c cs openStart collapsed = replaceFirst c openStart collapsed
                                                              ++ concatMap renderChunk (tokenizeBracketClasses cs)
                                                              ++ queryEnd
                                replaceFirst '*' _ _ = globWildcard
                                -- collapsed=True only for an explicit "*/"/"*^" prefix, where the
                                -- separator can be folded into the preceding wildcard as optional;
                                -- a bare leading '/' or '^' with no wildcard must stay a required literal.
                                replaceFirst c openStart collapsed
                                    | collapsed && (c == '/' || c == '^') = if openStart
                                                             then "(?:" ++ globWildcard ++ replaceWildcard c ++ ")?"
                                                             else ""
                                    -- makePattern will insert /
                                    | c == '/' && not openStart = ""

                                    | otherwise            = if openStart
                                                             then globWildcard ++ replaceWildcard c
                                                             else replaceWildcard c

                                queryEnd = if bindEnd == None then "" else "$"

                                replaceWildcard c
                                    | c == '^'         = "[^\\w%.-]"
                                    | c == '*'         = globWildcard
                                    | c `elem` special = '\\' : [c]
                                    | otherwise        = [c]
                                    where special = "?$.+[{}()\\|" -- also ^ and * are special
                                        -- Note: lone ']' outside a bracket class is not special in PCRE

                                -- Split a query string into alternating plain-text runs and bracket character classes
                                tokenizeBracketClasses :: String -> [Either String String]
                                tokenizeBracketClasses [] = []
                                tokenizeBracketClasses s@(x:xs)
                                    | x == '[', Just (cls, rest) <- matchClass xs =
                                        Right ('[' : cls ++ "]") : tokenizeBracketClasses rest
                                    | otherwise =
                                        let (plain, rest) = breakNextBracket s
                                        in Left plain : tokenizeBracketClasses rest
                                  where
                                    -- Like 'break (== \'[\')', but always consumes at least the first character first.
                                    breakNextBracket :: String -> (String, String)
                                    breakNextBracket [] = ([], [])
                                    breakNextBracket (c:cs) =
                                        let (p, r) = break (== '[') cs in (c : p, r)
                                    matchClass ys = case break (== ']') ys of
                                       (body, ']':rest')
                                           | not (null body) && all isClassChar body -> Just (body, rest')
                                       _ -> Nothing
                                    isClassChar ch = isAlphaNum ch || ch `elem` "-^"

                                renderChunk :: Either String String -> String
                                renderChunk (Left plain) = concatMap replaceWildcard plain
                                renderChunk (Right cls)  = cls

hostSeparators :: String
hostSeparators = "^/"

-- Privoxy's non-PCRE host glob syntax
isPlainHostChar :: Char -> Bool
isPlainHostChar c = isAlphaNum c || c `elem` "-.*?:"

hostNeedsPCRE :: String -> Bool
hostNeedsPCRE = any (not . isPlainHostChar)

-- Translate a host glob into an equivalent PCRE
pcreHostPattern :: String -> String
pcreHostPattern s = anchorStart ++ concatMap esc s ++ anchorEnd
    where
        anchorStart = if any (`isPrefixOf` s) ["*", "."] then "" else "^"
        anchorEnd   = if any (`isSuffixOf` s) ["*", "."] then "" else "$"
        esc '*' = globWildcard
        esc '?' = "."
        esc '.' = "\\."
        esc c   = [c]

-- Parses a leading "|" / "||" URL-anchor prefix, if present.
bindStart = (try (Soft <$ string "||") <|> try (Hard <$ string "|") <|> return None) <?> "query start"

-- Parses the end of a pattern: a trailing "|" right before end-of-input,
-- or the '\0' sentinel used to mark end-of-line, without consuming it in
-- the '\0' case (so callers can still 'lookAhead' it).
queryEnd = (char '|' <* eof) <|> ('\0' <$ eof) <|> char '\0' <?> "query end"

-- Parses a trailing "|" URL-anchor suffix, if present.
bindEnd = (\c -> if c == '|' then Hard else None) <$> queryEnd

-- Parses an explicit ":port" suffix on a host, where the port may itself
-- contain '*' wildcards (e.g. ":80*").
portSuffix = (:) <$> char ':' <*> many1 (digit <|> char '*')

-- Strips a single trailing '\0' end-of-line sentinel, if present.
trimTrailingNul :: String -> String
trimTrailingNul [] = []
trimTrailingNul (c:cs)
    | null cs && c == '\0' = []
    | otherwise             = c : trimTrailingNul cs

-- Characters legal in an (unglobbed) host label.
hostChar = alphaNum <|> oneOf ".-"

-- Recognized URL schemes, longest/most-specific match preferred by callers.
protocols :: [String]
protocols = ["https://", "http://"]

-- Separator between alternative continuations tracked while parsing 'proto'.
protocolsSeparator :: String
protocolsSeparator = ";"

-- Characters that can appear inside one of 'protocols' (minus the slash,
-- which is handled separately via 'hostSeparators').
protocolChar = oneOf (delete '/' $ nub $ join protocols)

-- If 'host' is empty but 'query' actually starts with a
-- [.]domain[:port]/ prefix, that domain was mis-captured as query text
-- by the main grammar.
repairUnparsedHost :: String -> String -> (String, String)
repairUnparsedHost "" query
    | Just (hostPrefix, rest) <- unparsedHostPrefix query = (hostPrefix, rest)
repairUnparsedHost host query = (host, query)

-- Detects a domain[:port]/ prefix (optionally preceded by a single
-- leading '.' or '/') at the start of a query string, and if found,
-- splits it into the domain/port prefix and the remaining query
-- (still starting with '/').
unparsedHostPrefix :: String -> Maybe (String, String)
unparsedHostPrefix query = case break (== '/') afterLead of
    (candidate, rest@('/':_)) | isHostCandidate candidate -> Just (leadPrefix ++ candidate, rest)
    _                                                      -> Nothing
    where
    (leadPrefix, afterLead) = case query of
        ('.':rest') -> (".", rest')
        _           -> ("", query)
        -- leading '/' means a genuine path rule
    isHostCandidate candidate = case break (== ':') candidate of
        (domainPart, ':':portDigits) ->
            not (null portDigits)
            && all (\c -> isDigit c || c == '*') portDigits
            && (null domainPart || isPlainDomain domainPart) -- empty domain + port is a valid host (e.g. ":8443")
        (domainPart, "") -> isPlainDomain domainPart -- no port: still a host if it's a plain multi-label domain
        _ -> False
    isPlainDomain domainPart =
        all (\c -> isAlphaNum c || c == '-' || c == '.') domainPart
        && length (filter (not . null) (split "." domainPart)) >= 2

-- Finds the unescaped '/' that closes a leading /regex/ body, per
-- AdGuard hosts-blocklist syntax, returning the regex body and whatever
-- follows the closing slash (e.g. "$mods"), if any.
splitClosingSlash :: String -> Maybe (String, String)
splitClosingSlash ('/':rest) = go rest ""
    where
        go []          _   = Nothing
        go ('\\':c:cs) acc = go cs (c : '\\' : acc)
        go ('/':cs)    acc = Just (reverse acc, cs)
        go (c:cs)      acc = go cs (c : acc)
splitClosingSlash _ = Nothing

-- Turns one raw parsed (bindStart, proto, host, query, bindEnd) tuple
-- into zero, one, or two UrlPattern's
postfilter :: UrlPattern -> [UrlPattern]
postfilter url@(UrlPattern bs proto rawHost rawQuery be _) = regular ++ regex -- ++ www
    where
        -- Workaround: host text can get swallowed into query when nothing forces
        -- host parsing (see host/urlParts); heuristically recovered here when a
        -- domain[:port]/ prefix makes the intent unambiguous. The real fix
        -- belongs in the host parser itself.
        (host, repairedQuery) = repairUnparsedHost rawHost rawQuery
        -- A lone trailing '^' is just the end-of-host separator
        query = if repairedQuery == "^" then "" else repairedQuery

        -- "/regex/" or "/regex/$modifiers" per AdGuard hosts-blocklist syntax is
        -- only meaningful when the WHOLE pattern is host/proto/bind-less. Without
        -- this gate, an ordinary two-segment leftover path from a host-bearing case
        -- (e.g. "/q/" from ".example.com/q/") has the same open/close-slash shape
        -- and gets misread as a regex, producing a spurious hostless rule.
        regex = case (bs, proto, host) of
                    (None, "", "") -> case splitClosingSlash query of
                        Just (body, mods) | not (null body) && (null mods || "$" `isPrefixOf` mods) ->
                            [UrlPattern bs "" "" body be True]
                        _ -> []
                    _ -> []
        regular = let
                     leftBound = bs /= None || proto /= ""
                     rightBound = be /= None || query /= ""
                     orphanQuery = leftBound && host == "" && query /= "" && not ("*" `isPrefixOf` query)
                     duplicateHostStar = host == "*"
                     -- Do not block tld hosts, i.e. reject tld-only here; n.b. independent of leftBound / rightBound to avoid partially-anchored TLD-only hosts
                     hostLabels = filter (\l -> l /= "" && l /= "*") (split "." host)
                     -- trailing "." (any-tld) or an explicit "*" mean "prefix", not a bare TLD; don't reject those
                     tldOnlyHost = host /= "" && length hostLabels < 2
                                   && '*' `notElem` host && not ("." `isSuffixOf` host)
                     -- A bare /regex/ (no bind, no proto, no host) is regex-only per spec: don't also emit it as a literal rule.
                     bareRegex = not (null regex) && bs == None && proto == "" && host == ""
                     -- An unanchored ".domain.tld/path" (no "|"/proto forcing host
                     -- parsing) is the Adblock convention for "this domain and any
                     -- subdomain"; the leading '.' is the signal. When "cases" also
                     -- produced a sibling that actually captured the domain as host,
                     -- this "no host at all" reading is a spurious duplicate of it.
                     leadingDotHost = case break (`elem` hostSeparators) query of
                        (candidate@('.':_), _:_) ->
                            let labels = filter (not . null) (split "." candidate)
                            in length labels >= 2
                               && all (all (\c -> isAlphaNum c || c == '-')) labels
                        _ -> False
                     unanchoredDottedHost = host == "" && leadingDotHost
                  in if not (orphanQuery || tldOnlyHost || bareRegex || unanchoredDottedHost)
                     then
                        let
                            query' = if "*" `isSuffixOf` host && query /= "" then '*' : query else query
                        in [url {_host = host, _query = query'}]
                     else []

-- Combines a leading bind, the parsed (proto, host, query) cases, and a
-- trailing bind into the list of raw UrlPattern's for a line
makeUrls :: SideBind -> [(String, String, String)] -> SideBind -> [UrlPattern]
makeUrls start mid end = (makeUrl start <$> mid) <*> pure end

-- Builds a single raw (pre-postfilter) UrlPattern from one parsed case
makeUrl :: SideBind -> (String, String, String) -> SideBind -> UrlPattern
makeUrl start (proto, host, query) end = UrlPattern start proto (trimTrailingNul host) query end False

-- The core proto/host/query grammar. Each of proto/host/query is
-- individually optional-ish (see the parsers below); 'host' in
-- particular must be able to match nothing at all, or a bare-query
-- pattern (a plain /path rule, or a bare /regex/ rule with no
-- domain) fails to produce any case here at all, and the whole line is
-- silently dropped rather than emitted or erroring.
urlParts :: [StringStateParser (String,String,String)]
urlParts = square3 proto (manyCases host) (oneCase query)
    where
        append xs x = xs ++ [x]
        proto :: StringStateParser String
        proto = do
                masksString <- get
                case masksString of
                    Nothing ->
                        do
                        put $ Just $ intercalate protocolsSeparator protocols
                        return "" --allow to skip proto
                    Just masksString' ->
                        do
                        let masks = split protocolsSeparator masksString'
                        if null masks
                            then lift pzero -- no continuations available (parser have finished on previous iteration)
                            else
                                do
                                lift $ skipMany $ char '*' --skip leading * if presented
                                name <- lift $ many1 protocolChar
                                sep <- lift $ many $ oneOf hostSeparators
                                let chars = name ++ replace "^" "//" sep -- concatenate input and expand separator wildcard
                                nextChar <- lift $ lookAhead anyChar
                                let masks' = filterProtoMasks masks chars nextChar -- find possible continuations for current input
                                if null masks' || null chars
                                    then lift pzero -- fail parser if no continuations or no chars read
                                    else
                                        do
                                        put $ Just $ if isJust (find null masks')  -- if empty continuation found (i.e. parser finished)
                                                        then "" -- make no continuations available next time
                                                        else intercalate protocolsSeparator masks'
                                        return $ if nextChar == '*' then chars ++ "*" else chars
        -- N.b. no domain for rules like /regex/, /path
        host = try (append <$> many hostChar <*> char '*') <|>
               try ((++) <$> many1 hostChar <*> option "" (try portSuffix) <* lookAhead separator) <?> "host"

        separator = (oneOf hostSeparators <|> queryEnd) <?> "separator"
        query = notFollowedBy (try $ string "//") *> manyTill anyChar (lookAhead (try queryEnd)) <?> "query"

        filterProtoMasks :: [String] -> String -> Char -> [String]
        filterProtoMasks masks chars nextChar = mapMaybe filterProtoMask masks
            where filterProtoMask mask = if nextChar /= '*'
                            then if chars `isSuffixOf` mask
                                 then Just ""
                                 else Nothing
                            else let tailFound = find (chars `isPrefixOf`) (tails mask)
                                 in drop (length chars) <$> tailFound

-- Parses one line into its component UrlPattern's
parseUrl :: Pattern -> Either ParseError [UrlPattern]
parseUrl pattern
    -- "##"/"##+js(...)" etc. is a cosmetic/scriptlet rule, not a URL pattern; don't force it through this grammar
    | "##" `isInfixOf` pattern = Right []
    | otherwise = parse (nub . join <$> (fmap.fmap) postfilter raw) "url" (decodeUrlEscapes pattern) -- collapse identical cases
    where
        raw = makeUrls <$> bindStart <*> cases urlParts <*> bindEnd

-- synonym for unEscapeString
decodeUrlEscapes :: String -> String
decodeUrlEscapes = unEscapeString
