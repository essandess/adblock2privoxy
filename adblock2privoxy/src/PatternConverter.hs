{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module PatternConverter (
makePattern,
parseUrl,
hasUnsupportedPCRE,
isOverbroadPattern,
maxPatternLength
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

data UrlPattern = UrlPattern {
                   _bindStart :: SideBind,
                   _proto :: String,
                   _host :: String,
                   _query :: String,
                   _bindEnd :: SideBind,
                   _regex :: Bool }
              deriving (Show)

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
                        | otherwise                                 =  lst : "*."
                    changeLast (c:cs) = c : changeLast cs

                    changeFirst []    = []
                    changeFirst (first:cs)
                        | first == '*'                       =       '.' :  '*'  : cs
                        | first == '.' || bindStart == Hard || proto /= ""   =       first : cs
                        | bindStart == Soft                  =       '.' : first : cs
                        | otherwise                          = '.' : '*' : first : cs

        query' = case query of
                    ""     -> ""
                    (start:other) ->
                              if isRegex then query
                              else case query of
                                '*' : '/' : other' -> replaceQuery '/' other' True
                                '*' : '^' : other' -> replaceQuery '^' other' True
                                _                  -> replaceQuery start other (bindStart == None && host == "")
                              where
                                -- replaceQuery c cs openStart = replaceFirst c openStart ++ (join . map replaceWildcard $ cs) ++ queryEnd
                                replaceQuery c cs openStart = replaceFirst c openStart
                                                              ++ concatMap renderChunk (tokenizeBracketClasses cs)
                                                              ++ queryEnd
                                replaceFirst '*' _ = globWildcard
                                replaceFirst c openStart
                                    | c == '/' || c == '^' = if openStart
                                                             then "(?:" ++ globWildcard ++ replaceWildcard c ++ ")?"
                                                             else ""
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
isPlainHostChar c = isAlphaNum c || c `elem` "-.*?"

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

parseUrl :: Pattern -> Either ParseError [UrlPattern]
parseUrl =
    let  raw = makeUrls <$> bindStart <*> cases urlParts <*> bindEnd
    in   parse (join <$> (fmap.fmap) postfilter raw) "url"
    where
        makeUrls start mid end = (makeUrl start <$> mid) <*> pure end
        makeUrl start (proto, host, query) end = UrlPattern start proto (trimTrailingNul host) query end False

        bindStart = (try (Soft <$ string "||") <|> try (Hard <$ string "|") <|> return None) <?> "query start"
        queryEnd = (char '|' <* eof) <|> ('\0' <$ eof) <|> char '\0' <?> "query end"
        bindEnd = (\c -> if c == '|' then Hard else None) <$> queryEnd

        portSuffix :: Parser String
        portSuffix = (:) <$> char ':' <*> many1 (digit <|> char '*')


        trimTrailingNul :: String -> String
        trimTrailingNul [] = []
        trimTrailingNul (c:cs)
            | null cs && c == '\0' = []
            | otherwise             = c : trimTrailingNul cs

        hostChar :: Parser Char
        hostChar = alphaNum <|> oneOf ".-"

        protocols :: [String]
        protocols = ["https://", "http://"]

        protocolsSeparator :: String
        protocolsSeparator = ";"

        protocolChar :: Parser Char
        protocolChar = oneOf (delete '/' $ nub $ join protocols)

        -- If host is empty but query starts with "[.]domain[:port]/", the domain[:port]
        -- was mis-captured as query text; split it back out into host.
        repairUnparsedHost :: String -> String -> (String, String)
        repairUnparsedHost "" query
            | Just (hostPrefix, rest) <- unparsedHostPrefix query = (hostPrefix, rest)
        repairUnparsedHost host query = (host, query)

        unparsedHostPrefix :: String -> Maybe (String, String)
        unparsedHostPrefix query = case break (== '/') afterDot of
            (candidate, rest@('/':_)) | isDomainWithPort candidate -> Just (leadingDot ++ candidate, rest)
            _                                                      -> Nothing
            where
            (leadingDot, afterDot) = case query of
                ('.':rest') -> (".", rest')
                _           -> ("", query)
            -- Only trust this when a port marker is present; an ordinary dotted path
            -- segment (e.g. "assets.min.js/x") is not enough of a signal on its own.
            isDomainWithPort candidate = case break (== ':') candidate of
                (domainPart, ':':portDigits) ->
                    not (null portDigits)
                    && all (\c -> isDigit c || c == '*') portDigits
                    && all (\c -> isAlphaNum c || c == '-' || c == '.') domainPart
                    && length (filter (not . null) (split "." domainPart)) >= 2
                _ -> False

        postfilter :: UrlPattern -> [UrlPattern]
        postfilter url@(UrlPattern bs proto rawHost rawQuery be _) = regular ++ regex -- ++ www
            where
                -- An unanchored rule like ".domain.tld:port/path" has no "||"/"|" to force
                -- host parsing, so it's ambiguous whether the leading text is a host or
                -- plain path text; but a "domain[:port]/" prefix with an explicit port is
                -- an unambiguous signal that it's a host, so recover it here if it was
                -- otherwise left folded into the query.
                (host, query) = repairUnparsedHost rawHost rawQuery

                regex = if     proto == ""
                            && host == ""
                            && "/" `isPrefixOf` query
                            && length query > 2
                            && "/" `isSuffixOf` query
                            then
                                let query' = take (length query - 2) . drop 1 $ query
                                in [UrlPattern bs "" "" query' be True]
                            else []
                regular = let
                             leftBound = bs /= None || proto /= ""
                             rightBound = be /= None || query /= ""
                             orphanQuery = leftBound && host == "" && query /= "" && not ("*" `isPrefixOf` query)
                             duplicateHostStar = host == "*"
                             -- Do not block tld hosts, i.e. reject told-only here; n.b. independent of leftBound / rightBound to avoid partially-anchored TLD-only hosts
                             hostLabels = filter (\l -> l /= "" && l /= "*") (split "." host)
                             tldOnlyHost = host /= "" && length hostLabels < 2
                          in if not (orphanQuery || tldOnlyHost)
                             then
                                let
                                    query' = if "*" `isSuffixOf` host && query /= "" then '*' : query else query
                                in [url {_host = host, _query = query'}]
                             else []

        -- TODO: process port as an url part
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
