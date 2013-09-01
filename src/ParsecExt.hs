{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module ParsecExt (
    --testParsecExt,
    CasesParser,
    Parser,
    cases
) where

import Utils
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding ((<|>),State)
import Control.Monad.Trans 
import Control.Monad.RWS
        
---------------------------------------------------------------------------------------------
------------------------- usage samples ------------------------------------------------------
---------------------------------------------------------------------------------------------

type ExampleCase = ([String], String, String)

-- auto way
parsersChain' :: [ExampleCase -> Parser ExampleCase]
parsersChain' = undefined

parsersChainElem :: String -> Parser String
parsersChainElem acc = if length acc > 3 then pzero else string "d"

-- manual way

parsersChainElem' :: ExampleCase -> Parser ExampleCase
parsersChainElem' (_,_,acc) = (\x -> (mempty, mempty, x)) <$> (if length acc > 3 then pzero else string "s") 


--parsersChain :: [Parser ExampleCase]
--parsersChain = bb ((:[]) <$> prefix) mid suffix
--        where -- all parsers except for last one should consume some input and give some output
--            bb = square3
--            
--            prefix = string "ab" <|> string "zz" -- list of sequental tokens
--            mid =    (:[]) <$> letter            -- list of letters
--            suffix = many1 alphaNum <* eof              -- just string. Last pattern should be applied once and can deal with empty input/output

--testParsecExt :: Either ParseError [([String], String, String)]
--testParsecExt =  parse (cases parsersChain) "x" "abebz12"





---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------

--type StateParser = GenParser Char st
type CasesParser r = RWST () [r] String Parser 

cases :: (Monoid r) => [r -> Parser r] -> Parser [r]
cases parsers =  do
                    input <- getInput
                    let boxedParser = mapRWST lookAhead $ casesParser mempty parsers
                    (input', res) <- execRWST boxedParser () input
                    setInput input'
                    return res
                                        
casesParser :: (Monoid r) => r -> [r -> Parser r] -> CasesParser r ()
casesParser _ []                         = error "Empty parser list is not accepted"
casesParser acc parsers@(parserFun:next) = do
        maybeRes <- lift.optionMaybe.try $ parserFun acc
        case maybeRes of 
            Nothing -> return ()
            Just res -> do
                input <- lift getInput
                if null input || null next 
                    then do 
                            put input
                            tell [acc <> res]
                    else do 
                            mapRWST lookAhead $ casesParser (acc <> res) next
                            casesParser (acc <> res) parsers                                      
                                        
------------------------------------------------------------------------------------------------