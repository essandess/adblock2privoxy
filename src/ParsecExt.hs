{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module ParsecExt (
    testParsecExt,
    CasesParser,
    StateParser,
    StringStateParser,
    cases
) where

import Utils
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding ((<|>),State)
import Control.Monad.Trans 
import Control.Monad.RWS
import Data.Maybe
        
---------------------------------------------------------------------------------------------
------------------------- usage samples ------------------------------------------------------
---------------------------------------------------------------------------------------------

type ExampleCase = ([String], String, String)

parsersChain :: [StringStateParser ExampleCase]
parsersChain = square3 prefix mid suffix
        where -- all parsers except for last one should consume some input and give some output
            prefix = do acc <- getState
                        setState $ Just ""
                        if isNothing acc 
                            then return [] 
                            else ((:[]) <$> (string "ab" <|> string "zz"))
            mid =    (:[]) <$> letter            -- list of letters
            suffix = many1 alphaNum <* eof             

testParsecExt :: Either ParseError [([String], String, String)]
testParsecExt =  runParser (cases parsersChain) Nothing "x" "abebz12"

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
type StringStateParser = StateParser String
type StateParser st = GenParser Char (Maybe st)
type CasesParser st r = RWST () [r] String (StateParser st) 

cases :: forall r st.(Monoid r) => [StateParser st r] -> StateParser st [r]
cases parsers =  do
                    input <- getInput
                    let boxedParser = mapRWST lookAhead $ casesParser mempty parsers
                    (input', res) <- execRWST boxedParser () input
                    setInput input'
                    return res
                                        
casesParser :: forall r st.(Monoid r) => r -> [StateParser st r] -> CasesParser st r ()
casesParser _ []                         = error "Empty parser list is not accepted"
casesParser acc parsers@(parser:next) = do
        maybeRes <- lift.optionMaybe.try $ parser
        case maybeRes of 
            Nothing -> return ()
            Just res -> do
                input <- lift getInput
                if null input || null next 
                    then do 
                            put input
                            tell [acc <> res]
                    else do 
                            st <- lift getState
                            lift (setState Nothing)
                            mapRWST lookAhead $ casesParser (acc <> res) next
                            lift (setState st)
                            casesParser (acc <> res) parsers                                      
                                        
------------------------------------------------------------------------------------------------