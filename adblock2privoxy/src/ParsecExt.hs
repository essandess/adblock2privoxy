module ParsecExt (
    CasesParser,
    StateParser,
    StringStateParser,
    cases,
    manyCases,
    many1Cases,
    oneCase
) where

import Utils
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding ((<|>),State)
import Control.Monad.Trans 
import Control.Monad.RWS
import Control.Monad.State
import Data.Maybe
        
-- parser should consume some input to prevent infinite loop
manyCases :: (Monoid a, Monoid st) => Parser a -> StateParser st a
manyCases p = do    acc <- get
                    put  $ Just mempty
                    lift $ if isNothing acc 
                              then return mempty 
                              else p
                        
oneCase :: (Monoid a, Monoid st) => Parser a -> StateParser st a
oneCase p = do  acc <- get
                put  $ Just mempty
                lift $ if isNothing acc 
                          then p
                          else pzero

many1Cases :: Parser a -> StateParser st a
many1Cases = lift

type StringStateParser = StateParser String
type StateParser st = StateT (Maybe st) Parser
type CasesParser st r = RWST () [r] String (StateParser st)

optionMaybeTry :: StateParser st a -> StateParser st (Maybe a)
optionMaybeTry p = liftM Just (mapStateT try p) <|> return Nothing

cases :: forall r st.(Monoid r) => [StateParser st r] -> Parser [r]
cases parsers =  evalStateT stateParser Nothing
            where stateParser =  do
                    input <- lift getInput
                    let boxedParser = (mapRWST.mapStateT) lookAhead $ casesParser mempty parsers
                    (input', res) <- execRWST boxedParser () input  
                    lift (setInput input')
                    return res
                 
                                        
casesParser :: forall r st.(Monoid r) => r -> [StateParser st r] -> CasesParser st r ()
casesParser _ []                         = error "Empty parser list is not accepted"
casesParser acc parsers@(parser:next) = do
        maybeRes <- lift (optionMaybeTry parser)
        case maybeRes of 
            Nothing -> return ()
            Just res -> do
                input <- lift.lift $ getInput
                let acc' = acc <> res
                if null input || null next 
                        then do
                            modify (minList input) -- TODO: somehow use processed length to select min input
                            tell [acc']
                        else do 
                            st <- lift get
                            lift (put Nothing)
                            (mapRWST.mapStateT) lookAhead $ casesParser acc' next
                            lift (put st)
                unless (null input) $ casesParser acc' parsers                                      
                                        
------------------------------------------------------------------------------------------------