module ParsecExt (
testParsecExt,
CasesParser,
MyParser,
cases
) where
import Utils
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding ((<|>),State)
import Control.Monad.Trans 
import Control.Monad.Writer
        
---------------------------------------------------------------------------------------------        
        
-- TODO:                   add optional sections

---------------------------------------------------------------------------------------------
------------------------- usage sample ------------------------------------------------------
---------------------------------------------------------------------------------------------

prefixSuffixChain :: [MyParser ([String], String, String)]
prefixSuffixChain = square3 ((:[]) <$> prefix) mid suffix
        where
            prefix = string "a" <|> string "z" -- list of sequental tokens
            mid =    option "\NUL" ((:[]) <$> letter) -- optional section
            suffix = many alphaNum               -- just string

testParsecExt :: Either ParseError ([([String], String, String)], String)
testParsecExt =  parse ((,) <$> cases prefixSuffixChain <*> string "$$$") "x" "aaz12$$$"

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------

type MyParser a = GenParser Char () a
type CasesParser r = WriterT [r] (GenParser Char ()) 

cases :: (Monoid r) => [MyParser r] -> MyParser [r]
cases parsers = execWriterT (mapWriterT lookAhead $ casesParser mempty 0 parsers)
                        <* consumingParser parsers

consumingParser :: (Monoid r) => [MyParser r] -> MyParser ()
consumingParser = foldl addParser (return ()) 
    where 
        addParser res x = res <* many' x
        many' parser = do 
                            posBefore <- getPosition
                            maybeRes <- optionMaybe.try $ parser
                            posAfter <- getPosition
                            case maybeRes of 
                                Nothing -> return ()
                                Just _ -> when (posBefore /= posAfter) $ many' parser
                                        
casesParser :: (Monoid r) => r -> Int -> [MyParser r] -> CasesParser r ()
casesParser acc _ [] = do
                      tell [acc]
                      return ()
casesParser acc rep parsers@(parser:next) = do
                        posBefore <- lift getPosition
                        maybeRes <- lift.optionMaybe.try $ parser
                        posAfter <- lift getPosition
                        case maybeRes of 
                            Nothing -> return ()
                            Just res -> do
                                        let acc' = acc <> res
                                        when (posBefore /= posAfter || rep == 0) $ mapWriterT lookAhead $ casesParser acc' 0 next
                                        when (posBefore /= posAfter) $ casesParser acc' (rep + 1) parsers
                                        
------------------------------------------------------------------------------------------------