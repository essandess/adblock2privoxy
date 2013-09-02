module ParserExtTests (
parseMorse,
encodeMorse
) where
import Text.ParserCombinators.Parsec

parseMorse :: String -> Either ParseError [String]
encodeMorse :: String -> String
