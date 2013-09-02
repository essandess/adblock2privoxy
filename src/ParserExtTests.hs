module ParserExtTests (
parseMorse,
encodeMorse
) where
import Utils
import ParsecExt
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding ((<|>),State)
import Control.Monad
import Data.List
import Data.Maybe
import Control.Monad.State
import Debug.Trace

--------------------------------------------------------------------------------
--- Morse chars parsing: parse "......-...-..---" in all possible ways ---------
--------------------------------------------------------------------------------

morseChars :: [(String, Char)]
morseChars = [  (".-", 'A'), 
                ("-...", 'B'),
                ("-.-.", 'C'), 
                ("-..", 'D'),
                (".", 'E'),   
                ("..-.", 'F'),
                ("--.", 'G'), 
                ("....", 'H'),
                ("..", 'I'),  
                (".---", 'J'),
                ("-.-", 'K'), 
                (".-..", 'L'),
                ("--", 'M'),  
                ("-.", 'N'),
                ("---", 'O'), 
                (".--.", 'P'),
                ("--.-", 'Q'),    
                (".-.", 'R'),
                ("...", 'S'), 
                ("-", 'T'),
                ("..-", 'U'), 
                ("...-", 'V'),
                (".--", 'W'), 
                ("-..-", 'X'),
                ("-.--", 'Y'),    
                ("--..", 'Z'),
                ("-----", '0'),   
                (".----", '1'),
                ("..---", '2'),   
                ("...--", '3'),
                ("....-", '4'),   
                (".....", '5'),
                ("-....", '6'),   
                ("--...", '7'),
                ("---..", '8'),   
                ("----.", '9')]

morseCharCodes :: [String]
morseCharCodes = fst <$> morseChars

-- HELLO = "......-...-..---"
encodeMorse :: String -> String
encodeMorse s = join $ fst <$> catMaybes (code <$> s)
        where code c = find (\pair -> snd pair == c) morseChars
        
decodeMorse :: [String] -> String
decodeMorse ss = snd <$> catMaybes (code <$> ss)
        where code s = find (\pair -> fst pair == s) morseChars 


-- find possibilites to continue from a given prefix
findMorseSteps :: String -> [String] -> [String]
findMorseSteps prefix codes = case find (== prefix) codes of
                            Nothing -> case filter (isPrefixOf prefix) codes of 
                                            [] -> []
                                            filtered ->    findMorseSteps (prefix ++ ".") filtered
                                                        ++ findMorseSteps (prefix ++ "-") filtered
                            Just match -> [match]

morseStepParser :: [String] -> Parser String
morseStepParser [] = pzero
morseStepParser [step] = string step
morseStepParser (step:steps') = string step <|> morseStepParser steps'

morseParser :: Int -> StringStateParser (ZipListM String)
morseParser pos = do     acc' <- get
                         let acc = case acc' of
                                      Nothing -> ""
                                      Just val -> val
                             candidates = filter (\x -> isPrefixOf acc x && acc /= x) morseCharCodes
                             steps = drop (length acc) <$> findMorseSteps acc candidates
                             parser = morseStepParser steps    
                         res <- lift parser
                         put (Just $ acc ++ res)
                         return (zipListM $ (replicate pos "") ++ (res : repeat ""))
                  

morseParsers :: [StringStateParser (ZipListM String)]
morseParsers = (repeat morseParser) <*> [0..]

parseMorse :: String -> Either ParseError [String]
parseMorse s = fmap (filter $ isPrefixOf "HELL") $ (fmap.fmap) postProcess $ parseMorseRaw "x" s
            where 
            parseMorseRaw =  parse (cases $ morseParsers) 
            postProcess = decodeMorse.toLists 
            toLists = (takeWhile $ not.null) . getZipListM 
            