module Normalizer (
--opa,
fixLines
) where
import InputParser
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding (Line, (<|>))
import Control.Monad.State
import Data.List
import Data.String.Utils (strip)
import Utils
import PatternConvertor

 
fixLines :: [Line] -> [Line]
fixLines = join . fmap fixLine

fixLine :: Line -> [Line]
fixLine  (Line text (ElementHide                  restrDom  excl        pattern)) 
       = [Line text (ElementHide (fixRestrictions restrDom) excl (strip pattern))]
 
fixLine  (Line text                     requestBlock@(RequestBlock {})) 
       =  Line text <$> fixRequestBlock requestBlock 
          where    
              fixRequestBlock      (RequestBlock excl                       pattern                  options)
                             = case RequestBlock excl <<$> fixBlockPattern pattern $>> fixOptions options of
                                    Right res     -> res
                                    Left  problem -> [Error $ show problem]
              fixRequestBlock _ = undefined
                             
              fixOptions (RequestOptions                  restrRt  tp                  restrDom  mc coll dnt u) 
                        = RequestOptions (fixRestrictions restrRt) tp (fixRestrictions restrDom) mc coll dnt u 
              
              fixBlockPattern :: Pattern -> Either ParseError [Pattern]
              fixBlockPattern pattern = makePattern <<$> parseUrl pattern
                                                   
          
fixLine a = [a]

fixRestrictions :: (Eq a) => Restrictions a -> Restrictions a
fixRestrictions = annigilate.deduplicate.allowAll
        where 
        allowAll (Restrictions (Just []) n) = Restrictions Nothing n
        allowAll a = a
        deduplicate (Restrictions (Just p) n) = Restrictions (Just $ nub p) (nub n)
        deduplicate a = a
        annigilate (Restrictions (Just p) n) = 
                            let notN x = x `notElem` n
                            in Restrictions (Just $ filter notN p) n
        annigilate a = a
        


        




            


