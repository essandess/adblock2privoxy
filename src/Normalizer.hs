module Normalizer (
fixLines
) where
import InputParser
import Control.Applicative hiding (many)
import Control.Monad.State
import Data.List
import Data.String.Utils (strip)
import Utils
import PatternConverter

 
fixLines :: [Line] -> [Line]
fixLines = join . fmap fixLine

fixLine :: Line -> [Line]
fixLine  (Line text (ElementHide                  restrDom  excl        pattern)) 
       = [Line text (ElementHide (fixRestrictions restrDom) excl (strip pattern))]
 
fixLine  (Line text                     requestBlock@(RequestBlock {})) 
       =  Line text <$> fixRequestBlock requestBlock 
          where                
              fixRequestBlock (RequestBlock excl                       pattern                  options)
                                = let fixedOptions = options {
                                                     _requestType = fixRestrictions $ _requestType options,
                                                     _domain      = fixRestrictions $      _domain options
                                                     }
                                      fixedPattern = makePattern (_matchCase options) <<$> parseUrl pattern
                                  in case RequestBlock excl <<$> fixedPattern $>> fixedOptions of
                                          Right res     -> res
                                          Left  problem -> [Error $ show problem]
              fixRequestBlock _ = undefined        
          
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
        


        




            


