module UrlBlocker where
import InputParser
import Control.Applicative
import Control.Monad
import Data.List
import Data.Char (toLower)
import Data.Monoid
import OptionsConverter
import Utils 
import qualified Templates 
import qualified Data.Map as Map
               

data TaggerType = Client | Server
data TaggerForwarder = Forward (Maybe Filter) FilterChain | Cancel Tagger 
data Tagger = Tagger { _taggerCode :: String, _forwarding :: [TaggerForwarder], _headerType :: HeaderType }

data ActionType   = TaggerAction Tagger | BlockAction | TerminalAction
data ActionSwitch = Switch Bool ActionType
data Action = Action { _actionCode :: String, _switches :: [ActionSwitch], _patterns :: [Pattern], _hasTag :: Bool }

type UrlBlockData = ([Tagger], [Action])

class Named a where
    name :: a -> String

urlBlockData :: [Line] -> UrlBlockData 
urlBlockData lns = mconcat [chainResult node | node <- sortBy cmpPolicy filterNodesList ]
    where
    cmpPolicy node1 node2 = compare (_policy node1) (_policy node2)
    filterNodesList = Map.foldr (:) [] $ Map.fromListWith joinNodes $ lns >>= blockLine
        where
        blockLine (Line _ (RequestBlock policy pattern options)) 
            = [(name $ _chain node, node) | node <- filterNodes policy [pattern] options]
        blockLine _ = []  
        joinNodes (Node patterns1 chain1 nested1 policy1) 
                  (Node patterns2 _ nested2 _) 
            = Node (patterns1 ++ patterns2) chain1 (nested1 || nested2) policy1 
        

chainResult :: FilterNode -> UrlBlockData
chainResult (Node patterns chain@(Chain filters nextChain) nested policy)
    = (taggers, (mainAction : auxActions))
    where 
    mainAction = Action { _actionCode = name chain,
                          _switches   = appendIf (policy == Unblock) 
                                            (Switch False BlockAction)
                                            (Switch True . TaggerAction <$> taggers),
                          _patterns   = patterns,
                          _hasTag     = nested } 
   
    auxActions = do forwarder <- taggers >>= _forwarding
                    case forwarder of
                       Cancel tagger -> 
                           return $ Action ('-' : name tagger) [Switch False $ TaggerAction tagger] [] True
                       _ -> mzero
    
    taggers = filters >>= filterTaggers
    filterTaggers (HeaderFilter headerType@HeaderType {_typeCode = typeCode} filter'@(Filter filterCode _ orEmpty))  
        | orEmpty   = [orEmptyTagger, mainTagger [Cancel orEmptyTagger]]
        | otherwise = [mainTagger []]
        where
        mainTagger moreForwarders = Tagger { _taggerCode = makeName nextChain $ typeCode : filterCode,
                                             _forwarding = Forward (Just filter') nextChain : moreForwarders,
                                             _headerType = headerType }
        orEmptyTagger             = Tagger { _taggerCode = makeName nextChain $ ['n', typeCode],
                                             _forwarding = [Forward Nothing nextChain],
                                             _headerType = headerType }
chainResult (Node patterns chain nested policy) = ([], [baseAction])
    where baseAction = Action (name chain) [Switch (policy == Block) TerminalAction] patterns nested
            
instance Named FilterChain where
    name chain = makeName chain "" 
    
makeName :: FilterChain -> String -> String
makeName (Terminal policy) rest 
    = join [Templates.ab2pPrefix, toLower <$> show policy, (if null rest then "" else "-"), rest]
makeName (Chain filters next) rest 
    = makeName next $ join [filtersCode, (if null rest then "" else "-when-"), rest]
    where 
    filtersCode = (intercalate "-" $ filterCode <$> filters)
    filterCode (HeaderFilter HeaderType {_typeCode = typeCode} (Filter code _ orEmpty))
        | orEmpty   = 'n' : typeCode : '-' : mainCode  
        | otherwise = mainCode
        where mainCode = typeCode : code

instance Show TaggerType where
    show Client = "CLIENT-HEADER-TAGGER"
    show Server = "SERVER-HEADER-TAGGER"

instance Named TaggerType where
    name = fmap toLower . show

instance Named Tagger where
    name = _taggerCode

instance Show Tagger where
    show (Tagger code forwarding HeaderType {_name = headerName, _taggerType =  taggerType }) 
        = intercalate "\n" (caption : (forward <$> forwarding))
        where caption = show taggerType ++ (':' : ' ' : code)
              forward (Forward (Just filter') tagret) = foreardRegex headerName (_regex filter') ":" "" tagret
              forward (Forward Nothing tagret) = foreardRegex "" "" "" "" tagret
              forward (Cancel tagger) = foreardRegex headerName "" ":" "-" tagger
              foreardRegex header lookahead' value tagPrefix tagret
                = let modifier 
                        | '$' `elem` lookahead' = "TDi"
                        | otherwise            = "Ti"
                  in join ["s@^", header, lookahead', value, ".*@", tagPrefix, name tagret, "@", modifier] 
    
instance Named Bool where
    name True = "+"
    name False = "-"                  

instance Show ActionSwitch where
    show (Switch enable TerminalAction) = Templates.terminalActionSwitch enable
    show (Switch enable BlockAction) = name enable ++ "block"
    show (Switch enable (TaggerAction tagger)) 
        = join [name enable, name $ _taggerType $ _headerType $ tagger, "{", name tagger,  "}" ] 

instance Named Action where
    name = _actionCode
    
instance Show Action where
    show (Action code switches patterns hasTag)
        = intercalate "\n" (caption : switches' : patterns')
        where caption = '#' : code
              switches' = join ["{", intercalate " \\\n " (show <$> switches), " \\\n}"]
              patterns' 
                | hasTag    = join ["TAG:^", code, "$"] : patterns
                | otherwise = patterns  
                
                
             
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
    
    