module UrlBlocker (
BlockMethod(..),
TaggerType(..),
urlBlock
) where
import InputParser
import Control.Applicative
import Control.Monad
import Data.List
import Data.Char (toLower)
import Data.Monoid
import OptionsConverter
import Utils 
import Control.Monad.State
import qualified Templates 
import qualified Data.Map as Map
import Data.String.Utils (split)
import Data.Maybe   
import System.IO  
import System.FilePath.Posix
import PatternConverter          

data TaggerType = Client | Server
data TaggerForwarder = Forward (Maybe Filter) String | Cancel Tagger 
data Tagger = Tagger { _taggerCode :: String, _forwarding :: [TaggerForwarder], _headerType :: HeaderType }

data ActionType   = TaggerAction Tagger | BlockAction | TerminalAction BlockMethod
data ActionSwitch = Switch Bool ActionType
data Action = Action { _actionCode :: String, _switches :: [ActionSwitch], _patterns :: [Pattern], _hasTag :: Bool }

type UrlBlockData = ([Tagger], [Action])
data BlockMethod = Request | Xframe | Elem deriving (Show, Eq)
data FilteringNode = Node { _pattern :: [Pattern], _filters :: HeaderFilters, _isNested :: Bool, 
    _policy :: Policy, _method :: BlockMethod }

class Named a where
    name :: a -> String

urlBlock :: String -> [Line] -> IO()
urlBlock path = writeBlockData path . urlBlockData 
    
writeBlockData :: String -> UrlBlockData -> IO()
writeBlockData path (taggers, actions) = 
    do writeContent (path </> "adblock.filter") "#AbBlock generated filters -- don't edit --" taggers
       writeContent (path </> "adblock.action") "#AbBlock generated actions -- don't edit --" actions

writeContent :: Show a => String -> String -> [a] -> IO()
writeContent filename header content = 
     do outFile <- openFile filename WriteMode
        hPutStrLn outFile (header ++ "\n") 
        hPutStrLn outFile $ intercalate "\n\n" $ show <$> content
        hClose outFile

urlBlockData :: [Line] -> UrlBlockData 
urlBlockData lns = mconcat [nodeResult node | node <- shortenNodes $ sortBy cmpPolicy filterNodesList ]
    where
    cmpPolicy node1 node2 = compare (_policy node1) (_policy node2)
    filterNodesList = Map.foldr (:) [] $ Map.fromListWith joinNodes $ lns >>= blockLine
        where
        blockLine (Line _ (RequestBlock policy pattern options)) 
            = [(name node, node) | node <- filteringNodes policy (errorToPattern expandedPatterns) options]
            where 
            expandedPatterns = makePattern (_matchCase options) <<$> parseUrl pattern
            errorToPattern (Left parseError) = ['#' : pattern ++ " - " ++ show parseError]
            errorToPattern (Right patterns') = patterns'
        blockLine _ = []
        joinNodes (Node patterns1 filters1 nested1 policy1 method1) 
                  (Node patterns2 _ nested2 _ _) 
            = Node (patterns1 ++ patterns2) filters1 (nested1 || nested2) policy1 method1


shortenNodes :: [FilteringNode] -> [FilteringNode]      
shortenNodes nodes = evalState (mapM shortenNode nodes) initialState
    where 
    initialState = Map.empty :: Map.Map String String
    shortenNode node = (\f -> node {_filters = f}) <$> ((mapM.mapM) shortenFilter $ _filters node)       
    shortenFilter headerFilter@(HeaderFilter headerType flt) 
        = let filterCode = _code flt 
          in do 
             dictionary <- get 
             case Map.lookup filterCode dictionary of 
                 Just shortenCode -> return $ HeaderFilter headerType flt { _code = shortenCode }
                 Nothing -> case break (=='[') filterCode of
                    (_,[]) -> return headerFilter
                    (start, rest) -> 
                        let end = last $ split "]" rest 
                            shortenCode' = start ++ (show $ Map.size dictionary + 1) ++  end 
                        in do put $ Map.insert filterCode shortenCode' dictionary
                              return $ HeaderFilter headerType flt { _code = shortenCode' }
                            

filteringNodes :: Policy -> [Pattern] -> RequestOptions -> [FilteringNode]
filteringNodes policy patterns requestOptions 
    = join $ mainResult ++ subdocumentResult ++ elemhideResult
    where 
    mainResult = optionsToNodes mainOptions $> Request
    subdocumentResult = maybeToList (optionsToNodes (singleTypeOptions Subdocument) $> Xframe)
    elemhideResult = maybeToList (optionsToNodes (singleTypeOptions Elemhide) $> Elem)
    requestType = _requestType requestOptions
    mainOptions = [requestOptions {_requestType = requestType { _positive = mainRequestTypes } }]
    mainRequestTypes = filter (/= Subdocument) <$> (_positive requestType)
    singleTypeOptions singleType = 
        do
        foundTypes <- filter (== singleType) <$> (_positive requestType)
        foundType <- listToMaybe foundTypes
        return requestOptions {_requestType = requestType { _positive = Just [foundType] } }
    optionsToNodes options = collectNodes patterns <$> headerFilters policy 2 <$> options
    collectNodes :: [Pattern] -> Maybe HeaderFilters -> BlockMethod -> [FilteringNode]
    collectNodes _ Nothing _ = [] 
    collectNodes patterns' (Just []) method = [Node patterns' [] (null patterns') policy method]
    collectNodes patterns' (Just filters@(_: next)) method
            = Node patterns' filters (null patterns') policy Request : collectNodes [] (Just next) method
          

nodeResult :: FilteringNode -> UrlBlockData
nodeResult node@(Node patterns (levelFilters : nextLevelFilters) nested policy method)
    = (taggers, (mainAction : auxActions))
    where 
    mainAction = Action { _actionCode = name node,
                          _switches   = appendIf (policy == Unblock && method == Request) 
                                            (Switch False BlockAction)
                                            (Switch True . TaggerAction <$> taggers),
                          _patterns   = patterns,
                          _hasTag     = nested } 
   
    auxActions = do forwarder <- taggers >>= _forwarding
                    case forwarder of
                       Cancel tagger -> 
                           return $ Action ('-' : name tagger) [Switch False $ TaggerAction tagger] [] True
                       _ -> mzero
    
    taggers = levelFilters >>= filterTaggers
    filterTaggers (HeaderFilter headerType@HeaderType {_typeCode = typeCode} filter'@(Filter filterCode _ orEmpty))  
        | orEmpty  = [orEmptyTagger, mainTagger [Cancel orEmptyTagger]]
        | otherwise   = [mainTagger []]
        where
        nextLevelName = filtersCode policy method nextLevelFilters
        mainTagger moreForwarders = Tagger {   _taggerCode = nextLevelName $ typeCode : filterCode,
                                               _forwarding = Forward (Just filter') (nextLevelName "") : moreForwarders,
                                               _headerType = headerType }
        orEmptyTagger             = Tagger { _taggerCode = nextLevelName ['n', typeCode],
                                             _forwarding = [Forward Nothing (nextLevelName "")],
                                             _headerType = headerType }
nodeResult node@(Node patterns [] nested policy method) = ([], [baseAction])
    where baseAction = Action (name node) [Switch (policy == Block) $ TerminalAction method] patterns nested
            
instance Named FilteringNode where
    name (Node _ filters _ policy method)  = filtersCode policy method filters "" 
    
filtersCode :: Policy -> BlockMethod -> HeaderFilters -> String -> String
filtersCode policy method [] rest 
    = join [Templates.ab2pPrefix, toLower <$> show policy, "-" ,toLower <$> show method,(if null rest then "" else "-"), rest]
filtersCode policy method (levelFilters : nextLevelFilters) rest 
    = filtersCode policy method nextLevelFilters $ join [levelCode, (if null rest then "" else "-when-"), rest]
    where 
    levelCode = (intercalate "-" $ filterCode <$> levelFilters)
    filterCode (HeaderFilter HeaderType {_typeCode = typeCode} (Filter code _ orEmpty))
        | orEmpty   = 'n' : typeCode : '-' : mainCode  
        | otherwise    = mainCode
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
              forward (Forward (Just filter') tagret) = forwardRegex headerName (_regex filter') ":" "" tagret
              forward (Forward Nothing tagret) = forwardRegex "" "" "" "" tagret
              forward (Cancel tagger) = forwardRegex headerName "" ":" "-" (name tagger)
              forwardRegex header lookahead' value tagPrefix tagret
                = let modifier 
                        | '$' `elem` lookahead' = "TDi"
                        | otherwise            = "Ti"
                  in join ["s@^", header, lookahead', value, ".*@", tagPrefix, tagret, "@", modifier] 
    
instance Named Bool where
    name True = "+"
    name False = "-"                  

instance Show ActionSwitch where
    show (Switch enable (TerminalAction method)) = Templates.terminalActionSwitch enable method
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
                
                
             
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
    
    