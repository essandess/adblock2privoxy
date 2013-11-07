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
data TaggerForwarder = Forward (Maybe Filter) String | CancelTagger String
data Tagger = Tagger { _taggerCode :: String, _forwarding :: [TaggerForwarder], _headerType :: HeaderType }

data ActionType   = TaggerAction Tagger | BlockAction | TerminalAction BlockMethod
data ActionSwitch = Switch Bool ActionType
data Action = Action { _actionCode :: String, _switches :: [ActionSwitch], _patterns :: [Pattern], _hasTag :: Bool }

data ChainType = Regular | Nested | Negate deriving (Eq, Ord)
type UrlBlockData = ([Tagger], [Action])
data BlockMethod = Request | Xframe | Elem | Dnt deriving (Show, Eq)
data FilteringNode = Node { _pattern :: [Pattern], _filters :: HeaderFilters, _nodeType :: ChainType, 
    _policy :: Policy, _method :: BlockMethod }


class Named a where
    name :: a -> String

urlBlock :: String -> [String] -> [Line] -> IO()
urlBlock path info = writeBlockData . urlBlockData 
    where    
    writeBlockData :: UrlBlockData -> IO()
    writeBlockData (taggers, actions) = 
        do writeContent (path </> "ab2p.filter") Templates.filtersFilePrefix taggers
           writeContent (path </> "ab2p.action") Templates.actionsFilePrefix actions
    writeContent filename header content = 
         do outFile <- openFile filename WriteMode
            hPutStrLn outFile (header) 
            _ <- mapM (hPutStrLn outFile) $ ('#':) <$> info
            hPutStrLn outFile $ intercalate "\n\n" $ show <$> content
            hClose outFile

urlBlockData :: [Line] -> UrlBlockData 
urlBlockData lns = filterBlockData $ result
    where
    result = mconcat [nodeResult node | node <- shortenNodes $ sortBy cmpPolicy $ filterNodesList blockLines]
    cmpPolicy node1 node2 = compare (_policy node1) (_policy node2)
    blockLines = lns >>= blockLine
        where 
        blockLine (Line position (RequestBlock policy pattern options)) 
            = filteringNodes policy (errorToPattern expandedPatterns) options
            where 
            expandedPatterns = makePattern (_matchCase options) <<$> parseUrl pattern
            sourceText = recordSourceText position 
            errorToPattern (Left parseError) = ["# ERROR: " ++ sourceText  ++ " - " ++ show parseError]
            errorToPattern (Right patterns') = ("# " ++ sourceText) : patterns'
        blockLine _ = []
    
filterNodesList :: [FilteringNode] -> [FilteringNode]
filterNodesList nodes = Map.foldr (:) [] $ Map.fromListWith joinNodes $ list
    where
    list = [(name node, node) | node <- nodes]
    joinNodes (Node patterns1 filters1 type1 policy1 method1) 
              (Node patterns2 _ type2 _ _) 
        = Node (patterns1 ++ patterns2) filters1 (max type1 type2) policy1 method1

filterBlockData :: UrlBlockData -> UrlBlockData
filterBlockData blockData = (result, snd blockData)
    where
    result = Map.foldr (:) [] $ Map.fromListWith joinTaggers taggerItems
    taggerItems = [(name tagger, tagger) | tagger <- fst blockData]
    metric = length._forwarding
    joinTaggers tagger1 tagger2 | metric tagger1 >= metric tagger2 = tagger1
                                | otherwise                        = tagger2
         
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
    = join.join $  [mainResult, subdocumentResult, elemhideResult, dntResult]
    where 
    mainResult = optionsToNodes mainOptions $> Request
    subdocumentResult = maybeToList (optionsToNodes (singleTypeOptions Subdocument) $> Xframe)
    elemhideResult = maybeToList (optionsToNodes (boolOptions _elemHide) $> Elem)
    dntResult = maybeToList (optionsToNodes (boolOptions _doNotTrack) $> Dnt)
    requestType = _requestType requestOptions
    mainOptions = [requestOptions {_requestType = requestType { _positive = mainRequestTypes } }]
    mainRequestTypes = filter (`notElem` [Subdocument, Popup]) <$> (_positive requestType)
    boolOptions getter = case getter requestOptions of
        False -> Nothing
        True  -> Just requestOptions {_requestType = Restrictions Nothing [], _thirdParty = Nothing}
    singleTypeOptions singleType = 
        do
        foundTypes <- filter (== singleType) <$> (_positive requestType)
        foundType <- listToMaybe foundTypes
        return requestOptions {_requestType = requestType { _positive = Just [foundType] } }
    optionsToNodes options = collectNodes patterns <$> headerFilters policy 2 <$> options
    nestedOrRegular True = Nested
    nestedOrRegular False = Regular
    collectNodes :: [Pattern] -> Maybe HeaderFilters -> BlockMethod -> [FilteringNode]
    collectNodes _ Nothing _ = [] 
    collectNodes patterns' (Just []) method = [Node patterns' [] (nestedOrRegular $ null patterns') policy method]
    collectNodes patterns' (Just filters@(levelFilters: next)) method
            = Node patterns' filters (nestedOrRegular $ null patterns') policy method 
              : (levelFilters >>= negateNode) 
              ++ collectNodes [] (Just next) method
        where 
        negateNode negateFilter@(HeaderFilter _ (Filter {_orEmpty = True})) 
                = [Node [] ([negateFilter] : next) Negate policy method]
        negateNode _ = [] 
          
nodeResult :: FilteringNode -> UrlBlockData
nodeResult node@(Node patterns [] nodeType policy method) = ([], [baseAction])
    where baseAction = Action (name node) [Switch (policy == Block) $ TerminalAction method] patterns (nodeType == Nested)
nodeResult node@(Node _ ([flt] : nextLevelFilters) Negate policy method)
    = ([negateTagger], [negateAction])
    where
    negateAction = Action (name node) [Switch False $ TaggerAction negateTagger] [] True
    negateTagger = newTagger flt nextLevelFilters policy method Negate []
nodeResult node@(Node patterns (levelFilters : nextLevelFilters) nodeType policy method)
    = (taggers, [action])
    where 
    action = Action { _actionCode = name node,
                      _switches   = appendIf (policy == Unblock && method == Request) 
                                        (Switch False BlockAction)
                                        (Switch True . TaggerAction <$> taggers),
                      _patterns   = patterns,
                      _hasTag     = (nodeType == Nested) }  
    taggers = filterTaggers <$> levelFilters
    filterTaggers flt@(HeaderFilter _ (Filter _ _ orEmpty))  
        = newTagger flt nextLevelFilters policy method Regular moreForwarding
        where
        orEmptyTaggerCode   = filtersCode ([flt] : nextLevelFilters) Negate  policy method ""
        moreForwarding  | orEmpty = [CancelTagger orEmptyTaggerCode]
                        | otherwise = []
            
newTagger :: HeaderFilter -> HeaderFilters -> Policy -> BlockMethod -> ChainType -> [TaggerForwarder] -> Tagger
newTagger flt@(HeaderFilter headerType filter') nextLevelFilters policy method chainType moreForwarding
   = Tagger { _taggerCode = taggerCode,
              _forwarding = Forward filter'' nextLevelActionCode : moreForwarding,
              _headerType = headerType }     
   where
   filter'' | chainType == Negate = Nothing
            | otherwise           = Just filter'
   taggerCode          = filtersCode ([flt] : nextLevelFilters) chainType policy method ""        
   nextLevelActionCode = filtersCode nextLevelFilters  Nested policy method ""   
           
instance Named FilteringNode where
    name (Node _ filters Negate policy method)  = '-' : filtersCode filters Negate policy method "" 
    name (Node _ filters _ policy method)  = filtersCode filters Nested policy method "" 
    
filtersCode :: HeaderFilters -> ChainType -> Policy -> BlockMethod -> String -> String
filtersCode [] _ policy method rest 
    = join [Templates.ab2pPrefix, toLower <$> show policy, "-" ,toLower <$> show method,(if null rest then "" else "-"), rest]
filtersCode (levelFilters : nextLevelFilters) chainType policy method rest 
    = filtersCode nextLevelFilters Nested policy method $ join [levelCode, (if null rest then "" else "-when-"), rest]
    where 
    levelCode = (intercalate "-" $ filterCode <$> levelFilters)
    filterCode (HeaderFilter HeaderType {_typeCode = typeCode} (Filter code _ orEmpty))
        | chainType == Negate            = negateCode
        | chainType == Nested && orEmpty = negateCode ++ '-' : mainCode  
        | otherwise                      = mainCode
        where 
        mainCode = typeCode : code
        negateCode = 'n' : [typeCode]

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
              forward (CancelTagger taggerCode) = forwardRegex headerName "" ":" "-" taggerCode
              forwardRegex header lookahead' value tagPrefix tagret
                = let modifier | '$' `elem` lookahead' = "TDi"
                               | otherwise             = "Ti"
                  in join ["s@^", header, lookahead', value, ".*@", tagPrefix, tagret, "@", modifier] 
    
instance Named Bool where
    name True = "+"
    name False = "-"                  

instance Show ActionSwitch where
    show (Switch enable (TerminalAction method)) = Templates.terminalActionSwitch enable method
    show (Switch enable BlockAction) = name enable ++ "block"
    show (Switch enable (TaggerAction tagger)) 
        = intercalate " \\\n " $ mainText : (_forwarding tagger >>= cancelTaggerText)
        where 
        mainText = join [name enable, name $ _taggerType $ _headerType $ tagger, "{", name tagger,  "}" ]
        cancelTaggerText (CancelTagger cancelTaggerCode) 
            = [join [name enable, name $ _taggerType $ _headerType $ tagger, "{", cancelTaggerCode,  "}" ]]
        cancelTaggerText _ = []                
    
instance Named Action where
    name = _actionCode
    
instance Show Action where
    show (Action code switches patterns hasTag)
        = intercalate "\n" (caption : switches' : patterns')
        where caption = '#' : code
              switches' = join ["{", intercalate " \\\n " (show <$> switches), " \\\n}"]
              patterns' | hasTag    = join ["TAG:^", code, "$"] : patterns
                        | otherwise = patterns  
                
                
             
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
    
    