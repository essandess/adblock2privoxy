module CabalTemplate (
       CabalValue,
       (#),
       (##),
       text,
       expandTemplate
)
where
import Distribution.PackageDescription
import Distribution.Text

data CabalValue = Constant String | Function (PackageDescription -> String)

infixl 1 #, ##

class CabalChainable c where
     append :: [CabalValue] -> c -> [CabalValue]

     (#) :: [[CabalValue]] -> c -> [[CabalValue]]
     (#) (line:lns) item = append line item : lns
     (#) [] item = [append [] item]

     (##) :: [[CabalValue]] -> c -> [[CabalValue]]
     (##) lns item = append [] item : lns

instance CabalChainable String where
     append line literal = Constant literal : line

instance CabalChainable (PackageDescription -> String) where
     append line getter = Function getter : line

text :: Text t => t -> String
text = show.disp

expandTemplate :: [[CabalValue]] -> PackageDescription -> String
expandTemplate template cabalMeta = (unlines.reverse) $ (concat.reverse) <$> stringified
        where
        stringified = (fmap.fmap) stringify template

        stringify (Constant s) = s
        stringify (Function f) = f cabalMeta
