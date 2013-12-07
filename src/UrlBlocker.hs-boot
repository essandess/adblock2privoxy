module UrlBlocker (
BlockMethod(..),
TaggerType(..)
) where

data BlockMethod = Request | Xframe | Elem | Dnt | Xpopup
data TaggerType = Client | Server