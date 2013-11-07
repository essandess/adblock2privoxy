module UrlBlocker (
BlockMethod(..),
TaggerType(..)
) where

data BlockMethod = Request | Xframe | Elem | Dnt
data TaggerType = Client | Server