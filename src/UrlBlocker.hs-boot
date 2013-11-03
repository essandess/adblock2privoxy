module UrlBlocker (
BlockMethod(..),
TaggerType(..)
) where

data BlockMethod = Request | Xframe | Elem
data TaggerType = Client | Server