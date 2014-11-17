{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Life.DB.Tag
   ( Tag(..)
   )
where

import Data.Data (Data,Typeable)

import Data.SafeCopy (base, deriveSafeCopy)

data Tag = Tag
   { tagName :: String
   } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Tag)

