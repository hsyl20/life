{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Life.DB.Skills
   ( Skill(..)
   )
where

import Data.Text
import Data.Set (Set)
import Data.Data (Data,Typeable)

import Data.SafeCopy (base, deriveSafeCopy)

import Life.DB.Tag


data Skill = Skill
   { skillName :: Text
   , skillTags :: Set Tag
   } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Skill)

