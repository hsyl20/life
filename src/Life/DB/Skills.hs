{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Life.DB.Skills
   ( Account(..)
   , newAccount
   , GetAccountSkills(..)
   , AddAccountSkill(..)
   , AccountSkillNames(..)
   , Skill(..)
   , GetSkillName(..)
   , Tag(..)
   )
where

import Data.Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Data (Data,Typeable)
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Data.Acid (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)


data Tag = Tag
   { tagName :: String
   } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Tag)

data Skill = Skill
   { skillName :: Text
   , skillTags :: Set Tag
   } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Skill)

getSkillName :: Query Skill Text
getSkillName = skillName <$> ask

$(makeAcidic ''Skill ['getSkillName])

data Account = Account
   { accountUserName :: Text
   , accountSkills :: [Skill]
   } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Account)


accountSkillNames :: Query Account [Text]
accountSkillNames = (fmap skillName . accountSkills) <$> ask

-- | Create a new account
newAccount :: Account
newAccount = Account "Victor Hugo" [Skill "Learning Spanish" (Set.fromList [Tag "Language"])]


-- | Get account skills
getAccountSkills :: Query Account [Skill]
getAccountSkills = accountSkills <$> ask

-- | Add a new skill
addAccountSkill :: Skill -> Update Account ()
addAccountSkill s = do
   acc <- get
   let sk = accountSkills acc
   put (acc { accountSkills = s:sk })

$(makeAcidic ''Account ['addAccountSkill, 'getAccountSkills, 'accountSkillNames])
