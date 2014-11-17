{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Life.DB.Account
   ( Account(..)
   , newAccount
   , GetAccountSkills(..)
   , AddAccountSkill(..)
   , Skill(..)
   )
where

import Data.Text
import qualified Data.Set as Set
import Data.Data (Data,Typeable)
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Data.Acid (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)

import Life.DB.Tag
import Life.DB.Skills
import Life.DB.Books

data Account = Account
   { accountUserName :: Text
   , accountSkills :: [Skill]
   } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Account)

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

$(makeAcidic ''Account ['addAccountSkill, 'getAccountSkills])
