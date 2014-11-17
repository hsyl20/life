{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Life.Theme
import Life.Welcome
import Life.Chart
import Life.DB.Account
import Life.DB.Skills
import Life.DB.Books

import Prelude hiding (writeFile)

import Control.Applicative ((<$>))
import Control.Monad (msum, forM, forM_)
import Control.Monad.Trans.Class (lift)
import Control.Exception (bracket)
import Network.Socket (withSocketsDo)
import Text.Printf
import Data.Text

import Data.Time.LocalTime
import Data.Time.Calendar

import System.Environment
import System.Exit

import Happstack.Server

import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid (AcidState, openLocalState)
import Data.Acid.Advanced (query', update')

import Text.Blaze.Html5 ((!), toHtml, Html)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

main :: IO ()
main = withSocketsDo $ do
   -- Get port
   p <- getArgs >>= \case
      []  -> return 8000
      [x] -> return (read x)
      _   -> do
         putStrLn =<< (printf "Usage: %s [PORT]" <$> getProgName)
         exitSuccess
   -- Launch server
   bracket 
      (openLocalState newAccount)
      (createCheckpointAndClose)
      (\db -> server db (nullConf { port = p}))

server :: AcidState Account -> Conf -> IO ()
server db conf = do
   putStrLn (printf "Starting Web server at localhost:%d" (port conf))
   simpleHTTP conf $ msum [
           -- Show a loop CFG
           dir "chart" $ do
               rep <- lift $ showChart (chartExample datas)
               ok rep

           -- Show skill list
         , dir "skills" $ do
               showSkills db

           -- Show welcome screen
         , nullDir >> (ok . toResponse . appTemplate $ welcomePage)
      ]


-- | Show skill list
showSkills :: AcidState Account -> ServerPartT IO Response
showSkills db = do
   skillNames <- fmap skillName <$> query' db GetAccountSkills
   
   ok . toResponse . appTemplate $ do
      H.h2 "Skill list"
      H.ul $ forM_ skillNames $ \skill -> do
         H.li $
            H.a (toHtml $ skill)
               ! A.href (H.toValue $ "/skill/" ++ unpack skill)


datas :: [(LocalTime, Double)]
datas = 
   [ (mkDate 2014 10 31, 20.5)
   , (mkDate 2014 09 31, 10.5)
   ]

mkDate :: Integer -> Int -> Int -> LocalTime
mkDate y m d = LocalTime (fromGregorian y m d) (TimeOfDay 0 0 0)
