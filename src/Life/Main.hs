{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Life.Theme
import Life.Welcome

import Prelude hiding (writeFile)

import Control.Applicative ((<$>))
import Control.Monad (msum, forM_)
import Control.Monad.Trans.Class (lift)
import Network.Socket (withSocketsDo)
import Text.Printf
import qualified Data.ByteString.Char8 as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)

import Data.Time.LocalTime
import Data.Time.Calendar

import System.Environment

import Happstack.Server

import Text.Blaze.Html5 ((!), toHtml, Html)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

main :: IO ()
main = withSocketsDo $ do
   getArgs >>= \case
      []  -> server nullConf
      [p] -> server (nullConf { port = read p})
      _   -> putStrLn =<< (printf "Usage: %s [PORT]" <$> getProgName)

server :: Conf -> IO ()
server conf = do
   putStrLn (printf "Starting Web server at localhost:%d" (port conf))
   simpleHTTP conf $ msum [
           -- Show a loop CFG
           dir "chart" $ do
               rep <- lift $ showChart chartExample
               ok rep

           -- Show skill list
         , dir "skills" $ do
               ok . toResponse . appTemplate $ showSkills

           -- Show welcome screen
         , nullDir >> (ok . toResponse . appTemplate $ welcomePage)
      ]


skills :: [String]
skills = ["Speaking spanish", "Dancing", "Playing drums"]

-- | Show skill list
showSkills :: Html
showSkills = do
   H.h2 "Skill list"
   H.ul $ forM_ skills $ \skill -> do
      H.li $
         H.a (toHtml $ skill)
            ! A.href (H.toValue $ "/skill/" ++ skill)

-- | Render a chart in SVG and return it
showChart :: Renderable () -> IO Response
showChart chart = do
   (svg, _) <- renderableToSVG chart 800 600
   return $ toResponseBS (C.pack "image/svg+xml") (renderSvg svg)

chartExample :: Renderable ()
chartExample = toRenderable layout
   where
      layout = layout_title .~ "Evolution"
             $ layout_background .~ solidFillStyle (opaque white)
             $ layout_foreground .~ (opaque black)
             $ layout_left_axis_visibility . axis_show_ticks .~ False
             $ layout_plots .~ [ toPlot evol ]
             $ def
  
      --evol :: PlotLines Day Double
      evol = plot_lines_style .~ lineStyle
             $ plot_lines_values .~ [ datas ]
             $ plot_lines_title .~ "evolution"
             $ def

      lineStyle = line_width .~ 3 * lwidth
             $ line_color .~ opaque blue
             $ def

      lwidth = 0.5


datas :: [(LocalTime, Double)]
datas = 
   [ (mkDate 2014 10 31, 20.5)
   , (mkDate 2014 09 31, 10.5)
   ]

mkDate :: Integer -> Int -> Int -> LocalTime
mkDate y m d = LocalTime (fromGregorian y m d) (TimeOfDay 0 0 0)
