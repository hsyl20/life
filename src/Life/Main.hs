import Prelude hiding (writeFile)

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Backend.Types
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import Data.ByteString.Lazy (writeFile)

import Data.Time.LocalTime
import Data.Time.Calendar

main :: IO ()
main = do
   (svg, _) <- renderableToSVG chart 800 600
   writeFile "test.svg" (renderSvg svg)
   return ()

chart = toRenderable layout
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

mkDate y m d = LocalTime (fromGregorian y m d) (TimeOfDay 0 0 0)

--instance Default Day where
--   def = fromGregorian 1986 03 14
