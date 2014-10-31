module Life.Chart
   ( showChart
   , chartExample
   )
where

import qualified Data.ByteString.Char8 as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)

import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import Data.Time.LocalTime

import Happstack.Server

-- | Render a chart in SVG and return it
showChart :: Renderable () -> IO Response
showChart chart = do
   (svg, _) <- renderableToSVG chart 800 600
   return $ toResponseBS (C.pack "image/svg+xml") (renderSvg svg)

chartExample :: [(LocalTime,Double)] -> Renderable ()
chartExample datas = toRenderable layout
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

