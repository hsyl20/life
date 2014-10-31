module Life.Chart
   ( showChart
   )
where

import qualified Data.ByteString.Char8 as C
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import Happstack.Server

-- | Render a chart in SVG and return it
showChart :: Renderable () -> IO Response
showChart chart = do
   (svg, _) <- renderableToSVG chart 800 600
   return $ toResponseBS (C.pack "image/svg+xml") (renderSvg svg)

