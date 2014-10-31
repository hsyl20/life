{-# LANGUAGE OverloadedStrings #-}
module Life.Theme
   ( appTemplate
   )
where

import Text.Blaze.Html5 ((!), Html, docTypeHtml)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

-- | HTML template
appTemplate :: Html -> Html
appTemplate bdy = docTypeHtml $ do
   H.head $ do
      H.title "Personal life manager"
      H.meta ! A.httpEquiv "Content-Type"
             ! A.content "text/html;charset=utf-8"
      css
   H.body $ do
      H.h1 "Personal life manager"
      bdy


-- | CSS style
css :: Html
css = do
   H.style $ "\
      \  body {\n\
      \     font-family:monospace;\n\
      \     background-color: white;\n\
      \  }\n\
      \  h1 {\n\
      \     text-align:center;\n\    
      \  }\n"
