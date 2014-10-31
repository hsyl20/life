{-# LANGUAGE OverloadedStrings #-}
module Life.Welcome
   ( welcomePage
   )
where

import Text.Blaze.Html5 ((!), Html)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5 as H

-- | Welcome page
welcomePage :: Html
welcomePage = do
   H.p "Personal life manager"
   H.a "Skill list" ! A.href "/skills"
   H.br
   H.a "A chart" ! A.href "/chart"

