{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Text.Read (readMaybe)
import           Control.Monad
import           Control.Arrow ((***))

{-

The input on this page accepts a list of integers and colors. Whenever the list changes
(and is successfully parsed), the list is transformed into concentric circles.
-}
main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  el "h1" $ text "Circle test"
  let initList = [(30,"red"),(20,"purple"),(10,"blue") :: (Int, T.Text)]
  ti <- textArea $ def & textAreaConfig_initialValue .~ (T.pack . show . fmap (id *** T.unpack) $ initList)
                       & textAreaConfig_attributes   .~ (constDyn $ Map.fromList [("rows", "5")])
  -- list which changes only when we successfully parse input
  dynList :: Dynamic t [(Int, T.Text)] <- holdDyn initList
    . fmap (fmap (id *** T.pack))
    . fmapMaybe (readMaybe . T.unpack)
    . _textArea_input
    $ ti
  -- show list whenever we succesfully parse it
  dynText $ T.pack . show . fmap (id *** T.unpack) <$> dynList
  let mkCircles items = forM_ items $ \(radius, color) -> do
        elSvg "circle" (constDyn $ Map.fromList
          [ ("cx","50")
          , ("cy","50")
          , ("r", (T.pack . show $ radius))
          , ("fill",color)
          ])
          (return ())
  el "div" $ elSvg "svg" (constDyn $ Map.fromList [("width", "100"), ("height", "100"), ("style", "border: 1px solid black")] ) $
    widgetHold (mkCircles initList) (mkCircles <$> updated dynList)
  return ()


svgNS :: Maybe T.Text
svgNS = Just "http://www.w3.org/2000/svg"

elSvg tag a1 a2 = do
  elDynAttrNS' svgNS tag a1 a2
  return ()
