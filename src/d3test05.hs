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
  let initList = [(40,"red"),(100,"purple"),(150,"blue") :: (Int, T.Text)]
      initSvgSize = 200 :: Int
  ti <- textArea $ def & textAreaConfig_initialValue .~ (T.pack . show . fmap (id *** T.unpack) $ initList)
                       & textAreaConfig_attributes   .~ (constDyn $ Map.fromList [("rows", "5"), ("cols", "100")])
  -- list which changes only when we successfully parse input
  dynList :: Dynamic t [(Int, T.Text)] <- holdDyn initList
    . fmap (fmap (id *** T.pack))
    . fmapMaybe (readMaybe . T.unpack)
    . _textArea_input
    $ ti
  let dynSvgSize = max initSvgSize . maximum . fmap fst <$> dynList
  -- show list whenever we succesfully parse it
  dynText $ T.pack . show . fmap (id *** T.unpack) <$> dynList
  let mkCircles (items, svgSize) = forM_ items $ \(position, color) -> do
        elSvg "circle" (constDyn $ Map.fromList
          [ ("cx", toText position)
          , ("cy", toText $ svgSize - position)
          , ("r", "30")
          , ("fill",color)
          ])
          (return ())
  let dynSvgAttr = (flip fmap dynSvgSize $ \svgSize -> Map.fromList
        [ ("width", toText svgSize)
        , ("height", toText svgSize)
        , ("style", "border: 1px solid black")
        ])

  el "div" $ elSvg "svg" dynSvgAttr $
    widgetHold (mkCircles (initList,initSvgSize)) (mkCircles <$> updated (zipDyn dynList dynSvgSize))
  return ()

svgNS :: Maybe T.Text
svgNS = Just "http://www.w3.org/2000/svg"

toText :: Show a => a -> T.Text
toText = T.pack . show

elSvg tag a1 a2 = do
  elDynAttrNS' svgNS tag a1 a2
  return ()
