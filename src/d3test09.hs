-- http://alignedleft.com/tutorials/d3/making-a-scatterplot

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell, RecursiveDo #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Control.Monad
import           System.Random
import           Control.Arrow ((***))
import           Text.Read (readMaybe)

import           Scales

main :: IO ()
main = do
  gen <- newStdGen
  mainWidget (bodyElement gen)

bodyElement :: MonadWidget t m => StdGen -> m ()
bodyElement gen0 = do
  let coordsCount = 10
      svgWidth = 500 :: Int
      svgHeight = 100 :: Int
      (gen1,gen2) = split gen0
      randCoords = fmap (fromIntegral *** fromIntegral) $ zip (randomRs (0,1000::Int) gen1) (randomRs (0,1000::Int) gen2)
      initList = take coordsCount randCoords
  el "h1" $ text "Scatter plot"
  -- show list whenever we regenerate random numbers
  evRegenerate <- button "Regenerate"
  evCoordsR <- mapAccum_ (\rs _ -> (drop coordsCount rs, take coordsCount rs))
                         (drop coordsCount randCoords)
                         evRegenerate
  ti <- textArea $ def & textAreaConfig_initialValue .~ (toText initList)
                       & textAreaConfig_attributes   .~ (constDyn $ Map.fromList [("rows", "5"), ("cols", "100")])
                       & textAreaConfig_setValue     .~ (toText <$> evCoordsR)
  -- list which changes only when we successfully parse input
  let evCoordsT = fmapMaybe (readMaybe . T.unpack)
        . _textArea_input
        $ ti
      -- evCoordsR does not propagate through TextArea, but we want to update
      -- the graph whenever either of them changes
      evCoords = leftmost [evCoordsT, evCoordsR]
  -- create a dot graph from random coords
  let mkDots coords = forM_ coords $ \(x0,y0) -> do
        let x1 = xScale x0
            y1 = yScale y0
            r  = rScale y0
        elSvg "circle" (constDyn $ Map.fromList
          [ ("cx", toText x1)
          , ("cy", toText y1)
          , ("r", toText r)
          ]) (return ())
        elSvg "text" (constDyn$ Map.fromList
          [ ("x", toText x1)
          , ("y", toText y1)
          , ("font-family", "sans-serif")
          , ("font-size", "11px")
          , ("fill", "red")
          ]) (text $ toText (x0,y0))
        where
          (maxX, maxY) = (maximum *** maximum) . unzip $ coords
          xScale = linearScale (0, maxX) (0, fromIntegral svgWidth)
          yScale = linearScale (0, maxY) (fromIntegral svgHeight, 0)
          rScale = linearScale (0, maxY) (2,5)

  el "div" $ do
    elSvg "svg" (constDyn $ Map.fromList [("width", toText svgWidth), ("height", toText svgHeight)]) $ do
      widgetHold (mkDots initList) (mkDots <$> evCoords)
  return ()

svgNS :: Maybe T.Text
svgNS = Just "http://www.w3.org/2000/svg"

toText :: Show a => a -> T.Text
toText = T.pack . show

elSvg tag a1 a2 = do
  elDynAttrNS' svgNS tag a1 a2
  return ()
