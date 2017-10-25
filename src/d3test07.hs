-- http://alignedleft.com/tutorials/d3/making-a-bar-chart

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell, RecursiveDo #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Control.Monad
import           System.Random

main :: IO ()
main = do
  gen <- newStdGen
  mainWidget (bodyElement gen)

bodyElement :: MonadWidget t m => StdGen -> m ()
bodyElement gen = do
  let randList = randomRs(0,100::Int) $ gen
      initList = take 20 randList
  el "h1" $ text "Bar chart"
  -- show list whenever we regenerate random numbers
  evRegenerate <- button "Regenerate"
  evNumbers <- mapAccum_ (\rs _ -> (drop 20 rs, take 20 rs))
                         (drop 20 randList)
                         evRegenerate
  dynList :: Dynamic t [Int] <- holdDyn initList evNumbers
  dynText $ toText <$> dynList
  -- create a bar graph from random numbers
  let barWidth = 20 :: Int
      barMargin = 2 :: Int
      svgWidth = 500 :: Int
      svgHeight = 100 :: Int
  let mkBars xs = forM_ (zip [0..] xs) $ \(index, value) -> do
        elSvg "rect" (constDyn $ Map.fromList
          [ ("x", toText $ (barWidth + barMargin) * index)
          -- reverse the Y coordinate, so that bars grow up from bottom
          , ("y", toText $ svgHeight - value)
          , ("width", toText barWidth)
          , ("height", toText value)
          , ("fill", "rgb(0, 0, " <> (toText (floor $ fromIntegral value * (2.55::Double) :: Int)) <> ")")
          ]) (return ())
        elSvg "text" (constDyn$ Map.fromList
          [ ("x", toText $ index * (barWidth + barMargin) + 10)
          , ("y", toText $ svgHeight - value + 15)
          , ("font-family", "sans-serif")
          , ("font-size", "11px")
          , ("fill", "white")
          , ("text-anchor", "middle")
          ]) (text $ toText value)

  el "div" $ do
    elSvg "svg" (constDyn $ Map.fromList [("width", toText svgWidth), ("height", toText svgHeight)]) $ do
      widgetHold (mkBars initList) (mkBars <$> evNumbers)
  return ()

svgNS :: Maybe T.Text
svgNS = Just "http://www.w3.org/2000/svg"

toText :: Show a => a -> T.Text
toText = T.pack . show

elSvg tag a1 a2 = do
  elDynAttrNS' svgNS tag a1 a2
  return ()
