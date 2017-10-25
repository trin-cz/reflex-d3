-- http://alignedleft.com/tutorials/d3/making-a-scatterplot

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
bodyElement gen0 = do
  let coordsCount = 10
      (gen1,gen2) = split gen0
      randCoords = zip (randomRs (0,500::Int) gen1) (randomRs (0,100::Int) gen2)
      initList = take coordsCount randCoords
  el "h1" $ text "Scatter plot"
  -- show list whenever we regenerate random numbers
  evRegenerate <- button "Regenerate"
  evCoords <- mapAccum_ (\rs _ -> (drop coordsCount rs, take coordsCount rs))
                        (drop coordsCount randCoords)
                        evRegenerate
  dynList :: Dynamic t [(Int,Int)] <- holdDyn initList evCoords
  dynText $ toText <$> dynList
  -- create a bar graph from random numbers
  let svgWidth = 500 :: Int
      svgHeight = 100 :: Int
  let mkDots xys = forM_ xys $ \(x,y) -> do
        elSvg "circle" (constDyn $ Map.fromList
          [ ("cx", toText x)
          , ("cy", toText y)
          , ("r", toText (sqrt . fromIntegral $ svgHeight - y :: Double))
          ]) (return ())
        elSvg "text" (constDyn$ Map.fromList
          [ ("x", toText x)
          , ("y", toText y)
          , ("font-family", "sans-serif")
          , ("font-size", "11px")
          , ("fill", "red")
          ]) (text $ toText (x,y))

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
