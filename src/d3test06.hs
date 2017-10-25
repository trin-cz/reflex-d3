
-- http://alignedleft.com/tutorials/d3/the-power-of-data

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell, RecursiveDo #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Control.Monad
import           System.Random
import           Data.FileEmbed

main :: IO ()
main = do
  gen <- newStdGen
  mainWidgetWithCss css (bodyElement gen)
  where css = $(embedFile "css/d3test06.css")

bodyElement :: MonadWidget t m => StdGen -> m ()
bodyElement gen = do
  let randList = randomRs(0,100::Int) $ gen
      initList = take 20 randList
  el "h1" $ text "Data test"
  -- show list whenever we regenerate random numbers
  evRegenerate <- button "Regenerate"
  evNumbers <- mapAccum_ (\rs _ -> (drop 20 rs, take 20 rs))
                         (drop 20 randList)
                         evRegenerate
  dynList :: Dynamic t [Int] <- holdDyn initList evNumbers
  dynText $ toText <$> dynList
  -- create a bar graph from random numbers
  let mkBars xs = forM_ xs $ \height ->
        elDynAttr "div" (constDyn $ Map.fromList
          [ ("class", "bar")
          , ("style", "height: " <> (toText height) <> "px")
          ]) (return ())
  el "div" $ do
    widgetHold (mkBars initList) (mkBars <$> evNumbers)
  return ()

toText :: Show a => a -> T.Text
toText = T.pack . show
