{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Text.Read (readMaybe)
import           Control.Monad

{-

The input on this page accepts a list of integers. Whenever the list changes
(and is successfully parsed), the list of "Hello World"s is printed. One for each
item in the list.
-}
main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  el "h1" $ text "Data test"
  let initList = [1,2,3 :: Int]
  ti <- textInput $ def & textInputConfig_initialValue .~ (T.pack . show $ initList)
  -- list which changes only when we successfully parse input
  dynList :: Dynamic t [Int] <- holdDyn initList
    . fmapMaybe (readMaybe . T.unpack)
    . _textInput_input
    $ ti
  -- show list whenever we succesfully parse it
  dynText $ T.pack . show <$> dynList
  let createHellos items = forM_ items $ \_ -> el "p" $ text "Hello World!"
  widgetHold (createHellos initList) (createHellos <$> updated dynList)
  return ()
