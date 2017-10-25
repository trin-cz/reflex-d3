{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid ((<>))

main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
    el "h1" $ text "SVG test"
    el "h2" $ text "Rectangle example"
    elSvg "svg" (constDyn $ Map.fromList [("width", "50"), ("height", "50")] ) $ do
      elSvg "rect"
        (constDyn $ Map.fromList [("x","0"), ("y","0"), ("width","50"), ("height","50"), ("fill","green")])
        (return ())
      return ()
    el "h2" $ text "Circle example"
    elSvg "svg" (constDyn $ Map.fromList [("width", "50"), ("height", "50")] ) $ do
      elSvg "circle"
        (constDyn $ Map.fromList [("cx","25"), ("cy","25"), ("r","25"), ("fill","blue")])
        (return ())
      return ()
    el "h2" $ text "Ellipse example"
    elSvg "svg" (constDyn $ Map.fromList [("width", "50"), ("height", "50")] ) $ do
      elSvg "ellipse"
        (constDyn $ Map.fromList [("cx","25"), ("cy","25"), ("rx","25"), ("ry","10"), ("fill","purple")])
        (return ())
      return ()
    el "h2" $ text "Line example"
    elSvg "svg" (constDyn $ Map.fromList [("width", "50"), ("height", "50")] ) $ do
      elSvg "line"
        (constDyn $ Map.fromList [("x1","5"), ("y1","5"), ("x2","45"), ("y2","45"), ("stroke","gray"), ("stroke-width","5")])
        (return ())
      return ()
    el "h2" $ text "Polyline example"
    elSvg "svg" (constDyn $ Map.fromList [("width", "50"), ("height", "50")] ) $ do
      elSvg "polyline"
        (constDyn $ Map.fromList [("fill","none"), ("stroke","blue"), ("stroke-width","2"), ("points","5,30 15,30 15,20 25,20 25,10 35,10")])
        (return ())
      return ()
    el "h2" $ text "Polygon example"
    elSvg "svg" (constDyn $ Map.fromList [("width", "50"), ("height", "50")] ) $ do
      elSvg "polygon"
        (constDyn $ Map.fromList [("fill","yellow"), ("stroke","blue"), ("stroke-width","2"), ("points","05,30 15,10 25,30")])
        (return ())
      return ()
    return ()

svgNS :: Maybe T.Text
svgNS = Just "http://www.w3.org/2000/svg"

elSvg tag a1 a2 = do
  elDynAttrNS' svgNS tag a1 a2
  return ()
