{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Data.Fixed
import Data.Foldable
import Data.List
import Data.Maybe
import GHCJS.Concurrent
import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM.Document hiding (getRootElement)
import GHCJS.DOM.Element
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Node
import GHCJS.DOM.Window hiding (print)
import JavaScript.Web.AnimationFrame

#if defined(ghcjs_HOST_OS)
run :: a -> a
run = id
#elif defined(MIN_VERSION_jsaddle_wkwebview)
import Language.Javascript.JSaddle.WKWebView (run)
#else
import Language.Javascript.JSaddle.WebKitGTK (run)
#endif

data Rectangle = Rectangle
               { x        :: !Int
               , y        :: !Int
               , width    :: !Int
               , height   :: !Int
               } deriving (Eq, Ord, Show)

data Colour = Colour
            { red   :: !Int
            , green :: !Int
            , blue  :: !Int
            , alpha :: !Double
            } deriving (Eq, Ord, Show)

data View = View
          { frame     :: !Rectangle
          , children  :: ![View]
          , fill      :: !Colour
          } deriving (Eq, Ord, Show)

maybeZip :: [a] -> [a] -> [(Int, Maybe a, Maybe a)]
maybeZip !l !r =
  let maybeZipAt :: Int -> [a] -> [a] -> [(Int, Maybe a, Maybe a)]
      maybeZipAt _ [] [] = []
      maybeZipAt index (l : ls) [] = (index, Just l, Nothing) : (maybeZipAt (index + 1) ls [])
      maybeZipAt index [] (r : rs) = (index, Nothing, Just r) : (maybeZipAt (index + 1) [] rs)
      maybeZipAt index (l : ls) (r : rs) = (index, Just l, Just r) : (maybeZipAt (index + 1) ls rs)
  in  maybeZipAt 0 l r

colourToCSS :: Colour -> String
colourToCSS (Colour !r !g !b !a) = "rgba(" ++ (show r) ++ "," ++ (show g) ++ "," ++ (show b) ++ "," ++ (show a) ++ ")"

rectangleToCSS :: Rectangle -> [(String, String)]
rectangleToCSS (Rectangle !l !t !w !h) = [("position", "relative"), ("left", (show l) ++ "px"), ("top", (show t) ++ "px"), ("width", (show w) ++ "px"), ("height", (show h) ++ "px")]

viewStyleCSS :: View -> [(String, String)]
viewStyleCSS (View !frame _ !fill) = [("background-color", colourToCSS fill), ("overflow", "hidden")] ++ rectangleToCSS frame

applyAttributes :: Element -> [(String, String)] -> JSM ()
applyAttributes elem styleAttributes = do
  let styleAttribute = foldMap (\(key, value) -> key ++ ": " ++ value ++ "; ") styleAttributes
  setAttribute elem "style" styleAttribute  

elementWithStyle :: Document -> String -> [(String, String)] -> [(String, String)] -> JSM Element
elementWithStyle doc elemName styleAttributes otherAttributes = do
  elem <- createElement doc elemName
  applyAttributes elem styleAttributes
  traverse_ (\(k, v) -> setAttribute elem k v) otherAttributes
  return elem

getRootElement :: Document -> JSM Element
getRootElement doc = do
  rootElem <- getElementById doc "root"
  case rootElem of
    Just r -> return r
    Nothing -> do 
      elem <- elementWithStyle doc "div" (rectangleToCSS (Rectangle 0 0 10000 10000)) [("id", "root")]
      body <- getBodyUnsafe doc
      appendChild_ body elem
      return elem

buildKey :: [Int] -> Int -> String
buildKey !parentIndexes !index = intercalate "/" $ fmap show (index : parentIndexes)      

deleteElement :: Document -> [Int] -> Int -> JSM ()
deleteElement !doc !parentIndexes !index = do
  currentElement <- getElementByIndex doc parentIndexes index
  forM_ currentElement $ \e -> do
    possibleParent <- getParentNode e
    forM_ possibleParent $ \p -> removeChild_ p e

getElementByIndex :: Document -> [Int] -> Int -> JSM (Maybe Element)
getElementByIndex !doc !parentIndexes !index = getElementById doc $ buildKey parentIndexes index

getElementByIndexUnsafe :: Document -> [Int] -> Int -> JSM Element
getElementByIndexUnsafe !doc !parentIndexes !index = do
  elem <- getElementByIndex doc parentIndexes index
  maybe (error "Failure!!!") return elem

buildChildren :: Document -> [Int] -> Int -> Maybe View -> View -> JSM ()
buildChildren !doc !parentIndexes !index !old !new = do
  traverse_ (\(subIndex, subOld, subNew) -> buildElements doc (index : parentIndexes) subIndex subOld subNew) $ maybeZip (maybe [] children old) (children new)  

createAnElement :: Document -> [Int] -> Int -> Maybe View -> View -> JSM ()
createAnElement !doc !parentIndexes !index !old !new = do
  elem <- elementWithStyle doc "div" (viewStyleCSS new) [("id", buildKey parentIndexes index)]
  appendTo <- maybe (getRootElement doc) (\(first, rest) -> getElementByIndexUnsafe doc rest first) $ uncons parentIndexes
  appendChild_ appendTo elem
  buildChildren doc parentIndexes index old new

sameAttributes :: View -> View -> Bool
sameAttributes (View !oldFrame _ !oldFill) (View !newFrame _ !newFill) = oldFrame == newFrame && oldFill == newFill

updateElement :: Document -> [Int] -> Int -> View -> View -> JSM ()
updateElement !doc !parentIndexes !index !old !new = do
  elem <- getElementByIndexUnsafe doc parentIndexes index
  unless (sameAttributes old new) $ applyAttributes elem $ viewStyleCSS new
  buildChildren doc parentIndexes index (Just old) new

buildElements :: Document -> [Int] -> Int -> Maybe View -> Maybe View -> JSM ()
buildElements _ _ _ Nothing Nothing = return ()
buildElements !doc !parentIndexes !index (Just _) Nothing = deleteElement doc parentIndexes index
buildElements !doc !parentIndexes !index Nothing (Just !new) = createAnElement doc parentIndexes index Nothing new
buildElements !doc !parentIndexes !index (Just !old) (Just !new) = updateElement doc parentIndexes index old new

runFrame :: Document -> Maybe View -> Double -> JSM ()
runFrame !doc !lastView !timestamp = do
  let latestView = Just $ mainView timestamp
  buildElements doc [] 0 lastView latestView
  _ <- liftDOM $ inAnimationFrame ContinueAsync $ runFrame doc latestView
  return ()

mainView :: Double -> View
mainView !timestamp = View (Rectangle 0 0 800 800) (viewsLevel1 timestamp) (Colour 255 255 255 1)

viewsLevel1 :: Double -> [View]
viewsLevel1 !timestamp = do
  x <- [10, 20..100]
  y <- [10, 20..100]
  return $ View (Rectangle x y 400 800) (viewsLevel2 timestamp) (Colour 255 0 0 1)

viewsLevel2 :: Double -> [View]
viewsLevel2 !timestamp = do
  x <- [5, 10..20]
  y <- [5, 10..20]
  return $ View (Rectangle (x + (round (mod' (timestamp / 50) 50))) y 50 50) [] (Colour 0 0 255 1)

main :: IO ()
main = run fastDOM

fastDOM :: JSM ()
fastDOM = do
  doc <- currentDocumentUnchecked
  _ <- inAnimationFrame ContinueAsync $ runFrame doc Nothing
  return ()
