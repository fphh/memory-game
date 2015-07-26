{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where


import Data.Colour.SRGB

import Blaze.React

import qualified Data.Text as Text

import qualified Text.Blaze.Event                     as E
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A
import qualified Blaze.React.Run.ReactJS as ReactJS

import Data.HashMap.Strict (fromList)

import qualified Control.Monad.State as St
import qualified Control.Monad.Writer as Wr
import Control.Concurrent
import Control.Monad (unless)

import System.Random.Shuffle


import Text.Printf (printf)

colors :: [String]
colors = ["#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059", "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87","#5A0007", "#D16100", "#BA0900"]

white :: String
white = sRGB24show $ (sRGB24 255 255 255 :: Colour Double)

grey,gray :: String
grey = sRGB24show $ (sRGB24 160 160 160 :: Colour Double)
gray = grey

type Color = String

data CardState = NotVisible | Visible | Found deriving (Eq, Show)

data Card = Card {
  index :: Int,
  color :: Color,
  cardState :: CardState
  } deriving (Eq, Show)

data GameState =
     ZeroCards
     | OneCard Card
     | TwoCards
     deriving (Eq, Show)

data AppConfig = AppConfig {
  numberOfRows :: Int
  } deriving (Show)

newtype Turns = Turns { unTurns :: Int } deriving (Show, Num)
newtype PairsFound = PairsFound { unPairsFound :: Int } deriving (Show, Num)

data AppState = AppState {
  config :: AppConfig,
  gstate :: GameState,
  found :: [Card],
  cards :: [Card],
  numberOfTurns :: Turns,
  pairsFound :: PairsFound,
  scores :: [Double]
  } deriving (Show)

type PairFound = Bool

data Action =
  DoNothing
  | HideCards PairFound
  | ShowCard Card
  | GameOverI
  | GameOverII AppState

  deriving (Show)

deck :: Int -> [Card]
deck r = zipWith f [0..] (cs ++ cs)
  where f idx c = Card idx c NotVisible
        x = fromIntegral r :: Double
        y = round $ x*x/2
        cs = take y colors

newState :: AppConfig -> [Double] -> GameState -> IO AppState
newState cfg@(AppConfig r) scs gs = do
  crds <- shuffleM (deck r)
  return $ AppState cfg gs [] crds 0 0 scs

startState :: AppConfig -> IO AppState
startState cfg = newState cfg [] ZeroCards

card :: GameState -> Card -> H.Html Action
card gst crd@(Card _x c cSt) =
  (if (cSt == NotVisible && gst /= TwoCards) then (H.! E.onClick' (ShowCard crd)) else id)
  H.div H.! A.class_ (H.toValue "col-xs-12")
        H.! A.style (fromList [clr, width, height, margin, border, padding, cursor])
        $ mempty
  where clr = (Text.pack "backgroundColor", Text.pack y)
        y = case cSt of
                 NotVisible -> gray
                 Visible -> c
                 Found -> white
        width = (Text.pack "width", Text.pack "60px")
        height = (Text.pack "height", Text.pack "60px")
        margin = (Text.pack "margin", Text.pack "6px")
        border = (Text.pack "border", Text.pack (if cSt == Found then "0px" else "1px solid"))
        padding = (Text.pack "padding", Text.pack "10px")
        cursor = (Text.pack "cursor", Text.pack "default")

row :: GameState -> [Card] -> H.Html Action
row gst cs = H.div H.! A.class_ (H.toValue "row") $ foldMap (card gst) cs

field :: GameState -> [[Card]] -> H.Html Action
field gst cs = H.div H.! A.class_ (H.toValue "container-fluid field") $ foldMap (row gst) cs


statistics :: (H.ToMarkup a) => [(String, a)] -> H.Html Action
statistics xs =
  H.div H.! A.class_ (H.toValue "container-fluid") $ foldMap f xs
  where f (a, b) =
          H.div H.! A.class_ (H.toValue "row") $ do
            H.div H.! A.class_ (H.toValue "col-xs-5") $ H.toHtml a
            H.div H.! A.class_ (H.toValue "col-xs-3") $ H.toHtml b

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs =
  let (as, bs) = splitAt n xs
  in as : chunk n bs

message :: Turns -> Bool -> H.Html Action
message (Turns ts) b =
  H.p H.! (A.style (fromList [showText, bold]))
      $ H.toHtml $ case ts of
                        0 -> "Start the game by clicking on the cards below!"
                        _ -> "Well done! You found all pairs!"
  where showText = (Text.pack "visibility", Text.pack (if b || ts == 0 then "visible" else "hidden"))
        bold = (Text.pack "font-weight", Text.pack "bold")

renderGame :: AppState -> H.Html Action
renderGame (AppState (AppConfig r) gst _ cs x (PairsFound pf) ts) = do
  let len = fromIntegral (length ts)
      mean = sum ts / len
      stdDev = sqrt (sum (map (\t -> (t-mean)^(2::Integer)) ts) / len) :: Double
      toPerc :: Double -> String
      toPerc p = printf "%.2f" (100*p) ++ "%"
      f idx t = (show idx ++ ". Game", toPerc t)
      idxs = [1 :: Integer ..]
  message x (pf == r*r `div` 2) 
  field gst (chunk r cs)
  H.br
  statistics [("Number of turns", unTurns x), ("Pairs found", pf)]
  H.br
  unless (isNaN mean) $
    statistics $
    ("Average score", toPerc mean)
    : ("Standard deviation", toPerc stdDev)
    : (reverse $ zipWith f idxs ts)

rules :: H.Html Action
rules = do
  H.h3 (H.toHtml "Rules")
  H.p $ H.toHtml "After clicking on a card, you card will show its color. The goal of Memory is to consecutively click on two cards with the same color. After finding two such cards, they will vanish from the board. Otherwise their color will be hidden and you can try again. After having found all pairs of cards, you will get your score. After playing several games, you will get some statistics about your performance."

link :: String -> String -> H.Html Action
link url txt =
  H.a
  H.! A.href (H.toValue url)
  H.! A.target (H.toValue "_blank")
  $ H.toHtml txt

about :: H.Html Action
about = do
  H.h3 (H.toHtml "About")
  H.p $ do
    H.toHtml "This game is entirly written in "
    link "https://www.haskell.org/" "Haskell"
    H.toHtml ". It was compiled with Luite Stegeman's "
    link "https://github.com/ghcjs/ghcjs" "ghcjs"
    H.toHtml ", a JavaScript to Haskell compiler. For the rendering, I used "
    link "https://github.com/meiersi/blaze-react" "blaze-react"
    H.toHtml ", the blaze-html style ReactJS bindings for Haskell written by Simon Meier."
    H.toHtml " The styling was done with bootstrap."

impressum :: H.Html Action
impressum = do
  H.h3 (H.toHtml "Impressum")
  H.p $ do
    H.toHtml "This game has been brought to you by "
    link "http://78.47.219.106" "Funktionale Programmierung Dr. Heinrich Hördegen"
    H.toHtml "."

title :: H.Html Action
title = H.h1 $ H.toHtml "Memory Game"

renderPage :: H.Html Action -> H.Html Action
renderPage game =
  H.div H.! A.class_ (H.toValue "content") $ do
    title
    rules
    H.br
    game
    about
    impressum

    
renderState :: AppState -> WindowState Action
renderState state = WindowState {
  _wsBody = renderPage (renderGame state),
  _wsPath = Text.pack "" }

applyAction :: Action -> Transition AppState Action
applyAction action =
  runTransitionM $
  case action of
       ShowCard x -> do
         gs <- gstate `fmap` St.get
         case gs of
          ZeroCards -> showSelectedCard x (OneCard x)
          OneCard y -> do
            incNumberOfTurns
            incPairsFound x y
            showSelectedCard x TwoCards
            PairsFound pf <- pairsFound `fmap` St.get
            AppConfig r <- config `fmap` St.get
            let gOver = pf == r*r `div` 2
            if gOver then gameOver else continueGame x y
          TwoCards -> return ()
       HideCards pf -> 
         Wr.lift (St.modify (mapCards (hideCard pf) . modifyGameState ZeroCards))

       GameOverI -> do
         Wr.lift (St.modify showAllCards)
         appSt <- St.get
         Wr.tell [ threadDelay 3000000
                   >> GameOverII `fmap` newState (config appSt) (scores appSt) TwoCards ]
       GameOverII start ->
         Wr.lift (St.modify (modifyGameState ZeroCards . const start))

       DoNothing -> return ()

showSelectedCard :: Card -> GameState -> TransitionM AppState Action
showSelectedCard x gst = Wr.lift (St.modify (mapCards (showCard x) . modifyGameState gst))

continueGame :: Card -> Card -> TransitionM AppState Action
continueGame x y = do
  Wr.tell [threadDelay 1000000 >> return (HideCards $ color x == color y) ] 

gameOver :: TransitionM AppState Action
gameOver = do
  appSt <- St.get
  let Turns ts = numberOfTurns appSt
      AppConfig r = config appSt
      score = fromIntegral (r*r `div` 2) / fromIntegral ts
  Wr.lift (St.modify (\aSt -> aSt { scores = scores aSt ++ [score]}))
  Wr.tell [threadDelay 1000000 >> return GameOverI ]


showCard :: Card -> Card -> Card
showCard (Card x _ _) (Card idx c st) = Card idx c $ if idx == x then Visible else st

showAllCards :: AppState -> AppState
showAllCards appSt = appSt { cards = map f (cards appSt) }
  where f (Card idx c _) = Card idx c Visible

hideCard :: PairFound -> Card -> Card
hideCard False crd@(Card _ _ Found) = crd
hideCard False (Card idx c _) = Card idx c NotVisible
hideCard True (Card idx c Visible) = Card idx c Found
hideCard True crd = crd 

mapCards :: (Card -> Card) -> AppState -> AppState
mapCards f (AppState cfg gst fs cs x y ts) = AppState cfg gst fs (map f cs) x y ts

modifyGameState :: GameState -> AppState -> AppState
modifyGameState newSt (AppState cfg _ fs cs x y ts) = AppState cfg newSt fs cs x y ts

incNumberOfTurns :: TransitionM AppState Action
incNumberOfTurns = Wr.lift (St.modify f)
  where f x = x { numberOfTurns = numberOfTurns x + 1 }

incPairsFound :: Card -> Card -> TransitionM AppState Action
incPairsFound c0 c1 =
  let fnd = color c0 == color c1
      f x = x { pairsFound = pairsFound x + 1 }
  in Wr.lift (St.modify (if fnd then f else id))

app :: AppState -> App AppState Action
app state = App {
  appInitialState    = state,
  appInitialRequests = [],
  appApplyAction     = applyAction,
  appRender          = renderState }

main :: IO ()
main = do
  let cfg = AppConfig 6
  st <- startState cfg
  ReactJS.runApp' (app st)
