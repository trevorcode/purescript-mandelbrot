module Main where

import Prelude
import Web.DOM.Document (toParentNode)

import Data.Array ((..))
import Data.Complex (Cartesian(..), magnitudeSquared)
import Data.Foldable (for_)
import Data.Int (hexadecimal, toStringAs)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Data.String (toUpper)
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, fillRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setFillStyle, setStrokeStyle)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Element (fromEventTarget, getBoundingClientRect, toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.WheelEvent as WheelEvent

type Iterations = Int
type Point = { x :: Number, y :: Number }
type Camera =
  { center :: Point
  , zoom :: Number
  }

max_iters :: Int
max_iters = 50

z' :: forall a3. Semiring a3 => a3 -> a3 -> a3
z' z c = (z * z) + c

fractal :: Cartesian Number -> Cartesian Number -> Int -> Tuple (Boolean) Iterations
fractal c z iter
  | iter >= max_iters = (true) /\ iter
  | (sqrt $ magnitudeSquared z) > 2.0 = (false) /\ iter
  | otherwise = fractal c (z' z c) (iter + 1)

renderMandelbrot :: CanvasElement -> Ref Camera -> Effect Unit
renderMandelbrot canvas camera = do
  cam <- Ref.read camera
  let center = cam.center
  let zoom = cam.zoom

  let realset = { start: (-2.0 / zoom) + center.x, end: (0.5 / zoom) + center.x }
  let imaginaryset = { start: (-1.0 / zoom) + center.y, end: (1.0 / zoom) + center.y }

  ctx <- getContext2D canvas
  setStrokeStyle ctx "#000"

  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas

  for_ (1 .. Int.floor width) \x -> do
    for_ (1 .. Int.floor height) \y -> do
      let
        res =
          fractal
            ( Cartesian
                (realset.start + ((Int.toNumber $ x) / width) * (realset.end - realset.start))
                (imaginaryset.start + ((Int.toNumber $ y) / height) * (imaginaryset.end - imaginaryset.start))
            )
            (Cartesian 0.0 0.0)
            0

      let iter = (((Int.toNumber $ snd res) / Int.toNumber max_iters) * (16777215.0 / 1.0))
      let num = toUpper (toStringAs hexadecimal $ Int.floor iter)

      let
        r =
          { width: 1.0
          , height: 1.0
          , x: Int.toNumber x
          , y: Int.toNumber y
          }

      setFillStyle ctx $ "#" <> num
      fillRect ctx r

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  doc <- map (toParentNode <<< toDocument) (document =<< window)
  Just node <- querySelector (QuerySelector "#canvas") doc

  camera <- Ref.new
    { center: { x: 0.0, y: 0.0 }
    , zoom: 1.0
    }

  clickListener <- eventListener $ handleClick camera $ renderMandelbrot canvas camera
  scrollListener <- eventListener $ handleScroll camera $ renderMandelbrot canvas camera
  addEventListener (EventType "click") clickListener true (toEventTarget node)
  addEventListener (EventType "wheel") scrollListener true (toEventTarget node)

  renderMandelbrot canvas camera

handleClick :: Ref Camera -> Effect Unit -> Event -> Effect Unit
handleClick camera renderCanvas e = do
  for_ (MouseEvent.fromEvent e) \me -> do
    let canvas = fromEventTarget =<< Event.target e

    for_ canvas \c -> do
      boundingRect <- getBoundingClientRect c

      let x = MouseEvent.clientX me - Int.floor boundingRect.left
      let y = MouseEvent.clientY me - Int.floor boundingRect.top

      let newX = (Int.toNumber x - (boundingRect.width / 2.0)) / (boundingRect.width / 2.0)
      let newY = (Int.toNumber y - (boundingRect.height / 2.0)) / (boundingRect.height / 2.0)

      Ref.modify_ (\c -> c { center = { x: newX, y: newY } }) camera
      renderCanvas

handleScroll :: Ref Camera -> Effect Unit -> Event -> Effect Unit
handleScroll camera renderCanvas e = do
  for_ (WheelEvent.fromEvent e) \scrollEvent -> do
    let scrollY = WheelEvent.deltaY scrollEvent
    let newScroll = scrollY / (-100.0)

    Ref.modify_ (\c -> c { zoom = c.zoom + newScroll }) camera
    renderCanvas