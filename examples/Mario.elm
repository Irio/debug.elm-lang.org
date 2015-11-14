import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import List
import Window
import Set


-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }


type alias Input =
  ( Float
  , Keys
  , Set.Set Keyboard.KeyCode
  )


type Direction = Left | Right


type alias Keys = { x:Int, y:Int }


mario : Model
mario =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }


-- UPDATE

update : Input -> Model -> Model
update (dt, keys, codes) mario =
  mario
    |> gravity dt
    |> jump keys
    |> walk codes keys
    |> physics dt


jump : Keys -> Model -> Model
jump keys mario =
  if keys.y > 0 && mario.vy == 0
    then { mario | vy <- 6.0 }
    else mario


gravity : Float -> Model -> Model
gravity dt mario =
  { mario |
      vy <- if mario.y > 0 then mario.vy - dt/4 else 0
  }


physics : Float -> Model -> Model
physics dt mario =
  { mario |
      x <- mario.x + dt * mario.vx,
      y <- max 0 (mario.y + dt * mario.vy)
  }


walk : Set.Set Keyboard.KeyCode -> Keys -> Model -> Model
walk codes keys mario =
  let
    isRunning codes = Set.toList codes |> List.member 16
    speed = if isRunning codes then 4 else 2
  in
    { mario |
        vx <- toFloat (keys.x * speed),
        dir <-
          if  | keys.x < 0 -> Left
              | keys.x > 0 -> Right
              | otherwise  -> mario.dir
    }


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') mario =
  let
    (w,h) = (toFloat w', toFloat h')

    verb =
      if  | mario.y  >  0 -> "jump"
          | mario.vx /= 0 -> "walk"
          | otherwise     -> "stand"

    dir =
      case mario.dir of
        Left -> "left"
        Right -> "right"

    src =
      "../resources/imgs/mario/" ++ verb ++ "/" ++ dir ++ ".gif"

    marioImage =
      image 70 70 src

    groundY = 70 - h/2

    position =
      (mario.x, mario.y + groundY)
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 174 238 238)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move (0, 24 - h/2)
      , marioImage
          |> toForm
          |> move position
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions <| Signal.foldp update mario input


input : Signal Input
input =
  let
    delta = Signal.map (\t -> t/10) (fps 30)
  in
    Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Keyboard.keysDown)
