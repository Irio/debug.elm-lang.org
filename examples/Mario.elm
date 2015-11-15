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
  , game : GameState
  , holes : List(Hole)
  }

type alias Window = (Int, Int)


type alias Input =
  ( Float
  , Keys
  , Set.Set Keyboard.KeyCode
  , Window
  )


type alias Hole = (Float, Float)


type Direction = Left | Right


type GameState = Start | Live | Dead


type alias Keys = { x:Int, y:Int }


mario : Model
mario =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  , game = Start
  , holes = []
  }


-- UPDATE

update : Input -> Model -> Model
update (dt, keys, codes, dimensions) mario =
  mario
    |> initializeGame dimensions
    |> gravity dt
    |> jump keys
    |> walk codes keys
    |> physics dt dimensions


isInGivenHole : Model -> Hole -> Bool
isInGivenHole model hole =
  let
    holeX = fst hole
    holeXRange = (holeX - 130 / 2, holeX + 130 / 2)
  in
    fst holeXRange < model.x - 32 / 2
      && model.x + 32 / 2 < snd holeXRange


isFallingInHole : Model -> Bool
isFallingInHole model =
  let
    checkHole = (\hole isInOtherHole ->
      isInOtherHole || isInGivenHole model hole)
  in
    List.foldr checkHole False model.holes


isDead : Model -> Bool
isDead model =
  model.y == yLowerLimit model && isFallingInHole model


initializeGame : Window -> Model -> Model
initializeGame dimensions model =
  let
    w = toFloat (fst dimensions)
    h = toFloat (snd dimensions)
  in
    case model.game of
      Start ->
        { mario | x     <- w / -2 + 100
                , y     <- h
                , vx    <- 0
                , vy    <- 0
                , game  <- Live
                , holes <- [ (w / -3, 24 - h/2)
                           , (w / 10, 24 - h/2) ]
        }
      Live ->
        if isDead model
          then { model | game <- Dead }
          else model
      Dead ->
        { model | game <- Start }


jump : Keys -> Model -> Model
jump keys mario =
  if keys.y > round(yLowerLimit mario) && mario.vy == 0
    then { mario | vy <- 6.0 }
    else mario


yLowerLimit : Model -> Float
yLowerLimit model =
  if isFallingInHole model
    then -100.0
    else 0.0


gravity : Float -> Model -> Model
gravity dt mario =
  { mario | vy <- if mario.y > yLowerLimit mario
                    then mario.vy - dt/4
                    else yLowerLimit mario }


isCollidingWithWindow: Window -> Model -> Bool
isCollidingWithWindow (windowX, windowY) model =
  let
    marioWidthInProperDirection = case model.dir of
      Left -> abs (-18 + model.x)
      Right -> abs (16 + model.x)
  in
    marioWidthInProperDirection >= ((toFloat windowX) / 2)


isColliding: Window -> Model -> Bool
isColliding dimensions model =
  isCollidingWithWindow dimensions model



physics : Float -> Window -> Model -> Model
physics dt dimensions mario =
  { mario | x <-
              if isColliding dimensions mario
                then mario.x
                else mario.x + dt * mario.vx
          , y <-
              max (yLowerLimit mario) (mario.y + dt * mario.vy)
  }


walk : Set.Set Keyboard.KeyCode -> Keys -> Model -> Model
walk codes keys mario =
  let
    isRunning codes = Set.toList codes |> List.member 16
    speed = if isRunning codes
              then 4
              else 2
  in
    { mario | vx  <- toFloat (keys.x * speed)
            , dir <- if | keys.x < 0 -> Left
                        | keys.x > 0 -> Right
                        | otherwise  -> mario.dir
    }


-- VIEW

view : Window -> Model -> Element
view (w', h') model =
  let
    (w,h) = (toFloat w', toFloat h')

    verb =
      if | model.y  >  0 -> "jump"
         | model.vx /= 0 -> "walk"
         | otherwise     -> "stand"

    dir =
      case model.dir of
        Left -> "left"
        Right -> "right"

    src =
      "../resources/imgs/mario/" ++ verb ++ "/" ++ dir ++ ".gif"

    marioImage =
      image 70 70 src

    groundY = 70 - h/2

    position =
      (model.x, model.y + groundY)

    drawHole = ( \hole -> (rect 130 50
                           |> filled (rgb 174 238 238)
                           |> move hole) )

    holes = List.map drawHole model.holes

    scenario = [ rect w 50
                 |> filled (rgb 74 167 43)
                 |> move (0, 24 - h/2)
               , rect w h
                 |> filled (rgb 174 238 238) ]

    forms = List.append holes scenario

    mario = (marioImage
             |> toForm
             |> move position )
  in
    List.reverse (mario :: forms) |> collage w' h'


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions <| Signal.foldp update mario input


input : Signal Input
input =
  let
    delta = Signal.map (\t -> t/10) (fps 30)
  in
    Signal.sampleOn delta (Signal.map4 (,,,) delta Keyboard.arrows Keyboard.keysDown Window.dimensions)
