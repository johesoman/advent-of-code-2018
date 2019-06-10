module App



open Pic
open Game
open FColor
open Fable.Core
open Extensions
open Combinators
open Fable.Import
open Fable.AST.Babel
open Fable.Core.JsInterop
open Fable.Import.Browser



type Light =
  { pX : int
  ; pY : int
  ; vX : int
  ; vY : int
  }



type GameState =
  | Paused
  | Running



type State =
  { lights    : Light []
  ; gameState : GameState
  }



// ++++++++++
// + lights +
// ++++++++++



module Light =
  let tick ({pX = pX; pY = pY; vX = vX; vY = vY} as l) =
    {l with pX = pX + vX; pY = pY + vY}



  let initMany =
    let toLight ((pX, pY), (vX, vY)) =
      {pX = pX; pY = pY; vX = vX; vY = vY; }

    let lights =
      Seq.map toLight Input.pointsAndVectors
      |> Seq.toArray

    Seq.fold (fun acc _ -> Array.map tick acc) lights [1 .. 10476]



// +++++++++
// + state +
// +++++++++



module State =
  let init =
    { lights    = Light.initMany
    ; gameState = Paused
    }



// ++++++++
// + tick +
// ++++++++




let tick (st : State) =
  match st.gameState with
  | Paused  -> st
  | Running -> {st with lights = Array.map Light.tick st.lights }



// ++++++++
// + draw +
// ++++++++



let background width height color : Pic =
  Rectangle ({x = 0; y = 0}, width, height, Filled, color)



let rotate theta ls =
  let rotate l =
    let theta = double theta
    let x     = double l.pX
    let y     = double l.pY

    let x2 =  x * cos theta + y * sin theta |> int
    let y2 = -x * sin theta + y * cos theta |> int

    {l with pX = x2; pY = y2}

  Array.map rotate ls



let center ls =
  let xs, ys =
    Array.map (fun l -> double l.pX, double l.pY) ls
    |> Array.unzip

  let xAvg = Array.average xs |> int
  let yAvg = Array.average ys |> int

  Array.map (fun l -> {l with pX = l.pX - xAvg; pY = l.pY - yAvg}) ls



let shearX w ls =
  let shearX l =
    let x = double l.pX
    let y = double l.pY

    {l with pX = x + w * y |> int}

  Array.map shearX ls



let drawLight l =
  let x, y = l.pX, l.pY
  let xMul = 4
  let yMul = 4

  Rectangle ({x = x * xMul; y = y * yMul}, 2, 2, Filled, FColor.black)



let draw (cd : CanvasData) (st : State) =

  let ls =
    st.lights
    |> Array.map drawLight
    |> Array.map (translateXY -200 -200)

  Array.cons
    (background cd.width cd.height FColor.white)
    ls



// +++++++++
// + event +
// +++++++++



let event (e : UserEvent) (st : State) =
  match st.gameState with
  | Paused ->
      match e with
      | KeyDown Space     -> {st with gameState = Running}
      | KeyDown (Other 82) -> State.init
      | _                  -> st

  | Running ->
      match e with
      | KeyDown Space      -> {st with gameState = Paused}
      | KeyDown (Other 82) -> State.init
      | _                  -> st




// +++++++
// + run +
// +++++++



Game.run 5 800 600 event tick draw State.init
