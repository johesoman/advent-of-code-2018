module Game



open Pic
open System.IO
open Fable.Core
open Combinators
open Fable.Import
open Fable.AST.Babel
open Fable.Core.JsInterop
open Fable.Import.Browser



type FPS    = int
type Width  = int
type Height = int



[<Struct>]
type CanvasData =
  { width  : Width
  ; height : Height
  }



let canvas =
  Browser.document.getElementById "window"
  :?> Browser.HTMLCanvasElement



let resizeCanvas (width : Width) (height : Height) =
  canvas.width  <- double width
  canvas.height <- double height



let getCanvasData () =
  { width  = int canvas.width
  ; height = int canvas.height
  }



module Pic =
  let getPoint =
    function
    | Rectangle (p, _, _, _, _)    -> p
    | Text      (_, p, _, _, _, _) -> p

    | pic -> failwithf "Error @ Pic.getPoint %A" pic



  let setPoint p =
    function
    | Rectangle (_, width, height, style, color) ->
        Rectangle (p, width, height, style, color)

    | Text      (text, _, font, size, style, color) ->
        Pic.Text (text, p, font, size, style, color)


    | pic -> failwithf "Error @ Pic.getPoint %A" pic




let drawPic (ctx : Browser.CanvasRenderingContext2D) =
  let rec go =
    function
    | Rectangle (pos, width, height, style, color) ->
        ctx.fillStyle <- unbox color

        let arg =
          double pos.x, double pos.y,
          double width, double height

        match style with
        | Filled  -> ctx.fillRect   arg
        | Outline -> ctx.strokeRect arg

    | Text (text, pos, font, size, style, color) ->
        ctx.fillStyle <- unbox color

        let s =
          match size with
          | Px x -> string x + "px"
          | Pt x -> string x + "pt"

        ctx.font <- unbox (s + " " + font)

        match style with
        | Filled  -> ctx.fillText   (text, double pos.x, double pos.y)
        | Outline -> ctx.strokeText (text, double pos.x, double pos.y)

    | Translate (distX, distY, pic) ->
        match pic with
        | Translate (distX2, distY2, pic2) ->
            go (Translate (distX + distX2, distY + distY2, pic2))

        | pic ->
           let p = Pic.getPoint pic
           go (Pic.setPoint {x = p.x + distX; y = p.y + distY} pic)

  go


type Key =
  | Space
  | ArrowLeft
  | ArrowUp
  | ArrowRight
  | ArrowDown
  | Other of int



let keyOfInt =
  function
  | 32 -> Space
  | 37 -> ArrowLeft
  | 38 -> ArrowUp
  | 39 -> ArrowRight
  | 40 -> ArrowDown
  | x  -> Other x



type UserEvent =
  | KeyDown of Key



type Event =
  | Tick
  | UserEvent of UserEvent



// +++++++++++++
// + game loop +
// +++++++++++++



let frame (ctx   : CanvasRenderingContext2D)
          (tick  : 'State -> 'State)
          (draw  : CanvasData -> 'State -> Pic [])
          (cd    : CanvasData)
          (st    : 'State) =
  let st2 = tick st

  draw cd st2
  |> Array.iter (drawPic ctx)

  st2



let setupGameLoop (ctx   : CanvasRenderingContext2D)
                  (event : UserEvent -> 'State -> 'State)
                  (tick  : 'State -> 'State)
                  (draw  : CanvasData -> 'State -> Pic [])
                  (st    : 'State) =
  let frame = frame ctx tick draw

  let rec loop (box : MailboxProcessor<Event>) (st : 'State)  = async {
    let! msg = box.Receive()

    match msg with
    | UserEvent e -> return! loop box (event e st)
    | Tick        -> return! loop box (frame (getCanvasData()) st)
  }

  MailboxProcessor.Start (flip loop st)



// +++++++++
// + clock +
// +++++++++



let rec clock (loop : MailboxProcessor<Event>) ms = async {
    loop.Post Tick
    do! Async.Sleep ms
    return! clock loop ms
  }



let startClock loop fps =
  Async.StartImmediate
    (clock loop (int (1000. / double fps)))



// +++++++++++++
// + interface +
// +++++++++++++



let run (fps    : FPS)
        (width  : Width)
        (height : Height)
        (event  : UserEvent -> 'State -> 'State)
        (tick   : 'State -> 'State)
        (draw   : CanvasData -> 'State -> Pic [])
        (st     : 'State) =
  resizeCanvas width height

  let loop =
    setupGameLoop
      (canvas.getContext_2d())
      event
      tick
      draw
      st

  Browser.window.addEventListener_keydown (fun e ->
    int e.keyCode
    |> keyOfInt
    |> KeyDown
    |> UserEvent
    |> loop.Post
    )

  startClock loop fps

