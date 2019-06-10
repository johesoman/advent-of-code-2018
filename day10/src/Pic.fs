module Pic


open FColor



type Style =
  | Filled
  | Outline



type Width  = int
type Height = int



type Size =
  | Px of int
  | Pt of int



type Font = string



type Point =
  { x : int
  ; y : int
  }



type Pic =
  | Rectangle of Point * Width * Height * Style * Color
  | Text      of string * Point * Font * Size * Style * Color
  | Translate of int * int * Pic




let translateX x    p = Translate (x, 0, p)
let translateY    y p = Translate (0, y, p)
let translateXY x y p = Translate (x, y, p)



module Font =
  let sourceCodePro = "Source Code Pro"


