(* Booper.Color

   A tiny color utility layer on top of Bogue.RGB / Bogue.RGBA.
   Colors are plain [(r,g,b)] tuples (integers in [0..255]), so they can be
   passed directly to Bogue functions expecting a [Bogue.RGB.t] after
   conversion with [opaque] or [to_rgba]. *)

type t = int * int * int
(** An opaque RGB color. *)

type rgba = int * int * int * int
(** An RGBA color; this is the same representation as [Bogue.RGBA.t]. *)

let clamp x = min 255 (max 0 x)

let of_hex s =
  let hex subs = int_of_string ("0x" ^ subs) in
  let s =
    if String.length s > 0 && s.[0] = '#'
    then String.sub s 1 (String.length s - 1)
    else s
  in
  match String.length s with
  | 6 ->
    ( hex (String.sub s 0 2),
      hex (String.sub s 2 2),
      hex (String.sub s 4 2) )
  | 3 ->
    let double c = String.make 2 c in
    ( hex (double s.[0]),
      hex (double s.[1]),
      hex (double s.[2]) )
  | _ -> invalid_arg "Booper.Color.of_hex: expect #rgb or #rrggbb"

let channels (r, g, b) = (clamp r, clamp g, clamp b)

let to_rgba ?(alpha = 255) c =
  let r, g, b = channels c in
  (r, g, b, clamp alpha)

let opaque c = to_rgba c
let translucent ?(alpha = 128) c = to_rgba ~alpha c

let mix ?(t = 0.5) (r1, g1, b1) (r2, g2, b2) =
  let lerp a b = a + int_of_float (float (b - a) *. t) in
  (lerp r1 r2, lerp g1 g2, lerp b1 b2)

let lighten ?(amount = 0.1) c = mix ~t:amount c (255, 255, 255)
let darken ?(amount = 0.1) c = mix ~t:amount c (0, 0, 0)

let to_css (r, g, b) =
  Printf.sprintf "#%02x%02x%02x" r g b
