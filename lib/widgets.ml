(* Booper.Widgets

   Modern-looking widgets built on top of Bogue, all styled from a
   {!Booper.Theme}. Every constructor returns a [Bogue.Layout.t], so
   widgets compose directly with {!Booper.row} / {!Booper.column}. *)

(* Aliases to sibling modules, taken before [open Bogue] so they are not
   shadowed by Bogue.Theme. *)
module T = Theme
module Sdl = Tsdl.Sdl

open Bogue

type layout = Layout.t

type size = H1 | H2 | H3 | Body | Caption | Small
(** Typographic scale, relative to the theme's base font size. *)

type role =
  | Text | Muted | Primary | Secondary | Success | Warning | Danger
  | Custom of Color.t
(** Semantic text/icon colors. *)

type variant = Primary | Secondary | Ghost | Danger | Success
(** Button variants. *)

(* {2 Internal helpers} *)

let rgba ?alpha c = Color.to_rgba ?alpha c

let transparent = (0, 0, 0, 0)

let solid ?alpha c = Style.color_bg (rgba ?alpha c)

let vgrad ?(top = 0.06) ?(bottom = 0.04) c =
  Style.vgradient
    [ rgba (Color.mix ~t:top c (255, 255, 255));
      rgba (Color.mix ~t:bottom c (0, 0, 0)) ]

let role_color t = function
  | Text -> t.T.palette.text
  | Muted -> t.T.palette.text_muted
  | Primary -> t.T.palette.primary
  | Secondary -> t.T.palette.secondary
  | Success -> t.T.palette.success
  | Warning -> t.T.palette.warning
  | Danger -> t.T.palette.danger
  | Custom c -> c

let size_px t = function
  | H1 -> t.T.font_size + 14
  | H2 -> t.T.font_size + 8
  | H3 -> t.T.font_size + 3
  | Body -> t.T.font_size
  | Caption -> t.T.font_size - 2
  | Small -> t.T.font_size - 4

(* Make a Bogue label using the theme's font. *)
(* Resolve the theme font (path "" = keep Bogue's default). *)
let theme_font ?(weight = `Normal) t =
  match weight with
  | `Normal -> T.regular_font t
  | `Semibold -> T.semibold_font t

(* Backend Label.t, used e.g. inside buttons. *)
let backend_label ?align ?weight ~size ~fg text t =
  let font =
    Option.map Label.font_from_file
      (match theme_font ?weight t with "" -> None | f -> Some f)
  in
  Label.create ?font ?align ~size ~fg text

let pill_background ?(radius = 0) ?(border = `None) c =
  let border =
    match border with
    | `None -> None
    | `Color bc ->
      Some (Style.mk_border ~radius (Style.mk_line ~color:(rgba bc) ()))
  in
  Style.create ~background:(solid c) ?border ()

let clamp01 x = max 0. (min 1. x)
let clamp_range lo hi v = max lo (min hi v)

(* {2 Layout helpers} *)

let row ?(sep = -1) ?(margins = 0) ?(align = Draw.Center) ?background ?shadow t
    rooms =
  let sep = if sep < 0 then t.T.spacing else sep in
  Layout.flat ~sep ~margins ~align ?background ?shadow rooms

let column ?(sep = -1) ?(margins = 0) ?align ?background ?shadow t rooms =
  let sep = if sep < 0 then t.T.spacing else sep in
  Layout.tower ~sep ~margins ?align ?background ?shadow rooms

let spacer ?(w = 10) ?(h = 10) ?background () = Layout.empty ~w ~h ?background ()
let hfill () = Space.hfill ()
let vfill () = Space.vfill ()

(* {2 Text} *)

let label ?(size = Body) ?(weight = `Normal) ?(color = Text) ?align t text =
  T.apply t;
  let font =
    Option.map Label.font_from_file
      (match theme_font ~weight t with "" -> None | f -> Some f)
  in
  Widget.label ?font ?align ~size:(size_px t size) ~fg:(rgba (role_color t color))
    text
  |> Layout.resident

let heading t text = label t ~size:H2 ~weight:`Semibold text
let title t text = label t ~size:H1 ~weight:`Semibold text

let icon ?(size = 20) ?(color = Text) t name =
  T.apply t;
  Widget.icon ~size ~fg:(rgba (role_color t color)) name |> Layout.resident

let divider ?(width = 200) t =
  Widget.box ~w:width ~h:1
    ~style:(Style.of_bg (solid t.T.palette.border))
    ()
  |> Layout.resident

(* {2 Card} *)

let card ?(padding = 22) ?(sep = 14) ?(shadow = true) ?(radius = 14)
    ?(background = None) ?(border = true) t rooms =
  let p = t.T.palette in
  let bgc = match background with Some c -> c | None -> p.surface in
  let border_color =
    if border then Some (Color.lighten ~amount:0.04 p.border) else None
  in
  let style =
    Style.create ~background:(vgrad ~top:0.03 ~bottom:0. bgc)
      ?border:
        (Option.map
           (fun bc ->
             Style.mk_border ~radius (Style.mk_line ~color:(rgba bc) ()))
           border_color)
      ()
  in
  let shadow =
    if shadow then
      Some (Style.mk_shadow ~offset:(0, 4) ~size:3 ~width:radius ~radius ())
    else None
  in
  Layout.tower ~sep ~margins:padding ~background:(Layout.style_bg style) ?shadow
    rooms

(* {2 Button} *)

let pad ?(h = 16) ?(v = 12) l = Layout.flat ~hmargin:h ~vmargin:v [ l ]

let button ?(variant = Primary) ?(color = (Primary : role)) ?(small = false)
    ?radius t ~on_click text =
  T.apply t;
  let radius =
    match radius with Some r -> r | None -> max 4 (t.T.radius - 4)
  in
  let p = t.T.palette in
  let fs = if small then t.T.font_size - 2 else t.T.font_size in
  let fg, bg, border, hover =
    match variant with
    | Primary ->
      ( p.on_primary, vgrad ~top:0.08 ~bottom:0.05 p.primary,
        Some (Color.darken ~amount:0.3 p.primary),
        vgrad ~top:0.13 ~bottom:0.02 (Color.lighten ~amount:0.03 p.primary) )
    | Secondary ->
      ( p.text, vgrad ~top:0.05 ~bottom:0.06 p.surface_alt, Some p.border,
        vgrad ~top:0.09 ~bottom:0.03 p.surface_alt )
    | Ghost ->
      let c = role_color t color in
      (c, solid ~alpha:0 (0, 0, 0), None, solid ~alpha:38 c)
    | Danger ->
      ( (255, 255, 255), vgrad ~top:0.08 ~bottom:0.05 p.danger,
        Some (Color.darken ~amount:0.3 p.danger),
        vgrad ~top:0.13 ~bottom:0.02 p.danger )
    | Success ->
      ( (255, 255, 255), vgrad ~top:0.08 ~bottom:0.05 p.success,
        Some (Color.darken ~amount:0.3 p.success),
        vgrad ~top:0.13 ~bottom:0.02 p.success )
  in
  let lbl =
    backend_label ~size:fs ~fg:(rgba fg) text t
  in
  let w =
    Widget.button ~kind:Button.Trigger ~label:lbl ~fg:(rgba fg)
      ~bg_on:bg ~bg_off:bg ~bg_over:(Some hover)
      ?border_radius:
        (match variant with Ghost -> None | _ -> Some radius)
      ?border_color:(Option.map rgba border)
      ~action:(fun _ -> on_click ())
      ""
  in
  pad (Layout.resident w)

(* {2 Switch and check box} *)

type toggle = { layout : layout; state : unit -> bool }
(** An interactive on/off widget together with a way to read its state. *)

let switch_button ?state ~label_on ~label_off ~on_toggle t =
  T.apply t;
  let transparent = Style.color_bg transparent in
  Widget.button ~kind:Button.Switch ?state ~label_on ~label_off
    ~fg:(rgba t.T.palette.text_muted)
    ~bg_on:transparent ~bg_off:transparent ~bg_over:(Some transparent)
    ~action:(fun s -> on_toggle s)
    ""

let switch ?(state = false) ?(on_toggle = ignore) ?(size = 24) t () =
  let p = t.T.palette in
  let on = Label.icon ~size ~fg:(rgba p.primary) "toggle-on" in
  let off = Label.icon ~size ~fg:(rgba p.text_muted) "toggle-off" in
  let w = switch_button ~state ~label_on:on ~label_off:off ~on_toggle t in
  { layout = Layout.resident w; state = (fun () -> Widget.get_state w) }

let check ?(state = false) ?(on_toggle = ignore) ?(color = Text) t text =
  T.apply t;
  let p = t.T.palette in
  let s = t.T.font_size + 3 in
  let on = Label.icon ~size:s ~fg:(rgba p.primary) "check-square" in
  let off = Label.icon ~size:s ~fg:(rgba p.text_muted) "square-o" in
  let w = switch_button ~state ~label_on:on ~label_off:off ~on_toggle t in
  let l = label t ~size:Body ~color text in
  { layout = row t ~sep:8 [ Layout.resident w; l ];
    state = (fun () -> Widget.get_state w) }

(* {2 Text input} *)

type input = { layout : layout; text : unit -> string }

let input ?(width = 260) ?(height = 42) ?prompt t () =
  T.apply t;
  let p = t.T.palette in
  let w = Widget.text_input ?prompt ~size:t.T.font_size () in
  let bg =
    Layout.style_bg
      (pill_background ~radius:t.T.radius
         ~border:(`Color (Color.lighten ~amount:0.04 p.border))
         p.surface_alt)
  in
  let hmargin = 12 in
  let vmargin = max 4 ((height - 30) / 2) in
  let inner = Layout.resident ~w:(width - 2 * hmargin) w in
  { layout = Layout.flat ~hmargin ~vmargin ~background:bg [ inner ];
    text = (fun () -> Text_input.text (Widget.get_text_input w)) }

(* {2 Progress bar} *)

type progress = { layout : layout; set : float -> unit }

(* Draw a horizontal "pill" (rounded bar) with SDL primitives. *)
let paint_pill area ~x0 ~x1 ~y0 ~y1 ~color =
  let h = y1 - y0 in
  let r = h / 2 in
  Sdl_area.fill_rectangle area ~color ~w:(x1 - x0) ~h (x0, y0);
  Sdl_area.fill_circle area ~color ~radius:r (x0 + r, y0 + r);
  Sdl_area.fill_circle area ~color ~radius:r (x1 - r, y0 + r)

let progress ?(width = 220) ?(height = 8) ?track ?(fill = (Primary : role)) t
    percent =
  T.apply t;
  let p = t.T.palette in
  let track =
    match track with Some c -> c | None -> Color.darken ~amount:0.12 p.surface_alt
  in
  let fill = role_color t fill in
  let w = Widget.sdl_area ~w:width ~h:height () in
  let area = Widget.get_sdl_area w in
  let percent = ref (clamp01 percent) in
  let paint () =
    Sdl_area.clear area;
    let w, h = Sdl_area.drawing_size area in
    let y0 = (h - (Theme.scale_int height)) / 2 in
    let hh = Theme.scale_int height in
    paint_pill area ~x0:0 ~x1:w ~y0 ~y1:(y0 + hh) ~color:(rgba track);
    let fw = max (Theme.scale_int height) (int_of_float (float w *. !percent)) in
    paint_pill area ~x0:0 ~x1:fw ~y0 ~y1:(y0 + hh) ~color:(rgba fill);
    let r = hh / 2 in
    if fw > 2 * r && hh >= 6 then
      Sdl_area.fill_rectangle area
        ~color:(rgba ~alpha:40 (255, 255, 255))
        ~w:(fw - 2 * r) ~h:(max 1 (hh / 4))
        (r, y0 + (r / 2) + 1);
    Sdl_area.update area
  in
  let set v = percent := clamp01 v; Sync.push paint in
  Sync.push paint;
  { layout = Layout.resident w; set }

(* {2 Slider} *)

type slider = { layout : layout; get : unit -> int; set : int -> unit }

let slider ?(min = 0) ?(max = 100) ?value ?(width = 220) ?(on_change = ignore) t
    () =
  T.apply t;
  let p = t.T.palette in
  let value = match value with Some v -> v | None -> min in
  let w = Widget.sdl_area ~w:width ~h:26 () in
  let area = Widget.get_sdl_area w in
  let current = ref (clamp_range min max value) in
  let dragging = ref false in
  let paint () =
    Sdl_area.clear area;
    let w, h = Sdl_area.drawing_size area in
    let track_h = Theme.scale_int 6 in
    let knob_r = Theme.scale_int 9 in
    let pad = knob_r + 2 in
    let cy = h / 2 in
    let frac =
      float (!current - min) /. float (max - min) |> clamp01
    in
    let kx = pad + int_of_float (float (w - 2 * pad) *. frac) in
    (* track *)
    paint_pill area ~x0:pad ~x1:(w - pad) ~y0:(cy - track_h / 2)
      ~y1:(cy + track_h / 2)
      ~color:(rgba (Color.darken ~amount:0.1 p.surface_alt));
    (* filled part *)
    paint_pill area ~x0:pad ~x1:(Stdlib.max (pad + track_h) kx) ~y0:(cy - track_h / 2)
      ~y1:(cy + track_h / 2) ~color:(rgba p.primary);
    (* knob *)
    Sdl_area.fill_circle area ~color:(rgba ~alpha:60 (0, 0, 0))
      ~radius:(knob_r + 1) (kx, cy + 1);
    Sdl_area.fill_circle area ~color:(rgba p.surface) ~radius:knob_r (kx, cy);
    Sdl_area.draw_circle area ~thick:2 ~radius:(knob_r - 1)
      ~color:(rgba p.primary) (kx, cy);
    Sdl_area.update area
  in
  let request_paint () = Sync.push paint in
  let set v =
    let v = clamp_range min max v in
    if v <> !current then begin
      current := v;
      request_paint ();
      on_change v
    end
  in
  let apply_event ev =
    let x, _ = Sdl_area.pointer_pos area ev in
    let w, _ = Sdl_area.drawing_size area in
    let pad = (Theme.scale_int 9) + 2 in
    let frac =
      float (x - pad) /. float (Stdlib.max 1 (w - 2 * pad)) |> clamp01
    in
    set (min + int_of_float (frac *. float (max - min) +. 0.5))
  in
  let action _ _ ev =
    match Sdl.Event.(enum (get ev typ)) with
    | `Mouse_button_down | `Finger_down ->
      dragging := true; apply_event ev
    | `Mouse_button_up | `Finger_up -> dragging := false
    | `Mouse_motion | `Finger_motion when !dragging -> apply_event ev
    | _ -> ()
  in
  let c =
    Widget.connect w w ~priority:Widget.Main action
      (Trigger.pointer_motion @ Trigger.buttons_down @ Trigger.buttons_up)
  in
  Widget.add_connection w c;
  request_paint ();
  { layout = Layout.resident w; get = (fun () -> !current); set }

(* {2 Window} *)

let run ?(title = "Booper") ?(exit_on_escape = true) ?(width = 0) ?(height = 0)
    ?(margin = 24) ?(center = true) t content =
  T.apply t;
  let p = t.T.palette in
  let align = if center then Some Draw.Center else None in
  let body = column t ?align ~sep:0 ~margins:margin [ content ] in
  let house =
    Layout.tower ~name:"booper" ?align ~resize:Layout.Resize.Linear
      ~background:
        (Layout.style_bg
           (Style.of_bg
              (Style.vgradient
                 [ Color.opaque p.bg;
                   Color.opaque (Color.darken ~amount:0.28 p.bg) ])))
      [ body ]
  in
  let shortcuts =
    if exit_on_escape then Main.shortcuts_of_list [ Main.exit_on_escape ]
    else Main.shortcuts_empty ()
  in
  let board = Main.of_layout ~shortcuts house in
  Sync.push (fun () ->
      match Layout.window_opt house with
      | None -> ()
      | Some win ->
        Sdl.set_window_title win title |> ignore;
        let lw, lh = Layout.get_size house in
        let w =
          if width > 0 then Theme.scale_int width
          else Theme.scale_int (max lw (height * 4 / 3))
        in
        let h =
          if height > 0 then Theme.scale_int height
          else Theme.scale_int lh
        in
        if width > 0 || height > 0 then ignore (Sdl.set_window_size win ~w ~h));
  Main.run board;
  Main.quit ()
