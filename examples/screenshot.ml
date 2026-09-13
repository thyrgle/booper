(* Booper screenshot harness (dev tool).
   Renders a UI offscreen and saves a PNG:
     ./screenshot.exe [dark|light|nord|dracula|solarized|gruvbox|mocha|form] [output.png] *)

let arg i default = if Array.length Sys.argv > i then Sys.argv.(i) else default
let palette_name = arg 1 "dark"
let outfile = arg 2 "/tmp/booper_shot.png"

module Sdl = Tsdl.Sdl

let form_ui theme =
  let open Booper in
  let name = input ~width:240 ~prompt:"your name" theme () in
  let notify = check theme ~state:true "Enable notifications" in
  let dark_mode = switch theme ~state:true () in
  let volume_readout = ref None in
  let volume =
    slider theme ~value:40 ~width:200 ~on_change:(fun v ->
        match !volume_readout with
        | Some l -> Booper.W.set_text (L.widget l) (Printf.sprintf "%d%%" v)
        | None -> ())
      ()
  in
  volume_readout := Some (label theme ~color:Primary "40%");
  column theme ~sep:16
    [
      card theme
        [
          heading theme "Profile";
          row theme ~sep:12 [ label theme "Name"; name.layout ];
        ];
      card theme
        [
          heading theme "Preferences";
          notify.layout;
          row theme ~sep:12 [ label theme "Dark mode"; dark_mode.layout ];
          divider ~width:320 theme;
          row theme ~sep:12
            [ label theme "Volume"; volume.layout; Option.get !volume_readout ];
        ];
      row theme ~sep:12
        [
          button theme ~on_click:(fun () -> print_endline "saved") "Save settings";
          button theme ~variant:Secondary ~on_click:(fun () ->
              print_endline "cancelled") "Cancel";
          button theme ~variant:Danger ~small:true ~on_click:(fun () ->
              print_endline "deleted") "Delete";
        ];
    ]
module Sdl_image = Tsdl_image

let is_form = palette_name = "form"

let theme =
  Booper.Theme.create ~radius:14 ~font_size:15
    (Booper.Theme.preset (if is_form then "nord" else palette_name))

let stat_card theme icon value caption accent =
  Booper.card ~padding:20 ~sep:8 theme
    [
      Booper.row theme ~sep:8
        [
          Booper.icon theme ~color:accent icon;
          Booper.label theme ~size:Booper.Caption ~color:Booper.Muted caption;
        ];
      Booper.label theme ~size:Booper.H2 ~weight:`Semibold value;
    ]

let progress_row theme caption percent color =
  Booper.column theme ~sep:8
    [
      Booper.row theme ~sep:8
        [
          Booper.label theme ~color:Booper.Muted caption;
          Booper.label theme ~color
            (Printf.sprintf "%d%%" (int_of_float (100. *. percent)));
        ];
      (Booper.progress ~width:220 theme percent ~fill:color).layout;
    ]

let dash_ui theme =
  let open Booper in
  column theme ~sep:20
    [
      row theme ~sep:8
        [
          icon theme ~color:Primary "line-chart";
          title theme "Dashboard";
          spacer ~w:40 ~h:1 ();
          label theme ~color:Muted ("palette: " ^ palette_name);
        ];
      row theme ~sep:16
        [
          stat_card theme "users" "1,284" "Active users" Primary;
          stat_card theme "thumbs-o-up" "98.2%" "Satisfaction" Success;
          stat_card theme "exclamation-triangle" "3" "Open alerts" Warning;
        ];
      card theme
        [
          heading theme "Weekly goals";
          divider ~width:520 theme;
          progress_row theme "Deploys" 0.72 Secondary;
          progress_row theme "Code review" 0.45 Primary;
          progress_row theme "Bug fixes" 0.90 Success;
        ];
      row theme ~sep:12
        [
          button theme ~on_click:(fun () -> print_endline "new report!")
            "New report";
          button theme ~variant:Ghost ~on_click:(fun () ->
              print_endline "exported") "Export...";
          button theme ~variant:Secondary ~on_click:(fun () ->
              print_endline "cancelled") "Cancel";
        ];
    ]

let ui = if is_form then form_ui theme else dash_ui theme

let () =
  Booper.Theme.apply theme;
  let house =
    Bogue.Layout.tower ~name:"booper" ~resize:Bogue.Layout.Resize.Linear
      ~background:(Bogue.Layout.opaque_bg theme.palette.bg)
      [ ui ]
  in
  let frame = ref 0 in
  let grab () =
    match Bogue.Layout.window_opt house with
    | None -> prerr_endline "screenshot: no window"
    | Some win -> (
        match Sdl.get_renderer win with
        | Error (`Msg m) -> prerr_endline ("screenshot: no renderer: " ^ m)
        | Ok r -> (
            let w, h = Result.get_ok (Sdl.get_renderer_output_size r) in
            let pixels =
              Bigarray.Array1.create Bigarray.char Bigarray.c_layout ((w * h) * 4)
            in
            match
              Sdl.render_read_pixels r None
                (Some Sdl.Pixel.format_argb8888)
                pixels (w * 4)
            with
            | Error (`Msg m) -> prerr_endline ("screenshot: " ^ m)
            | Ok () -> (
                match
                  Sdl.create_rgb_surface_from pixels ~w ~h ~depth:32
                    ~pitch:(w * 4) 0x00ff0000l 0x0000ff00l 0x000000ffl
                    0xff000000l
                with
                | Error (`Msg m) -> prerr_endline ("screenshot: " ^ m)
                | Ok surf ->
                  let code = Tsdl_image.Image.save_png surf outfile in
                  if code <> 0 then prerr_endline "screenshot: save failed";
                  prerr_endline ("screenshot saved: " ^ outfile);
                  raise Bogue.Main.Exit)))
  in
  let board = Bogue.Main.of_layout house in
  Bogue.Main.run ~after_display:(fun () ->
      incr frame;
      if !frame >= 2 then grab ())
    board;
  Bogue.Main.quit ()
