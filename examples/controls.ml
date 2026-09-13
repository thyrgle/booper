(* Booper controls — a small settings form showing inputs, checks,
   switches and sliders. *)

let theme = Booper.Theme.create ~radius:10 ~font_size:15 Booper.Theme.nord

let set_text l text =
  let w = Booper.L.widget l in
  Bogue.Widget.set_text w text;
  Bogue.Update.push w

let ui =
  let open Booper in
  let name = input ~width:240 ~prompt:"your name" theme () in
  let notify = check theme ~state:true "Enable notifications" in
  let dark_mode = switch theme ~state:true () in
  let volume_readout = ref None in
  let volume =
    slider theme ~value:40 ~width:200 ~on_change:(fun v ->
        match !volume_readout with
        | Some l -> set_text l (Printf.sprintf "%d%%" v)
        | None -> ())
      ()
  in
  volume_readout := Some (label theme ~color:Primary "40%");

  let profile =
    card theme
      [
        heading theme "Profile";
        row theme ~sep:24 [ label theme "Name"; name.layout ];
      ]
  in
  let preferences =
    card theme
      [
        heading theme "Preferences";
        notify.layout;
        row theme ~sep:14 [ label theme "Dark mode"; dark_mode.layout ];
        divider ~width:320 theme;
        row theme ~sep:14
          [ label theme "Volume"; volume.layout; Option.get !volume_readout ];
      ]
  in
  Bogue.Layout.set_width profile (Bogue.Layout.width preferences);

  column theme ~sep:18 ~align:Bogue.Draw.Center
    [
      profile;
      spacer ~w:1 ~h:10 ();
      preferences;
      row theme ~sep:14
        [
          button theme ~on_click:(fun () ->
              Printf.printf "name=%s notify=%b dark=%b\n" (name.text ())
                (notify.state ()) (dark_mode.state ()))
            "Save settings";
          button theme ~variant:Secondary ~on_click:(fun () ->
              print_endline "cancelled") "Cancel";
        ];
    ]

let () = Booper.run ~title:"Booper — Controls" ~width:560 theme ui
