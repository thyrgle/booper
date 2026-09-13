(* Hello Booper — a minimal example. *)

let theme = Booper.Theme.create ~radius:10 Booper.Theme.dark

let count = ref 0

(* The label whose text is updated on every click. *)
let counter =
  let l = Booper.label theme ~color:Booper.Primary "clicked 0 times" in
  ref l

let update_counter () =
  let w = Booper.L.widget !counter in
  Bogue.Widget.set_text w (Printf.sprintf "clicked %d times" !count);
  Bogue.Update.push w

let ui =
  Booper.column theme ~sep:20 ~align:Bogue.Draw.Center
    [
      Booper.title theme "Hello, Booper";
      Booper.label theme ~size:Booper.Body ~color:Booper.Muted
        "A modern look for OCaml's Bogue GUI library.";
      Booper.row theme ~sep:14
        [
          Booper.button theme ~on_click:(fun () ->
              incr count;
              update_counter ())
            "Click me";
          !counter;
        ];
      Booper.row theme ~sep:14
        [
          Booper.button theme ~variant:Booper.Secondary ~small:true
            ~on_click:(fun () -> print_endline "secondary") "Secondary";
          Booper.button theme ~variant:Booper.Ghost ~small:true
            ~on_click:(fun () -> print_endline "ghost") "Ghost";
          Booper.button theme ~variant:Booper.Danger ~small:true
            ~on_click:(fun () -> print_endline "danger") "Danger";
        ];
    ]

let () = Booper.run ~title:"Booper — Hello" ~width:520 theme ui
