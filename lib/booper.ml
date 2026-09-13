(* Booper — modern UI styling for OCaml's Bogue library.

   Booper provides contemporary color palettes, rounded corners, soft
   shadows and flat widgets (buttons, cards, sliders, switches...) on top
   of Bogue. Quick start:

   {[
     let theme = Booper.Theme.create ~radius:10 Booper.Theme.dark in
     let ui = Booper.column ~sep:16 [
         Booper.title theme "Hello Booper";
         Booper.button theme ~on_click:(fun () -> print_endline "click!") "Click me";
       ] theme in
     Booper.run ~title:"My app" theme ui
   ]}
 *)

module Color = Color
module Theme = Theme
module W = Bogue.Widget
module L = Bogue.Layout

include Widgets
