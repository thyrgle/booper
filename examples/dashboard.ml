(* Booper dashboard — stat cards and progress bars.
   Choose a palette on the command line:
     ./dashboard.exe [dark|light|nord|dracula|solarized|gruvbox|mocha] *)

let palette_name =
  match Sys.argv with
  | [| _; name |] -> name
  | _ -> "dracula"

let theme = Booper.Theme.create ~radius:14 ~font_size:15 (Booper.Theme.preset palette_name)

let stat_card icon value caption accent =
  Booper.card ~padding:20 ~sep:8 theme
    [
      Booper.row theme ~sep:8
        [
          Booper.icon theme ~color:accent icon;
          Booper.label theme ~size:Booper.Caption ~color:Booper.Muted caption;
        ];
      Booper.label theme ~size:Booper.H2 ~weight:`Semibold value;
    ]

let progress_row caption percent color =
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

let ui =
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
          stat_card "users" "1,284" "Active users" Primary;
          stat_card "thumbs-o-up" "98.2%" "Satisfaction" Success;
          stat_card "exclamation-triangle" "3" "Open alerts" Warning;
        ];
      card theme
        [
          heading theme "Weekly goals";
          divider ~width:520 theme;
          progress_row "Deploys" 0.72 Secondary;
          progress_row "Code review" 0.45 Primary;
          progress_row "Bug fixes" 0.90 Success;
        ];
      row theme ~sep:12
        [
          button theme ~on_click:(fun () -> print_endline "new report!")
            "New report";
          button theme ~variant:Ghost ~on_click:(fun () ->
              print_endline "exported") "Export...";
        ];
    ]

let () = Booper.run ~title:"Booper — Dashboard" ~width:720 theme ui
