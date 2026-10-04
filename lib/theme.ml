(* Booper.Theme

   Modern color palettes and typography for Bogue applications.

   A [palette] is a set of semantic colors (background, surface, primary...).
   A [t] bundles a palette with layout metrics (corner radius, spacing,
   font sizes) and font resolution. Presets are provided for popular
   palettes: dark, light, nord, dracula, solarized, gruvbox, catppuccin. *)

type palette = {
  bg : Color.t;             (* window background *)
  surface : Color.t;        (* cards, panels *)
  surface_alt : Color.t;    (* inputs, hover, subtle fills *)
  border : Color.t;         (* hairline borders *)
  text : Color.t;           (* main text *)
  text_muted : Color.t;     (* secondary text *)
  primary : Color.t;        (* main accent *)
  on_primary : Color.t;     (* text/icons over primary *)
  secondary : Color.t;      (* second accent *)
  success : Color.t;
  warning : Color.t;
  danger : Color.t;
}

type t = {
  palette : palette;
  radius : int;             (* default corner radius *)
  spacing : int;            (* default gap between widgets *)
  font_size : int;          (* base font size *)
  width : int;              (* preferred window width  (0 = hug content) *)
  height : int;             (* preferred window height (0 = hug content) *)
  fonts : (string * string) Lazy.t;
  (* (regular, semibold) font paths; "" means keep Bogue's default font. *)
}

let create ?(radius = 10) ?(spacing = 14) ?(font_size = 15)
    ?(font = "") ?(font_semibold = "") ?(width = 0) ?(height = 0) palette =
  let fonts =
    lazy
      (let regular_candidates =
         (if font = "" then [] else [ font ])
         @ [ "FiraSans-Regular.ttf"; "NotoSans-Regular.ttf"; "Ubuntu-R.ttf";
             "DejaVuSans.ttf" ]
       in
       let semibold_candidates =
         (if font_semibold = "" then [] else [ font_semibold ])
         @ [ "FiraSans-SemiBold.ttf"; "NotoSans-Bold.ttf"; "Ubuntu-B.ttf";
             "DejaVuSans-Bold.ttf" ]
       in
       let find candidates =
         match List.find_map Bogue.Theme.get_font_path_opt candidates with
         | Some p -> p
         | None -> ""
       in
       (find regular_candidates, find semibold_candidates))
  in
  { palette; radius; spacing; font_size; width; height; fonts }

(* {2 Presets} *)

let dark =
  { bg = Bogue.RGB.find_color "#1e2127"; surface = Bogue.RGB.find_color "#2c313a";
    surface_alt = Bogue.RGB.find_color "#3a3f4b"; border = Bogue.RGB.find_color "#3e4451";
    text = Bogue.RGB.find_color "#d7dae0"; text_muted = Bogue.RGB.find_color "#9da5b4";
    primary = Bogue.RGB.find_color "#61afef"; on_primary = Bogue.RGB.find_color "#ffffff";
    secondary = Bogue.RGB.find_color "#98c379"; success = Bogue.RGB.find_color "#98c379";
    warning = Bogue.RGB.find_color "#e5c07b"; danger = Bogue.RGB.find_color "#e06c75" }

let light =
  { bg = Bogue.RGB.find_color "#f8fafc"; surface = Bogue.RGB.find_color "#ffffff";
    surface_alt = Bogue.RGB.find_color "#eef2f7"; border = Bogue.RGB.find_color "#dbe2ea";
    text = Bogue.RGB.find_color "#0f172a"; text_muted = Bogue.RGB.find_color "#64748b";
    primary = Bogue.RGB.find_color "#3b82f6"; on_primary = Bogue.RGB.find_color "#ffffff";
    secondary = Bogue.RGB.find_color "#10b981"; success = Bogue.RGB.find_color "#22c55e";
    warning = Bogue.RGB.find_color "#f59e0b"; danger = Bogue.RGB.find_color "#ef4444" }

let nord =
  { bg = Bogue.RGB.find_color "#2e3440"; surface = Bogue.RGB.find_color "#3b4252";
    surface_alt = Bogue.RGB.find_color "#434c5e"; border = Bogue.RGB.find_color "#4c566a";
    text = Bogue.RGB.find_color "#eceff4"; text_muted = Bogue.RGB.find_color "#d8dee9";
    primary = Bogue.RGB.find_color "#88c0d0"; on_primary = Bogue.RGB.find_color "#2e3440";
    secondary = Bogue.RGB.find_color "#81a1c1"; success = Bogue.RGB.find_color "#a3be8c";
    warning = Bogue.RGB.find_color "#ebcb8b"; danger = Bogue.RGB.find_color "#bf616a" }

let dracula =
  { bg = Bogue.RGB.find_color "#282a36"; surface = Bogue.RGB.find_color "#31334a";
    surface_alt = Bogue.RGB.find_color "#44475a"; border = Bogue.RGB.find_color "#44475a";
    text = Bogue.RGB.find_color "#f8f8f2"; text_muted = Bogue.RGB.find_color "#a9b0c9";
    primary = Bogue.RGB.find_color "#bd93f9"; on_primary = Bogue.RGB.find_color "#282a36";
    secondary = Bogue.RGB.find_color "#ff79c6"; success = Bogue.RGB.find_color "#50fa7b";
    warning = Bogue.RGB.find_color "#f1fa8c"; danger = Bogue.RGB.find_color "#ff5555" }

let solarized =
  { bg = Bogue.RGB.find_color "#002b36"; surface = Bogue.RGB.find_color "#073642";
    surface_alt = Bogue.RGB.find_color "#0d4552"; border = Bogue.RGB.find_color "#12586a";
    text = Bogue.RGB.find_color "#93a1a1"; text_muted = Bogue.RGB.find_color "#657b83";
    primary = Bogue.RGB.find_color "#268bd2"; on_primary = Bogue.RGB.find_color "#fdf6e3";
    secondary = Bogue.RGB.find_color "#2aa198"; success = Bogue.RGB.find_color "#859900";
    warning = Bogue.RGB.find_color "#b58900"; danger = Bogue.RGB.find_color "#dc322f" }

let gruvbox =
  { bg = Bogue.RGB.find_color "#282828"; surface = Bogue.RGB.find_color "#3c3836";
    surface_alt = Bogue.RGB.find_color "#504945"; border = Bogue.RGB.find_color "#5a524c";
    text = Bogue.RGB.find_color "#ebdbb2"; text_muted = Bogue.RGB.find_color "#a89984";
    primary = Bogue.RGB.find_color "#fe8019"; on_primary = Bogue.RGB.find_color "#282828";
    secondary = Bogue.RGB.find_color "#83a598"; success = Bogue.RGB.find_color "#b8bb26";
    warning = Bogue.RGB.find_color "#fabd2f"; danger = Bogue.RGB.find_color "#fb4934" }

let mocha =
  { bg = Bogue.RGB.find_color "#1e1e2e"; surface = Bogue.RGB.find_color "#28283d";
    surface_alt = Bogue.RGB.find_color "#3d3d55"; border = Bogue.RGB.find_color "#48485e";
    text = Bogue.RGB.find_color "#cdd6f4"; text_muted = Bogue.RGB.find_color "#a6adc8";
    primary = Bogue.RGB.find_color "#89b4fa"; on_primary = Bogue.RGB.find_color "#1e1e2e";
    secondary = Bogue.RGB.find_color "#f5c2e7"; success = Bogue.RGB.find_color "#a6e3a1";
    warning = Bogue.RGB.find_color "#f9e2af"; danger = Bogue.RGB.find_color "#f38ba8" }

let presets =
  [ "dark", dark; "light", light; "nord", nord; "dracula", dracula;
    "solarized", solarized; "gruvbox", gruvbox; "mocha", mocha ]

let preset name =
  match List.assoc_opt name presets with
  | Some p -> p
  | None ->
    invalid_arg
      (Printf.sprintf "Booper.Theme.preset: unknown palette %S (available: %s)"
         name (String.concat ", " (List.map fst presets)))

(* {2 Applying a theme to Bogue globals} *)

let last_applied : t option ref = ref None

let apply t =
  let already =
    match !last_applied with Some a -> a == t | None -> false
  in
  if not already then begin
    let regular, semibold = Lazy.force t.fonts in
    let pick () = if regular = "" then semibold else regular in
    (match pick () with
     | "" -> ()
     | path ->
       Bogue.Theme.set_label_font path;
       Bogue.Theme.set_text_font path);
    Bogue.RGB.set_text_color t.palette.text;
    Bogue.RGBA.set_text_color (Color.opaque t.palette.text);
    last_applied := Some t
  end

let regular_font t =
  apply t;
  let regular, semibold = Lazy.force t.fonts in
  if regular = "" then semibold else regular

let semibold_font t =
  apply t;
  let regular, semibold = Lazy.force t.fonts in
  if semibold = "" then regular else semibold
