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
  { bg = Color.of_hex "#1e2127"; surface = Color.of_hex "#2c313a";
    surface_alt = Color.of_hex "#3a3f4b"; border = Color.of_hex "#3e4451";
    text = Color.of_hex "#d7dae0"; text_muted = Color.of_hex "#9da5b4";
    primary = Color.of_hex "#61afef"; on_primary = Color.of_hex "#ffffff";
    secondary = Color.of_hex "#98c379"; success = Color.of_hex "#98c379";
    warning = Color.of_hex "#e5c07b"; danger = Color.of_hex "#e06c75" }

let light =
  { bg = Color.of_hex "#f8fafc"; surface = Color.of_hex "#ffffff";
    surface_alt = Color.of_hex "#eef2f7"; border = Color.of_hex "#dbe2ea";
    text = Color.of_hex "#0f172a"; text_muted = Color.of_hex "#64748b";
    primary = Color.of_hex "#3b82f6"; on_primary = Color.of_hex "#ffffff";
    secondary = Color.of_hex "#10b981"; success = Color.of_hex "#22c55e";
    warning = Color.of_hex "#f59e0b"; danger = Color.of_hex "#ef4444" }

let nord =
  { bg = Color.of_hex "#2e3440"; surface = Color.of_hex "#3b4252";
    surface_alt = Color.of_hex "#434c5e"; border = Color.of_hex "#4c566a";
    text = Color.of_hex "#eceff4"; text_muted = Color.of_hex "#d8dee9";
    primary = Color.of_hex "#88c0d0"; on_primary = Color.of_hex "#2e3440";
    secondary = Color.of_hex "#81a1c1"; success = Color.of_hex "#a3be8c";
    warning = Color.of_hex "#ebcb8b"; danger = Color.of_hex "#bf616a" }

let dracula =
  { bg = Color.of_hex "#282a36"; surface = Color.of_hex "#31334a";
    surface_alt = Color.of_hex "#44475a"; border = Color.of_hex "#44475a";
    text = Color.of_hex "#f8f8f2"; text_muted = Color.of_hex "#a9b0c9";
    primary = Color.of_hex "#bd93f9"; on_primary = Color.of_hex "#282a36";
    secondary = Color.of_hex "#ff79c6"; success = Color.of_hex "#50fa7b";
    warning = Color.of_hex "#f1fa8c"; danger = Color.of_hex "#ff5555" }

let solarized =
  { bg = Color.of_hex "#002b36"; surface = Color.of_hex "#073642";
    surface_alt = Color.of_hex "#0d4552"; border = Color.of_hex "#12586a";
    text = Color.of_hex "#93a1a1"; text_muted = Color.of_hex "#657b83";
    primary = Color.of_hex "#268bd2"; on_primary = Color.of_hex "#fdf6e3";
    secondary = Color.of_hex "#2aa198"; success = Color.of_hex "#859900";
    warning = Color.of_hex "#b58900"; danger = Color.of_hex "#dc322f" }

let gruvbox =
  { bg = Color.of_hex "#282828"; surface = Color.of_hex "#3c3836";
    surface_alt = Color.of_hex "#504945"; border = Color.of_hex "#5a524c";
    text = Color.of_hex "#ebdbb2"; text_muted = Color.of_hex "#a89984";
    primary = Color.of_hex "#fe8019"; on_primary = Color.of_hex "#282828";
    secondary = Color.of_hex "#83a598"; success = Color.of_hex "#b8bb26";
    warning = Color.of_hex "#fabd2f"; danger = Color.of_hex "#fb4934" }

let mocha =
  { bg = Color.of_hex "#1e1e2e"; surface = Color.of_hex "#28283d";
    surface_alt = Color.of_hex "#3d3d55"; border = Color.of_hex "#48485e";
    text = Color.of_hex "#cdd6f4"; text_muted = Color.of_hex "#a6adc8";
    primary = Color.of_hex "#89b4fa"; on_primary = Color.of_hex "#1e1e2e";
    secondary = Color.of_hex "#f5c2e7"; success = Color.of_hex "#a6e3a1";
    warning = Color.of_hex "#f9e2af"; danger = Color.of_hex "#f38ba8" }

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
