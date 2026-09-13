# Booper

Modern UI styling for [Bogue](https://github.com/sanette/bogue), OCaml's SDL-based GUI library.

Booper provides contemporary color palettes, rounded corners, soft shadows and
flat widgets — buttons, cards, switches, sliders, progress bars, text inputs —
built on top of Bogue and styled from a single `Theme`.

## Features

- **Palettes**: built-in presets for `dark`, `light`, `nord`, `dracula`,
  `solarized`, `gruvbox` and `mocha` (Catppuccin).
- **Theming**: one place to configure corner radius, spacing, font size,
  window size and fonts.
- **Widgets**: buttons (primary/secondary/ghost/danger/success variants),
  cards, headings, labels, icons, dividers, switches, check boxes, text
  inputs, sliders and progress bars.
- **Composable**: every widget is a `Bogue.Layout.t`, so they compose
  directly with `Booper.row` / `Booper.column`.

## Dependencies

- OCaml (>= 4.14 recommended) and [dune](https://dune.build) (>= 3.0)
- [bogue](https://opam.ocaml.org/packages/bogue/)
- [tsdl](https://opam.ocaml.org/packages/tsdl/)
- `tsdl-image` (only needed for the screenshot example)

With opam:

```sh
opam install bogue tsdl tsdl-image
```

## Building

```sh
dune build
```

## Quick start

```ocaml
let theme = Booper.Theme.create ~radius:10 Booper.Theme.dark

let ui =
  Booper.column theme ~sep:16
    [
      Booper.title theme "Hello Booper";
      Booper.button theme
        ~on_click:(fun () -> print_endline "click!")
        "Click me";
    ]

let () = Booper.run ~title:"My app" theme ui
```

## Running the examples

Each example opens a native window (so run it locally, not on a headless
machine). From the project root:

```sh
# Minimal "hello world" with a button and click counter
dune exec examples/hello.exe

# A small settings form with inputs, checkboxes, switches and sliders
dune exec examples/controls.exe

# Stat cards and progress bars; optionally pick a palette
dune exec examples/dashboard.exe nord

# Dev tool: renders offscreen and saves a PNG instead of opening a window
dune exec examples/screenshot.exe dracula /tmp/booper_shot.png
```

## Project layout

- `lib/` — the Booper library (`Color`, `Theme`, `Widgets`)
- `examples/` — runnable examples and the screenshot harness
