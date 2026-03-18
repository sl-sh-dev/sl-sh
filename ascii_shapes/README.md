Public API

┌────────────────────────────────────────────┬───────────────────────────────────────────────────────────────────┐
│                  Function                  │                              Purpose                              │
├────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
│ best_char(&ShapeVector, contrast)          │ Find the best-matching ASCII character for a sampling vector      │
├────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
│ shape_vector(char) -> Option<&ShapeVector> │ Look up a character's pre-computed shape vector                   │
├────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
│ sample_cell(&[f32], w, h) -> ShapeVector   │ Compute a sampling vector from a lightness grid (for image cells) │
├────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
│ rgb_to_lightness(r, g, b) -> f32           │ Convert RGB pixel to luminance                                    │
├────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────┤
│ apply_contrast                             │ Internal, but the contrast param on best_char exposes it          │
└────────────────────────────────────────────┴───────────────────────────────────────────────────────────────────┘

Usage

- Normal build: cargo build — uses checked-in generated.rs, no font dependencies
- Regenerate: cargo build -p ascii_shapes --features generate — re-rasterizes the font and overwrites0wwnerated.rs
- Add a new font: drop a .ttf in fonts/, update build.rs to point at it, run with --features generate