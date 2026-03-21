Public API

┌──────────────────────────────────────────────────────────────────────┬───────────────────────────────────────────────────────────────────────────┐
│                              Function                              │                                 Purpose                                   │
├──────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────┤
│ best_char(&ShapeVector, contrast)                                  │ Find the best-matching character for a sampling vector (O(1) lookup)      │
├──────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────┤
│ best_char_directional(&ShapeVector, &ExternalVector, exponent)     │ Best match using both global and directional contrast enhancement         │
├──────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────┤
│ sample_cell(&[f32], w, h) -> ShapeVector                           │ Compute a sampling vector from a lightness grid (for image cells)         │
├──────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────┤
│ sample_cell_external(&[f32], img_w, img_h, x, y, w, h)            │ Compute 10 external samples from neighboring cells for directional        │
│   -> ExternalVector                                                │   contrast enhancement                                                   │
├──────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────┤
│ apply_contrast(&ShapeVector, exponent) -> ShapeVector              │ Global contrast enhancement (raise normalized components to a power)      │
├──────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────┤
│ apply_directional_contrast(&ShapeVector, &ExternalVector, exponent)│ Directional contrast using external neighbor samples                      │
│   -> ShapeVector                                                   │                                                                           │
├──────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────┤
│ rgb_to_lightness(r, g, b) -> f32                                   │ Convert RGB pixel to luminance                                            │
├──────────────────────────────────────────────────────────────────────┼───────────────────────────────────────────────────────────────────────────┤
│ AFFECTING_EXTERNAL_INDICES: [&[usize]; 6]                          │ Which external circles affect each internal component                     │
└──────────────────────────────────────────────────────────────────────┴───────────────────────────────────────────────────────────────────────────┘

Types: ShapeVector = [f32; 6], ExternalVector = [f32; 10]

Architecture

Character matching uses a precomputed quantized 6D lookup table (default 8 levels
per dimension = 262,144 entries). At runtime, the shape vector is quantized and
used as a flat array index -- O(1) instead of linear scan over all characters.

Usage

- Normal build: cargo build -- uses checked-in generated.rs, no font dependencies
- Regenerate: cargo build -p ascii_shapes --features generate -- re-rasterizes the font and overwrites generated.rs
- Custom font: ASCII_SHAPES_FONT=/path/to/font.ttf cargo build -p ascii_shapes --features generate
  If ASCII_SHAPES_FONT is not set, the bundled JetBrains Mono is used.
  The build dynamically discovers every character the font supports (filtering out
  control characters, combining marks, and other non-visible glyphs).
- Add a new font: drop a .ttf in fonts/ (or anywhere), set ASCII_SHAPES_FONT to
  its path, and run with --features generate
- Quantization levels: ASCII_SHAPES_QUANT_LEVELS=N cargo build -p ascii_shapes --features generate
  Default is 8. Higher values give finer matching at the cost of larger generated.rs
  (table size = N^6). The value is baked into generated.rs as `pub const N`.