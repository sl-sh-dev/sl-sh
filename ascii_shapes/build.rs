#[cfg(feature = "generate")]
mod codegen {
    use fontdue::{Font, FontSettings};
    use std::collections::BTreeMap;
    use std::io::Write;
    use std::path::Path;

    /// Unicode ranges to include in the shape vector table.
    /// Each range is checked against the font — only characters that have
    /// glyphs in the font are actually included.
    const CHAR_RANGES: &[(u32, u32, &str)] = &[
        (0x0020, 0x007E, "Basic Latin (printable ASCII)"),
        (0x00A0, 0x00FF, "Latin-1 Supplement"),
        (0x2190, 0x21FF, "Arrows"),
        (0x2200, 0x22FF, "Mathematical Operators"),
        (0x2300, 0x23FF, "Miscellaneous Technical"),
        (0x2500, 0x257F, "Box Drawing"),
        (0x2580, 0x259F, "Block Elements"),
        (0x25A0, 0x25FF, "Geometric Shapes"),
        (0x2600, 0x26FF, "Miscellaneous Symbols"),
        (0x2700, 0x27BF, "Dingbats"),
    ];

    /// Rasterize size in pixels. Larger = more accurate shape vectors.
    /// This only affects the build-time computation, not runtime.
    const RASTER_SIZE: f32 = 64.0;

    /// Sampling circle layout parameters.
    /// These must match the runtime `sample_cell` function in lib.rs.
    struct CircleParams {
        cx: f32,
        cy: f32,
        rx: f32,
        ry: f32,
    }

    fn sampling_circles(w: f32, h: f32) -> [CircleParams; 6] {
        let rx = w * 0.28;
        let ry = h * 0.20;
        [
            CircleParams { cx: w * 0.30, cy: h * 0.20, rx, ry },
            CircleParams { cx: w * 0.70, cy: h * 0.13, rx, ry },
            CircleParams { cx: w * 0.30, cy: h * 0.53, rx, ry },
            CircleParams { cx: w * 0.70, cy: h * 0.47, rx, ry },
            CircleParams { cx: w * 0.30, cy: h * 0.87, rx, ry },
            CircleParams { cx: w * 0.70, cy: h * 0.80, rx, ry },
        ]
    }

    fn compute_shape_vector(bitmap: &[f32], width: usize, height: usize) -> [f32; 6] {
        let w = width as f32;
        let h = height as f32;
        let circles = sampling_circles(w, h);
        let mut result = [0.0_f32; 6];

        for (i, circle) in circles.iter().enumerate() {
            let mut sum = 0.0_f32;
            let mut count = 0u32;

            let y_start = ((circle.cy - circle.ry).max(0.0)) as usize;
            let y_end = ((circle.cy + circle.ry).min(h - 1.0)) as usize;
            let x_start = ((circle.cx - circle.rx).max(0.0)) as usize;
            let x_end = ((circle.cx + circle.rx).min(w - 1.0)) as usize;

            for y in y_start..=y_end {
                for x in x_start..=x_end {
                    let dx = (x as f32 - circle.cx) / circle.rx;
                    let dy = (y as f32 - circle.cy) / circle.ry;
                    if dx * dx + dy * dy <= 1.0 {
                        sum += bitmap[y * width + x];
                        count += 1;
                    }
                }
            }

            result[i] = if count > 0 { sum / count as f32 } else { 0.0 };
        }

        result
    }

    pub fn generate() {
        let font_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("fonts")
            .join("JetBrainsMono-Regular.ttf");

        let font_data = std::fs::read(&font_path).unwrap_or_else(|e| {
            panic!("Failed to read font at {}: {}", font_path.display(), e);
        });

        let font = Font::from_bytes(font_data, FontSettings::default()).unwrap_or_else(|e| {
            panic!("Failed to parse font: {}", e);
        });

        // Collect all characters to rasterize: union of CHAR_RANGES filtered
        // by what the font actually supports.
        let font_chars = font.chars();
        let mut chars_to_process: Vec<char> = Vec::new();
        for &(start, end, label) in CHAR_RANGES {
            let mut count = 0u32;
            for cp in start..=end {
                if let Some(ch) = char::from_u32(cp) {
                    if font_chars.contains_key(&ch) {
                        chars_to_process.push(ch);
                        count += 1;
                    }
                }
            }
            println!("cargo::warning={}: {} chars", label, count);
        }
        chars_to_process.sort();
        chars_to_process.dedup();

        let mut char_vectors: BTreeMap<char, [f32; 6]> = BTreeMap::new();

        // Get the metrics for a reference character to determine cell size.
        let (ref_metrics, _) = font.rasterize('M', RASTER_SIZE);
        let cell_width = ref_metrics.advance_width as usize;
        let cell_height = (RASTER_SIZE * 1.2) as usize; // approximate line height

        println!("cargo::warning=Total characters to rasterize: {}", chars_to_process.len());

        for ch in &chars_to_process {
            let ch = *ch;
            let (metrics, bitmap) = font.rasterize(ch, RASTER_SIZE);

            // Place the glyph bitmap into a full cell-sized canvas.
            let mut canvas = vec![0.0_f32; cell_width * cell_height];

            // Compute glyph placement within the cell.
            let baseline_y = (RASTER_SIZE * 0.8) as i32; // approximate baseline
            let glyph_y_offset = baseline_y - metrics.height as i32 - metrics.ymin;
            let glyph_x_offset = metrics.xmin.max(0) as usize;

            for gy in 0..metrics.height {
                for gx in 0..metrics.width {
                    let canvas_x = glyph_x_offset + gx;
                    let canvas_y = (glyph_y_offset + gy as i32) as usize;

                    if canvas_x < cell_width && canvas_y < cell_height {
                        // fontdue gives coverage as u8 (0-255)
                        let coverage = bitmap[gy * metrics.width + gx] as f32 / 255.0;
                        canvas[canvas_y * cell_width + canvas_x] = coverage;
                    }
                }
            }

            let vector = compute_shape_vector(&canvas, cell_width, cell_height);
            char_vectors.insert(ch, vector);
        }

        // Normalize: find the max value per component across all characters,
        // then divide each component by its max.
        let mut max_per_component = [0.0_f32; 6];
        for vector in char_vectors.values() {
            for i in 0..6 {
                if vector[i] > max_per_component[i] {
                    max_per_component[i] = vector[i];
                }
            }
        }

        for vector in char_vectors.values_mut() {
            for i in 0..6 {
                if max_per_component[i] > f32::EPSILON {
                    vector[i] /= max_per_component[i];
                }
            }
        }

        // Generate the PHF map source code.
        let out_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
        let out_path = out_dir.join("generated.rs");
        let mut file = std::fs::File::create(&out_path).unwrap();

        writeln!(file, "//! Auto-generated shape vector data.").unwrap();
        writeln!(file, "//! DO NOT EDIT — regenerate with `cargo build --features generate`.").unwrap();
        writeln!(file, "//!").unwrap();
        writeln!(file, "//! Font: JetBrains Mono Regular").unwrap();
        writeln!(file, "//! Raster size: {RASTER_SIZE}px").unwrap();
        writeln!(file, "//! Characters: {}", char_vectors.len()).unwrap();
        writeln!(file).unwrap();

        let mut map_builder = phf_codegen::Map::new();
        for (&ch, vector) in &char_vectors {
            let value = format!(
                "[{:.6}, {:.6}, {:.6}, {:.6}, {:.6}, {:.6}]",
                vector[0], vector[1], vector[2], vector[3], vector[4], vector[5]
            );
            // phf_codegen expects the key as a string that will be a Rust expression
            map_builder.entry(ch, &value);
        }

        writeln!(
            file,
            "pub static SHAPE_VECTORS: phf::Map<char, [f32; 6]> = {};",
            map_builder.build()
        )
        .unwrap();

        println!("cargo::rerun-if-changed=fonts/JetBrainsMono-Regular.ttf");
        println!("cargo::rerun-if-changed=build.rs");
    }
}

fn main() {
    #[cfg(feature = "generate")]
    codegen::generate();
}
