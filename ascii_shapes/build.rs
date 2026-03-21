#[cfg(feature = "generate")]
mod codegen {
    use fontdue::{Font, FontSettings};
    use std::collections::BTreeMap;
    use std::io::Write;
    use std::path::Path;

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

    /// Returns true for characters that are useful for shape-based matching.
    /// Excludes control characters, combining marks, and other categories
    /// that don't render as standalone visible glyphs.
    fn is_useful_char(ch: char) -> bool {
        // Always include printable ASCII
        if (' '..='~').contains(&ch) {
            return true;
        }
        // Skip C0/C1 control characters and surrogates
        if ch < '\u{00A0}' {
            return false;
        }
        // Skip combining diacritical marks (U+0300..U+036F) — they modify
        // the previous character rather than standing alone.
        if ('\u{0300}'..='\u{036F}').contains(&ch) {
            return false;
        }
        // Skip variation selectors and other non-visible modifiers
        if ('\u{FE00}'..='\u{FE0F}').contains(&ch) {
            return false;
        }
        // Skip private use area (powerline glyphs, etc. — non-standard)
        if ('\u{E000}'..='\u{F8FF}').contains(&ch) {
            return false;
        }
        true
    }

    fn squared_distance(a: &[f32; 6], b: &[f32; 6]) -> f32 {
        let mut sum = 0.0_f32;
        for i in 0..6 {
            let d = a[i] - b[i];
            sum += d * d;
        }
        sum
    }

    pub fn generate() {
        // Font path: use ASCII_SHAPES_FONT env var if set, otherwise the
        // bundled JetBrains Mono.
        let font_path = match std::env::var("ASCII_SHAPES_FONT") {
            Ok(p) => {
                let p = Path::new(&p).to_path_buf();
                println!("cargo::warning=Using custom font: {}", p.display());
                p
            }
            Err(_) => {
                let p = Path::new(env!("CARGO_MANIFEST_DIR"))
                    .join("fonts")
                    .join("JetBrainsMono-Regular.ttf");
                println!("cargo::warning=Using bundled font: {}", p.display());
                p
            }
        };

        let font_data = std::fs::read(&font_path).unwrap_or_else(|e| {
            panic!("Failed to read font at {}: {}", font_path.display(), e);
        });

        let font = Font::from_bytes(font_data, FontSettings::default()).unwrap_or_else(|e| {
            panic!("Failed to parse font: {}", e);
        });

        // Use every character the font supports, filtered to useful glyphs.
        let mut chars_to_process: Vec<char> = font
            .chars()
            .keys()
            .copied()
            .filter(|&ch| is_useful_char(ch))
            .collect();
        chars_to_process.sort();

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

        // --- Quantized lookup table generation ---

        let n: usize = std::env::var("ASCII_SHAPES_QUANT_LEVELS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(8);

        let table_size = n.pow(6);
        println!("cargo::warning=Quantization levels: {n}, table size: {table_size}");

        // Collect all character vectors into a Vec for brute-force search.
        let entries: Vec<(char, [f32; 6])> = char_vectors.into_iter().collect();

        let mut lookup = vec![' ' as u32; table_size];

        for flat_idx in 0..table_size {
            // Decompose flat index into 6 quantized coordinates.
            let mut remaining = flat_idx;
            let mut query = [0.0_f32; 6];
            for dim in (0..6).rev() {
                let q = remaining % n;
                remaining /= n;
                // Bucket center: (q + 0.5) / n
                query[dim] = (q as f32 + 0.5) / n as f32;
            }

            // Brute-force nearest neighbor.
            let mut best_ch = ' ';
            let mut best_dist = f32::MAX;
            for &(ch, ref vec) in &entries {
                let dist = squared_distance(&query, vec);
                if dist < best_dist {
                    best_dist = dist;
                    best_ch = ch;
                }
            }

            lookup[flat_idx] = best_ch as u32;
        }

        // Write generated.rs
        let out_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
        let out_path = out_dir.join("generated.rs");
        let mut file = std::fs::File::create(&out_path).unwrap();

        writeln!(file, "//! Auto-generated quantized lookup table.").unwrap();
        writeln!(file, "//! DO NOT EDIT — regenerate with `cargo build -p ascii_shapes --features generate`.").unwrap();
        writeln!(file, "//!").unwrap();
        writeln!(file, "//! Font: {}", font_path.display()).unwrap();
        writeln!(file, "//! Raster size: {RASTER_SIZE}px").unwrap();
        writeln!(file, "//! Characters: {}", entries.len()).unwrap();
        writeln!(file, "//! Quantization levels: {n}").unwrap();
        writeln!(file, "//! Table size: {table_size}").unwrap();
        writeln!(file).unwrap();
        writeln!(file, "pub const N: usize = {n};").unwrap();
        writeln!(file).unwrap();

        // Write the lookup table, 16 entries per line, as hex u32 values.
        let cols = 16;
        writeln!(file, "pub static LOOKUP: [u32; {}] = [", table_size).unwrap();
        for (i, &val) in lookup.iter().enumerate() {
            if i % cols == 0 {
                write!(file, "    ").unwrap();
            }
            write!(file, "0x{:08X},", val).unwrap();
            if i % cols == cols - 1 || i == table_size - 1 {
                writeln!(file).unwrap();
            }
        }
        writeln!(file, "];").unwrap();

        println!("cargo::rerun-if-changed={}", font_path.display());
        println!("cargo::rerun-if-changed=build.rs");
        println!("cargo::rerun-if-env-changed=ASCII_SHAPES_QUANT_LEVELS");
    }
}

fn main() {
    #[cfg(feature = "generate")]
    codegen::generate();
}
