use compile_state::state::SloshVm;
use slvm::{VMError, VMResult, Value};

/// Convert a lightness grid to an ASCII art string.
///
/// `lightness` is row-major \[0.0, 1.0\] values, `img_w` x `img_h` pixels.
/// `cols` is the desired output width in characters.
/// `contrast` is the contrast exponent (1.0 = no enhancement).
pub fn lightness_to_ascii(
    lightness: &[f32],
    img_w: usize,
    img_h: usize,
    cols: usize,
    contrast: f32,
    ascii_only: bool,
) -> String {
    if img_w == 0 || img_h == 0 || cols == 0 {
        return String::new();
    }

    // Clamp cols so cell_w >= 1.
    let cols = cols.min(img_w);
    let cell_w = img_w / cols;
    if cell_w == 0 {
        return String::new();
    }
    // Terminal characters are ~2:1 aspect ratio (tall), so each cell
    // covers twice as many vertical pixels as horizontal.
    let cell_h = cell_w * 2;
    let rows = img_h / cell_h;
    if rows == 0 {
        return String::new();
    }

    // Shape vectors measure ink density (0.0 = no ink, 1.0 = full ink).
    // Lightness is the inverse (1.0 = white = no ink), so convert to darkness.
    let darkness: Vec<f32> = lightness.iter().map(|&l| 1.0 - l).collect();

    let mut result = String::with_capacity((cols + 1) * rows);

    for row in 0..rows {
        if row > 0 {
            result.push('\n');
        }
        for col in 0..cols {
            let cx = col * cell_w;
            let cy = row * cell_h;

            let sv = ascii_shapes::sample_cell_external(
                &darkness, img_w, img_h, cx, cy, cell_w, cell_h,
            );
            let internal = ascii_shapes::sample_cell(
                &extract_cell(&darkness, img_w, cx, cy, cell_w, cell_h),
                cell_w,
                cell_h,
            );
            let ch = ascii_shapes::best_char_directional(&internal, &sv, contrast, ascii_only);
            result.push(ch);
        }
    }

    result
}

/// Extract a rectangular cell from a larger lightness grid into a flat buffer.
fn extract_cell(
    lightness: &[f32],
    img_w: usize,
    cx: usize,
    cy: usize,
    cell_w: usize,
    cell_h: usize,
) -> Vec<f32> {
    let mut buf = vec![0.0_f32; cell_w * cell_h];
    for y in 0..cell_h {
        let src_y = cy + y;
        if src_y >= lightness.len() / img_w {
            break;
        }
        for x in 0..cell_w {
            let src_x = cx + x;
            if src_x < img_w {
                buf[y * cell_w + x] = lightness[src_y * img_w + src_x];
            }
        }
    }
    buf
}

/// Load an image file and convert to ASCII art string.
pub fn image_to_ascii(
    path: &str,
    cols: usize,
    contrast: f32,
    ascii_only: bool,
) -> Result<String, String> {
    let img = image::open(path).map_err(|e| format!("failed to open image: {e}"))?;
    let gray = img.to_luma8();
    let (w, h) = gray.dimensions();
    let lightness: Vec<f32> = gray.pixels().map(|p| p.0[0] as f32 / 255.0).collect();
    Ok(lightness_to_ascii(
        &lightness,
        w as usize,
        h as usize,
        cols,
        contrast,
        ascii_only,
    ))
}

/// Decode a GIF into a vec of (ascii_string, delay_ms) pairs.
///
/// Maintains a persistent RGBA canvas across frames and handles GIF
/// disposal methods properly so incremental frames composite correctly.
pub fn gif_to_ascii_frames(
    path: &str,
    cols: usize,
    contrast: f32,
    ascii_only: bool,
) -> Result<Vec<(String, u64)>, String> {
    use gif::{DecodeOptions, DisposalMethod};

    let file = std::fs::File::open(path).map_err(|e| format!("failed to open GIF: {e}"))?;
    let mut opts = DecodeOptions::new();
    opts.set_color_output(gif::ColorOutput::RGBA);
    let mut decoder = opts
        .read_info(file)
        .map_err(|e| format!("failed to decode GIF: {e}"))?;

    let canvas_w = decoder.width() as usize;
    let canvas_h = decoder.height() as usize;
    if canvas_w == 0 || canvas_h == 0 {
        return Err("GIF has zero dimensions".to_string());
    }

    // Persistent RGBA canvas — starts as opaque white.
    let mut canvas = vec![255u8; canvas_w * canvas_h * 4];

    let mut result = Vec::new();
    while let Some(raw_frame) = decoder
        .read_next_frame()
        .map_err(|e| format!("failed to read GIF frame: {e}"))?
    {
        // Delay is in centiseconds; convert to ms, minimum 10ms.
        let delay_ms = (raw_frame.delay as u64 * 10).max(10);

        let left = raw_frame.left as usize;
        let top = raw_frame.top as usize;
        let fw = raw_frame.width as usize;
        let fh = raw_frame.height as usize;
        let dispose = raw_frame.dispose;

        // Save canvas before compositing if we need to restore it later.
        let saved = if dispose == DisposalMethod::Previous {
            Some(canvas.clone())
        } else {
            None
        };

        // Composite this frame's pixels onto the persistent canvas.
        for y in 0..fh {
            let cy = top + y;
            if cy >= canvas_h {
                break;
            }
            for x in 0..fw {
                let cx = left + x;
                if cx >= canvas_w {
                    break;
                }
                let src = (y * fw + x) * 4;
                let dst = (cy * canvas_w + cx) * 4;
                let a = raw_frame.buffer[src + 3] as f32 / 255.0;
                if a > 0.0 {
                    for c in 0..3 {
                        canvas[dst + c] = (raw_frame.buffer[src + c] as f32 * a
                            + canvas[dst + c] as f32 * (1.0 - a))
                            as u8;
                    }
                    canvas[dst + 3] = 255;
                }
            }
        }

        // Convert canvas to lightness for ASCII rendering.
        let lightness: Vec<f32> = canvas
            .chunks_exact(4)
            .map(|px| {
                (0.2126 * px[0] as f32 + 0.7152 * px[1] as f32 + 0.0722 * px[2] as f32)
                    / 255.0
            })
            .collect();

        let ascii = lightness_to_ascii(&lightness, canvas_w, canvas_h, cols, contrast, ascii_only);
        result.push((ascii, delay_ms));

        // Apply disposal method for next frame.
        match dispose {
            DisposalMethod::Previous => {
                canvas = saved.unwrap();
            }
            DisposalMethod::Background => {
                // Clear the frame region to white.
                for y in 0..fh {
                    let cy = top + y;
                    if cy >= canvas_h {
                        break;
                    }
                    for x in 0..fw {
                        let cx = left + x;
                        if cx >= canvas_w {
                            break;
                        }
                        let dst = (cy * canvas_w + cx) * 4;
                        canvas[dst..dst + 4].copy_from_slice(&[255, 255, 255, 255]);
                    }
                }
            }
            _ => {} // Keep / Any — leave canvas as-is
        }
    }

    if result.is_empty() {
        return Err("GIF has no frames".to_string());
    }
    Ok(result)
}

/// Render text as large ASCII art.
///
/// Rasterizes `text` using JetBrains Mono at `font_size`, then converts
/// the resulting bitmap to ASCII art with `cols` output width.
pub fn text_to_ascii(text: &str, cols: usize, font_size: f32, contrast: f32, ascii_only: bool) -> String {
    let font_data = include_bytes!("../../ascii_shapes/fonts/JetBrainsMono-Regular.ttf");
    let font = fontdue::Font::from_bytes(
        font_data as &[u8],
        fontdue::FontSettings::default(),
    )
    .expect("failed to load embedded font");

    // Rasterize each character and collect metrics + bitmaps.
    let mut glyphs: Vec<(fontdue::Metrics, Vec<u8>)> = Vec::new();
    for ch in text.chars() {
        glyphs.push(font.rasterize(ch, font_size));
    }

    if glyphs.is_empty() {
        return String::new();
    }

    // Compute the common baseline and total bitmap dimensions.
    // Each glyph has metrics.ymin (distance from bottom of bitmap to baseline).
    let ascent = glyphs
        .iter()
        .map(|(m, _)| m.height as i32 + m.ymin)
        .max()
        .unwrap_or(0);

    let descent = glyphs
        .iter()
        .map(|(m, _)| m.ymin)
        .min()
        .unwrap_or(0);

    let bitmap_h = (ascent - descent).max(1) as usize;

    // Total width: sum of advance_width for each glyph.
    let bitmap_w: usize = glyphs
        .iter()
        .map(|(m, _)| m.advance_width.ceil() as usize)
        .sum();

    if bitmap_w == 0 {
        return String::new();
    }

    // Compose glyphs into a single lightness bitmap.
    // Coverage 0 = no ink = lightness 1.0 (white background).
    // Coverage 255 = full ink = lightness 0.0 (black text).
    let mut lightness = vec![1.0_f32; bitmap_w * bitmap_h];
    let mut cursor_x: usize = 0;

    for (m, bitmap) in &glyphs {
        let advance = m.advance_width.ceil() as usize;
        // y offset: top of this glyph's bitmap in the composed image.
        let glyph_top = ascent - (m.height as i32 + m.ymin);
        let glyph_top = glyph_top.max(0) as usize;

        for gy in 0..m.height {
            let dest_y = glyph_top + gy;
            if dest_y >= bitmap_h {
                break;
            }
            for gx in 0..m.width {
                let dest_x = cursor_x + gx + m.xmin.max(0) as usize;
                if dest_x >= bitmap_w {
                    break;
                }
                let coverage = bitmap[gy * m.width + gx] as f32 / 255.0;
                // Invert: full coverage = dark (low lightness)
                lightness[dest_y * bitmap_w + dest_x] = 1.0 - coverage;
            }
        }
        cursor_x += advance;
    }

    lightness_to_ascii(&lightness, bitmap_w, bitmap_h, cols, contrast, ascii_only)
}

// ---------------------------------------------------------------------------
// Lisp builtins
// ---------------------------------------------------------------------------

/// Parse a keyword argument value as a positive integer from the VM registers.
fn parse_kw_int(vm: &SloshVm, val: &Value, kw_name: &str, fn_name: &str) -> VMResult<usize> {
    match val {
        Value::Int(n) => {
            let n = slvm::from_i56(n);
            if n < 1 {
                Err(VMError::new(
                    "ascii-art",
                    format!("{fn_name}: :{kw_name} must be positive"),
                ))
            } else {
                Ok(n as usize)
            }
        }
        _ => Err(VMError::new(
            "ascii-art",
            format!(
                "{fn_name}: :{kw_name} requires an integer, got {}",
                val.display_type(vm)
            ),
        )),
    }
}

/// Parse a keyword argument value as a float from the VM registers.
fn parse_kw_float(vm: &SloshVm, val: &Value, kw_name: &str, fn_name: &str) -> VMResult<f32> {
    match val {
        Value::Float(f) => Ok(f64::from(*f) as f32),
        Value::Int(n) => Ok(slvm::from_i56(n) as f32),
        _ => Err(VMError::new(
            "ascii-art",
            format!(
                "{fn_name}: :{kw_name} requires a number, got {}",
                val.display_type(vm)
            ),
        )),
    }
}

/// Extract a string from a Value (String or StringConst).
fn val_to_string(vm: &SloshVm, val: &Value, fn_name: &str, arg_desc: &str) -> VMResult<String> {
    match val {
        Value::String(h) => Ok(vm.get_string(*h).to_string()),
        Value::StringConst(i) => Ok(vm.get_interned(*i).to_string()),
        _ => Err(VMError::new(
            "ascii-art",
            format!(
                "{fn_name}: {arg_desc} must be a string, got {}",
                val.display_type(vm)
            ),
        )),
    }
}

/// (lightness->ascii lightness-vec img-w img-h [:cols N] [:contrast F] [:ascii BOOL])
///
/// Convert a flat vector of lightness floats to an ASCII art string.
pub fn builtin_lightness_to_ascii(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let fn_name = "lightness->ascii";
    let mut args = registers.iter();

    // First arg: lightness vector
    let lightness_val = args.next().ok_or_else(|| {
        VMError::new("ascii-art", format!("{fn_name}: requires a lightness vector"))
    })?;
    let lightness: Vec<f32> = match lightness_val {
        Value::Vector(h) => {
            let vec_data = vm.get_vector(*h);
            vec_data
                .iter()
                .map(|v| match v {
                    Value::Float(f) => Ok(f64::from(*f) as f32),
                    Value::Int(n) => Ok(slvm::from_i56(n) as f32),
                    _ => Err(VMError::new(
                        "ascii-art",
                        format!(
                            "{fn_name}: lightness vector elements must be numbers, got {}",
                            v.display_type(vm)
                        ),
                    )),
                })
                .collect::<VMResult<Vec<f32>>>()?
        }
        _ => {
            return Err(VMError::new(
                "ascii-art",
                format!(
                    "{fn_name}: first argument must be a vector, got {}",
                    lightness_val.display_type(vm)
                ),
            ));
        }
    };

    // Second arg: img-w
    let img_w_val = args.next().ok_or_else(|| {
        VMError::new("ascii-art", format!("{fn_name}: requires img-w (integer)"))
    })?;
    let img_w = parse_kw_int(vm, img_w_val, "img-w", fn_name)?;

    // Third arg: img-h
    let img_h_val = args.next().ok_or_else(|| {
        VMError::new("ascii-art", format!("{fn_name}: requires img-h (integer)"))
    })?;
    let img_h = parse_kw_int(vm, img_h_val, "img-h", fn_name)?;

    // Defaults
    let mut cols: usize = 80;
    let mut contrast: f32 = 1.2;
    let mut ascii_only = false;

    // Keyword args
    while let Some(arg) = args.next() {
        if let Value::Keyword(i) = arg {
            match vm.get_interned(*i) {
                "cols" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :cols requires a value"))
                    })?;
                    cols = parse_kw_int(vm, val, "cols", fn_name)?;
                }
                "contrast" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :contrast requires a value"))
                    })?;
                    contrast = parse_kw_float(vm, val, "contrast", fn_name)?;
                }
                "ascii" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :ascii requires a value"))
                    })?;
                    ascii_only = parse_kw_bool(vm, val, "ascii", fn_name)?;
                }
                other => {
                    return Err(VMError::new(
                        "ascii-art",
                        format!("{fn_name}: unknown keyword :{other}"),
                    ));
                }
            }
        } else {
            return Err(VMError::new(
                "ascii-art",
                format!(
                    "{fn_name}: expected keyword argument, got {}",
                    arg.display_type(vm)
                ),
            ));
        }
    }

    let result = lightness_to_ascii(&lightness, img_w, img_h, cols, contrast, ascii_only);
    Ok(vm.alloc_string(result))
}

/// (image->ascii path [:cols N] [:contrast F] [:ascii BOOL])
///
/// Load a PNG/JPG image and convert to ASCII art string.
pub fn builtin_image_to_ascii(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let fn_name = "image->ascii";
    let mut args = registers.iter();

    // First arg: file path
    let path_val = args.next().ok_or_else(|| {
        VMError::new("ascii-art", format!("{fn_name}: requires a file path"))
    })?;
    let path = val_to_string(vm, path_val, fn_name, "path")?;

    // Defaults
    let mut cols: usize = 80;
    let mut contrast: f32 = 1.2;
    let mut ascii_only = false;

    // Keyword args
    while let Some(arg) = args.next() {
        if let Value::Keyword(i) = arg {
            match vm.get_interned(*i) {
                "cols" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :cols requires a value"))
                    })?;
                    cols = parse_kw_int(vm, val, "cols", fn_name)?;
                }
                "contrast" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :contrast requires a value"))
                    })?;
                    contrast = parse_kw_float(vm, val, "contrast", fn_name)?;
                }
                "ascii" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :ascii requires a value"))
                    })?;
                    ascii_only = parse_kw_bool(vm, val, "ascii", fn_name)?;
                }
                other => {
                    return Err(VMError::new(
                        "ascii-art",
                        format!("{fn_name}: unknown keyword :{other}"),
                    ));
                }
            }
        } else {
            return Err(VMError::new(
                "ascii-art",
                format!(
                    "{fn_name}: expected keyword argument, got {}",
                    arg.display_type(vm)
                ),
            ));
        }
    }

    match image_to_ascii(&path, cols, contrast, ascii_only) {
        Ok(result) => Ok(vm.alloc_string(result)),
        Err(e) => Err(VMError::new("ascii-art", format!("{fn_name}: {e}"))),
    }
}

/// (text->ascii text [:cols N] [:size F] [:contrast F] [:ascii BOOL])
///
/// Rasterize text into large ASCII art using JetBrains Mono.
pub fn builtin_text_to_ascii(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let fn_name = "text->ascii";
    let mut args = registers.iter();

    // First arg: text string
    let text_val = args.next().ok_or_else(|| {
        VMError::new("ascii-art", format!("{fn_name}: requires a text string"))
    })?;
    let text = val_to_string(vm, text_val, fn_name, "text")?;

    // Defaults
    let mut cols: usize = 60;
    let mut font_size: f32 = 48.0;
    let mut contrast: f32 = 1.5;
    let mut ascii_only = false;

    // Keyword args
    while let Some(arg) = args.next() {
        if let Value::Keyword(i) = arg {
            match vm.get_interned(*i) {
                "cols" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :cols requires a value"))
                    })?;
                    cols = parse_kw_int(vm, val, "cols", fn_name)?;
                }
                "size" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :size requires a value"))
                    })?;
                    font_size = parse_kw_float(vm, val, "size", fn_name)?;
                }
                "contrast" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :contrast requires a value"))
                    })?;
                    contrast = parse_kw_float(vm, val, "contrast", fn_name)?;
                }
                "ascii" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :ascii requires a value"))
                    })?;
                    ascii_only = parse_kw_bool(vm, val, "ascii", fn_name)?;
                }
                other => {
                    return Err(VMError::new(
                        "ascii-art",
                        format!("{fn_name}: unknown keyword :{other}"),
                    ));
                }
            }
        } else {
            return Err(VMError::new(
                "ascii-art",
                format!(
                    "{fn_name}: expected keyword argument, got {}",
                    arg.display_type(vm)
                ),
            ));
        }
    }

    let result = text_to_ascii(&text, cols, font_size, contrast, ascii_only);
    Ok(vm.alloc_string(result))
}

/// Parse a keyword argument value as a boolean from the VM registers.
fn parse_kw_bool(vm: &SloshVm, val: &Value, kw_name: &str, fn_name: &str) -> VMResult<bool> {
    match val {
        Value::True => Ok(true),
        Value::False | Value::Nil => Ok(false),
        _ => Err(VMError::new(
            "ascii-art",
            format!(
                "{fn_name}: :{kw_name} requires a boolean, got {}",
                val.display_type(vm)
            ),
        )),
    }
}

/// (gif->ascii path panel-name [:cols N] [:contrast F] [:loop BOOL] [:ascii BOOL])
///
/// Decode a GIF file and play its frames as ASCII art in the named panel.
/// Returns immediately; frames are played from a background thread.
/// The animation stops when the panel is closed.
///
/// :cols     - output width in characters (default 80)
/// :contrast - contrast exponent, 1.0 = none (default 1.2)
/// :loop     - loop forever (default #t), set to #f for single play
/// :ascii    - restrict to printable ASCII only (default #f)
pub fn builtin_gif_to_ascii(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let fn_name = "gif->ascii";
    let mut args = registers.iter();

    // First arg: file path
    let path_val = args.next().ok_or_else(|| {
        VMError::new("ascii-art", format!("{fn_name}: requires a file path"))
    })?;
    let path = val_to_string(vm, path_val, fn_name, "path")?;

    // Second arg: panel name
    let panel_val = args.next().ok_or_else(|| {
        VMError::new(
            "ascii-art",
            format!("{fn_name}: requires a panel name"),
        )
    })?;
    let panel_name = val_to_string(vm, panel_val, fn_name, "panel-name")?;

    // Defaults
    let mut cols: usize = 80;
    let mut contrast: f32 = 1.2;
    let mut do_loop = true;
    let mut ascii_only = false;

    // Keyword args
    while let Some(arg) = args.next() {
        if let Value::Keyword(i) = arg {
            match vm.get_interned(*i) {
                "cols" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :cols requires a value"))
                    })?;
                    cols = parse_kw_int(vm, val, "cols", fn_name)?;
                }
                "contrast" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new(
                            "ascii-art",
                            format!("{fn_name}: :contrast requires a value"),
                        )
                    })?;
                    contrast = parse_kw_float(vm, val, "contrast", fn_name)?;
                }
                "loop" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :loop requires a value"))
                    })?;
                    do_loop = parse_kw_bool(vm, val, "loop", fn_name)?;
                }
                "ascii" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new("ascii-art", format!("{fn_name}: :ascii requires a value"))
                    })?;
                    ascii_only = parse_kw_bool(vm, val, "ascii", fn_name)?;
                }
                other => {
                    return Err(VMError::new(
                        "ascii-art",
                        format!("{fn_name}: unknown keyword :{other}"),
                    ));
                }
            }
        } else {
            return Err(VMError::new(
                "ascii-art",
                format!(
                    "{fn_name}: expected keyword argument, got {}",
                    arg.display_type(vm)
                ),
            ));
        }
    }

    // Pre-compute all frames
    let frames = gif_to_ascii_frames(&path, cols, contrast, ascii_only)
        .map_err(|e| VMError::new("ascii-art", format!("{fn_name}: {e}")))?;

    // Spawn background playback thread
    let panel_name_owned = panel_name.clone();
    std::thread::spawn(move || {
        use crate::panel::{DockEdge, PANEL_MANAGER};
        use std::fmt::Write as FmtWrite;
        use std::io::Write as IoWrite;

        loop {
            for (ascii, delay_ms) in &frames {
                {
                    let mgr = PANEL_MANAGER.lock().unwrap();
                    let main_area = mgr.main_area;
                    let panel = match mgr.get_panel(&panel_name_owned) {
                        Some(p) => p,
                        None => return, // panel closed — stop playback
                    };

                    // Compute the content area (panel bounds minus separator).
                    let bounds = panel.bounds;
                    let (area_col, area_row, area_w, area_h) = match panel.edge {
                        DockEdge::Top => (
                            bounds.col,
                            bounds.row,
                            bounds.width,
                            bounds.height.saturating_sub(1),
                        ),
                        DockEdge::Bottom => (
                            bounds.col,
                            bounds.row + 1,
                            bounds.width,
                            bounds.height.saturating_sub(1),
                        ),
                        DockEdge::Left => (
                            bounds.col,
                            bounds.row,
                            bounds.width.saturating_sub(1),
                            bounds.height,
                        ),
                        DockEdge::Right => (
                            bounds.col + 1,
                            bounds.row,
                            bounds.width.saturating_sub(1),
                            bounds.height,
                        ),
                    };
                    let content_w = area_w as usize;
                    let content_h = area_h as usize;

                    // Build the entire frame output as one string so it
                    // hits the terminal in a single write() call.
                    let ascii_lines: Vec<&str> = ascii.lines().collect();
                    let mut buf = String::with_capacity(
                        (content_w + 20) * content_h,
                    );

                    // Begin synchronized output, save cursor, hide it, open scroll region
                    let _ = write!(buf, "\x1B[?2026h\x1B7\x1B[?25l\x1B[r");

                    for row_idx in 0..content_h {
                        // Goto this row
                        let _ = write!(
                            buf,
                            "\x1B[{};{}H",
                            area_row as usize + row_idx,
                            area_col,
                        );
                        // Write the ASCII line (or spaces) padded to
                        // exactly content_w characters.
                        let line = ascii_lines.get(row_idx).copied().unwrap_or("");
                        let mut written = 0;
                        for ch in line.chars().take(content_w) {
                            buf.push(ch);
                            written += 1;
                        }
                        for _ in written..content_w {
                            buf.push(' ');
                        }
                    }

                    // Restore scroll region, cursor, show cursor, end synchronized output
                    let scroll_bottom =
                        main_area.row + main_area.height.saturating_sub(1);
                    let _ = write!(
                        buf,
                        "\x1B[{};{}r\x1B8\x1B[?25h\x1B[?2026l",
                        main_area.row, scroll_bottom,
                    );

                    // Single atomic write + flush
                    let mut out = std::io::stdout().lock();
                    let _ = out.write_all(buf.as_bytes());
                    let _ = out.flush();
                }
                std::thread::sleep(std::time::Duration::from_millis(*delay_ms));
            }
            if !do_loop {
                return;
            }
        }
    });

    Ok(Value::Nil)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lightness_to_ascii_all_white() {
        let w = 80;
        let h = 40;
        let lightness = vec![1.0_f32; w * h];
        let result = lightness_to_ascii(&lightness, w, h, 10, 1.2, false);
        assert!(!result.is_empty());
        // All-white should produce sparse/light characters (not dense ones).
        // With quantization, exact space isn't guaranteed for the all-zero
        // ink bucket, so just verify we don't get dense characters.
        for ch in result.chars() {
            if ch != '\n' {
                assert!(
                    ch != '#' && ch != '@' && ch != 'M',
                    "expected sparse character for all-white, got '{ch}'"
                );
            }
        }
    }

    #[test]
    fn lightness_to_ascii_all_black() {
        let w = 80;
        let h = 40;
        let lightness = vec![0.0_f32; w * h];
        let result = lightness_to_ascii(&lightness, w, h, 10, 1.0, false);
        assert!(!result.is_empty());
        // All-black should produce dense characters (not spaces)
        for ch in result.chars() {
            if ch != '\n' {
                assert_ne!(ch, ' ', "expected non-space for all-black");
            }
        }
    }

    #[test]
    fn lightness_to_ascii_dimensions() {
        let w = 100;
        let h = 80;
        let cols = 10;
        let lightness = vec![0.5_f32; w * h];
        let result = lightness_to_ascii(&lightness, w, h, cols, 1.0, false);
        let lines: Vec<&str> = result.lines().collect();
        // cell_w = 100/10 = 10, cell_h = 20, rows = 80/20 = 4
        assert_eq!(lines.len(), 4);
        for line in &lines {
            assert_eq!(line.chars().count(), cols);
        }
    }

    #[test]
    fn lightness_to_ascii_empty_input() {
        assert_eq!(lightness_to_ascii(&[], 0, 0, 10, 1.0, false), "");
        assert_eq!(lightness_to_ascii(&[0.5], 1, 1, 0, 1.0, false), "");
    }

    #[test]
    fn image_to_ascii_nonexistent_file() {
        let result = image_to_ascii("/nonexistent/path.png", 80, 1.2, false);
        assert!(result.is_err());
    }

    #[test]
    fn text_to_ascii_produces_output() {
        let result = text_to_ascii("Hello", 40, 64.0, 1.5, false);
        assert!(!result.is_empty(), "text->ascii should produce output for 'Hello'");
        let lines: Vec<&str> = result.lines().collect();
        assert!(lines.len() > 1, "text->ascii should produce multiple lines");
    }

    #[test]
    fn text_to_ascii_empty_string() {
        let result = text_to_ascii("", 40, 48.0, 1.5, false);
        assert!(result.is_empty());
    }

    #[test]
    fn gif_to_ascii_frames_nonexistent_file() {
        let result = gif_to_ascii_frames("/nonexistent/path.gif", 40, 1.2, false);
        assert!(result.is_err());
    }
}
