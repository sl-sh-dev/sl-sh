//! Shape-aware ASCII character matching.
//!
//! Instead of treating ASCII characters as pixels (mapping lightness to a
//! density-sorted character list), this crate captures the *shape* of each
//! character using a 6-dimensional vector derived from overlapping sampling
//! circles placed within the character cell.
//!
//! The 6 internal sampling circles are arranged in a staggered 3x2 grid:
//!   - Left column (slightly lowered): upper-left, middle-left, lower-left
//!   - Right column (slightly raised): upper-right, middle-right, lower-right
//!
//! Each component measures how much of the character's ink overlaps that
//! circle, yielding a value in [0.0, 1.0].
//!
//! Additionally, 10 *external* sampling circles are placed outside the cell
//! boundary (reaching into neighboring cells) to enable **directional
//! contrast enhancement**.  This prevents "staircasing" artifacts at
//! lightness boundaries by allowing edges to propagate across cell borders.
//!
//! Based on: "ASCII characters are not pixels" by Alex Harri (2026).

mod generated;

/// A 6-dimensional shape vector representing the visual density distribution
/// of a character across 6 overlapping sampling regions.
///
/// Index legend (staggered 3x2 spatial layout):
///   0: upper-left    1: upper-right
///   2: middle-left   3: middle-right
///   4: lower-left    5: lower-right
pub type ShapeVector = [f32; 6];

/// A 10-dimensional external sampling vector.
///
/// These 10 circles are placed *outside* the cell boundary, reaching into
/// neighboring cells.  They are used for directional contrast enhancement
/// to prevent "staircasing" at lightness boundaries.
///
/// Index legend (spatial positions around the cell boundary):
///   0: top-left      1: top-right
///   2: left-upper    3: right-upper
///   4: left-middle   5: right-middle
///   6: left-lower    7: right-lower
///   8: bottom-left   9: bottom-right
pub type ExternalVector = [f32; 10];

/// For each internal sampling circle (0-5), the indices of the external
/// circles that affect it during directional contrast enhancement.
///
/// When an external circle is "light" (high value), it indicates a boundary
/// in that direction, so the corresponding internal components get darkened
/// via contrast enhancement.  The "widened" mapping means external circles
/// affect not just their immediate neighbor but also adjacent internal
/// circles, which prevents the staircasing effect.
pub const AFFECTING_EXTERNAL_INDICES: [&[usize]; 6] = [
    &[0, 1, 2, 4], // internal 0 (upper-left)
    &[0, 1, 3, 5], // internal 1 (upper-right)
    &[2, 4, 6],    // internal 2 (middle-left)
    &[3, 5, 7],    // internal 3 (middle-right)
    &[4, 6, 8, 9], // internal 4 (lower-left)
    &[5, 7, 8, 9], // internal 5 (lower-right)
];

/// A character and its pre-computed shape vector.
#[derive(Debug, Clone, Copy)]
pub struct CharShape {
    pub ch: char,
    pub vector: ShapeVector,
}

/// Returns the pre-computed shape vector for a given ASCII character,
/// or `None` if the character is not in the table.
pub fn shape_vector(ch: char) -> Option<&'static ShapeVector> {
    generated::SHAPE_VECTORS.get(&ch)
}

/// Find the ASCII character whose shape vector best matches the given
/// sampling vector, using squared Euclidean distance.
///
/// The `contrast` parameter (>= 1.0) enhances edges by raising normalized
/// sampling vector components to this power before matching.  A value of
/// 1.0 means no contrast enhancement.
pub fn best_char(sampling: &ShapeVector, contrast: f32) -> char {
    let adjusted = if contrast > 1.0 {
        apply_contrast(sampling, contrast)
    } else {
        *sampling
    };

    let mut best_ch = ' ';
    let mut best_dist = f32::MAX;

    for (ch, shape) in generated::SHAPE_VECTORS.entries() {
        let dist = squared_distance(&adjusted, shape);
        if dist < best_dist {
            best_dist = dist;
            best_ch = *ch;
        }
    }

    best_ch
}

/// Find the best matching ASCII character using both global and directional
/// contrast enhancement.
///
/// This is the full algorithm from the article.  The `external` vector
/// contains 10 samples from neighboring cells.  Both global and directional
/// contrast are applied with the given `exponent` before matching.
pub fn best_char_directional(
    sampling: &ShapeVector,
    external: &ExternalVector,
    exponent: f32,
) -> char {
    let adjusted = if exponent > 1.0 {
        let global = apply_contrast(sampling, exponent);
        apply_directional_contrast(&global, external, exponent)
    } else {
        *sampling
    };

    let mut best_ch = ' ';
    let mut best_dist = f32::MAX;

    for (ch, shape) in generated::SHAPE_VECTORS.entries() {
        let dist = squared_distance(&adjusted, shape);
        if dist < best_dist {
            best_dist = dist;
            best_ch = *ch;
        }
    }

    best_ch
}

/// Squared Euclidean distance between two shape vectors.
/// We skip the sqrt since we only need relative ordering.
#[inline]
fn squared_distance(a: &ShapeVector, b: &ShapeVector) -> f32 {
    let mut sum = 0.0_f32;
    for i in 0..6 {
        let d = a[i] - b[i];
        sum += d * d;
    }
    sum
}

/// Apply global contrast enhancement to a sampling vector.
///
/// Normalizes the vector to [0, 1] relative to its own max component,
/// raises each component to the given exponent, then denormalizes back.
/// This makes darker regions darker while preserving the brightest region,
/// enhancing edges between different lightness zones.
pub fn apply_contrast(sampling: &ShapeVector, exponent: f32) -> ShapeVector {
    let max_val = sampling.iter().copied().fold(0.0_f32, f32::max);
    if max_val < f32::EPSILON {
        return *sampling;
    }

    let mut result = [0.0_f32; 6];
    for i in 0..6 {
        let normalized = sampling[i] / max_val;
        result[i] = normalized.powf(exponent) * max_val;
    }
    result
}

/// Apply directional contrast enhancement using external sampling circles.
///
/// For each internal component, the `maxValue` used for normalization is
/// the maximum of the component's own value and the values of all external
/// circles that affect it (per [`AFFECTING_EXTERNAL_INDICES`]).
///
/// This allows lightness boundaries in neighboring cells to "reach into"
/// the current cell's sampling vector, preventing the staircasing effect
/// that occurs with global contrast enhancement alone.
pub fn apply_directional_contrast(
    sampling: &ShapeVector,
    external: &ExternalVector,
    exponent: f32,
) -> ShapeVector {
    let mut result = [0.0_f32; 6];
    for i in 0..6 {
        let value = sampling[i];

        // Find max across the affecting external circles for this component.
        let mut max_val = value;
        for &ext_idx in AFFECTING_EXTERNAL_INDICES[i] {
            max_val = max_val.max(external[ext_idx]);
        }

        if max_val < f32::EPSILON {
            result[i] = value;
        } else {
            let normalized = value / max_val;
            result[i] = normalized.powf(exponent) * max_val;
        }
    }
    result
}

/// Compute a sampling vector from a grid of lightness values.
///
/// `lightness` is a row-major grid of values in [0.0, 1.0], with dimensions
/// `width` x `height`.  The 6 sampling circles are placed within this grid
/// and the average lightness under each circle is returned.
///
/// This is the function you call on each cell of your image grid to produce
/// the sampling vector that gets passed to `best_char`.
pub fn sample_cell(lightness: &[f32], width: usize, height: usize) -> ShapeVector {
    let w = width as f32;
    let h = height as f32;

    // Sampling circle centers (staggered 3x2 layout).
    // Left column is slightly lowered, right column slightly raised.
    let radius_x = w * 0.28;
    let radius_y = h * 0.20;

    let circles: [(f32, f32); 6] = [
        // Upper-left, Upper-right
        (w * 0.30, h * 0.20),
        (w * 0.70, h * 0.13),
        // Middle-left, Middle-right
        (w * 0.30, h * 0.53),
        (w * 0.70, h * 0.47),
        // Lower-left, Lower-right
        (w * 0.30, h * 0.87),
        (w * 0.70, h * 0.80),
    ];

    let mut result = [0.0_f32; 6];
    for (i, &(cx, cy)) in circles.iter().enumerate() {
        let mut sum = 0.0_f32;
        let mut count = 0u32;

        let y_start = ((cy - radius_y).max(0.0)) as usize;
        let y_end = ((cy + radius_y).min(h - 1.0)) as usize;
        let x_start = ((cx - radius_x).max(0.0)) as usize;
        let x_end = ((cx + radius_x).min(w - 1.0)) as usize;

        for y in y_start..=y_end {
            for x in x_start..=x_end {
                let dx = (x as f32 - cx) / radius_x;
                let dy = (y as f32 - cy) / radius_y;
                if dx * dx + dy * dy <= 1.0 {
                    sum += lightness[y * width + x];
                    count += 1;
                }
            }
        }

        result[i] = if count > 0 { sum / count as f32 } else { 0.0 };
    }

    result
}

/// Compute the external sampling vector for directional contrast enhancement.
///
/// `lightness` is the full image grid (row-major, `img_width` x `img_height`).
/// `cell_x`, `cell_y` are the pixel coordinates of the top-left corner of
/// this cell in the image.  `cell_w`, `cell_h` are the cell dimensions.
///
/// The 10 external circles are placed just outside the cell boundary,
/// mirroring the 6 internal circles but offset outward:
///   0: above-left    1: above-right
///   2: left-upper    3: right-upper
///   4: left-middle   5: right-middle
///   6: left-lower    7: right-lower
///   8: below-left    9: below-right
pub fn sample_cell_external(
    lightness: &[f32],
    img_width: usize,
    img_height: usize,
    cell_x: usize,
    cell_y: usize,
    cell_w: usize,
    cell_h: usize,
) -> ExternalVector {
    let w = cell_w as f32;
    let h = cell_h as f32;

    let radius_x = w * 0.28;
    let radius_y = h * 0.20;

    // External circle centers, placed outside the cell boundary.
    // Positions mirror the internal staggered layout but shifted outward.
    let cx = cell_x as f32;
    let cy = cell_y as f32;

    let external_circles: [(f32, f32); 10] = [
        // Above: mirrors upper-left and upper-right y positions
        (cx + w * 0.30, cy - h * 0.13),  // 0: above-left
        (cx + w * 0.70, cy - h * 0.20),  // 1: above-right
        // Left: mirrors the 3 left-column y positions
        (cx - w * 0.20, cy + h * 0.20),  // 2: left-upper
        // Right: mirrors the 3 right-column y positions
        (cx + w + w * 0.20, cy + h * 0.13), // 3: right-upper
        (cx - w * 0.20, cy + h * 0.53),  // 4: left-middle
        (cx + w + w * 0.20, cy + h * 0.47), // 5: right-middle
        (cx - w * 0.20, cy + h * 0.87),  // 6: left-lower
        (cx + w + w * 0.20, cy + h * 0.80), // 7: right-lower
        // Below: mirrors lower-left and lower-right y positions
        (cx + w * 0.30, cy + h + h * 0.13), // 8: below-left
        (cx + w * 0.70, cy + h + h * 0.20), // 9: below-right
    ];

    let mut result = [0.0_f32; 10];
    for (i, &(ecx, ecy)) in external_circles.iter().enumerate() {
        let mut sum = 0.0_f32;
        let mut count = 0u32;

        let y_start = ((ecy - radius_y).max(0.0)) as usize;
        let y_end = ((ecy + radius_y).min(img_height as f32 - 1.0)) as usize;
        let x_start = ((ecx - radius_x).max(0.0)) as usize;
        let x_end = ((ecx + radius_x).min(img_width as f32 - 1.0)) as usize;

        for y in y_start..=y_end {
            for x in x_start..=x_end {
                let dx = (x as f32 - ecx) / radius_x;
                let dy = (y as f32 - ecy) / radius_y;
                if dx * dx + dy * dy <= 1.0 {
                    sum += lightness[y * img_width + x];
                    count += 1;
                }
            }
        }

        result[i] = if count > 0 { sum / count as f32 } else { 0.0 };
    }

    result
}

/// Convert an RGB pixel to relative luminance (lightness) in [0.0, 1.0].
#[inline]
pub fn rgb_to_lightness(r: u8, g: u8, b: u8) -> f32 {
    (r as f32 * 0.2126 + g as f32 * 0.7152 + b as f32 * 0.0722) / 255.0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn space_is_all_zeros() {
        if let Some(v) = shape_vector(' ') {
            for &component in v {
                assert!(component < 0.05, "space should have near-zero shape: {:?}", v);
            }
        }
    }

    #[test]
    fn best_char_returns_space_for_empty() {
        let empty = [0.0_f32; 6];
        let ch = best_char(&empty, 1.0);
        assert_eq!(ch, ' ', "all-zero sampling should match space");
    }

    #[test]
    fn best_char_returns_dense_for_full() {
        let full = [1.0_f32; 6];
        let ch = best_char(&full, 1.0);
        // Should be a dense character like @ or M or #
        assert!(
            ['@', 'M', '#', 'W', '$', '%', 'B', '&'].contains(&ch),
            "all-1.0 sampling should match a dense character, got '{}'",
            ch
        );
    }

    #[test]
    fn contrast_preserves_uniform_vectors() {
        let uniform = [0.5_f32; 6];
        let contrasted = apply_contrast(&uniform, 3.0);
        // All components equal, so after normalize->pow->denormalize they stay equal
        for i in 0..6 {
            assert!(
                (contrasted[i] - uniform[i]).abs() < 0.01,
                "uniform vector should be mostly unchanged by contrast"
            );
        }
    }

    #[test]
    fn contrast_increases_separation() {
        let mixed = [0.8, 0.8, 0.3, 0.3, 0.1, 0.1];
        let contrasted = apply_contrast(&mixed, 3.0);
        // The bright components should stay near original, dark ones should get darker
        assert!(contrasted[0] > contrasted[4], "contrast should preserve ordering");
        let original_ratio = mixed[4] / mixed[0];
        let new_ratio = contrasted[4] / contrasted[0];
        assert!(
            new_ratio < original_ratio,
            "contrast should increase separation: {} vs {}",
            new_ratio,
            original_ratio
        );
    }

    #[test]
    fn rgb_to_lightness_bounds() {
        assert!((rgb_to_lightness(0, 0, 0) - 0.0).abs() < f32::EPSILON);
        assert!((rgb_to_lightness(255, 255, 255) - 1.0).abs() < 0.001);
    }

    #[test]
    fn directional_contrast_darkens_with_light_neighbors() {
        // Internal sampling: uniform 0.2 (dark region)
        let sampling = [0.2_f32; 6];
        // External: upper circles are light (indicating a boundary above)
        let mut external = [0.2_f32; 10];
        external[0] = 0.8; // above-left is light
        external[1] = 0.8; // above-right is light

        let result = apply_directional_contrast(&sampling, &external, 3.0);

        // Upper internal components (0, 1) should be darkened because
        // their affecting externals include 0 and 1 which are light.
        assert!(
            result[0] < sampling[0],
            "upper-left should be darkened: {} vs {}",
            result[0],
            sampling[0]
        );
        assert!(
            result[1] < sampling[1],
            "upper-right should be darkened: {} vs {}",
            result[1],
            sampling[1]
        );

        // Lower components (4, 5) should be less affected since their
        // affecting externals don't include 0 or 1.
        assert!(
            result[4] >= result[0],
            "lower should be less darkened than upper"
        );
    }

    #[test]
    fn directional_contrast_no_effect_on_uniform() {
        // When internal and external are all the same value,
        // directional contrast should have no additional effect.
        let sampling = [0.5_f32; 6];
        let external = [0.5_f32; 10];

        let result = apply_directional_contrast(&sampling, &external, 3.0);
        for i in 0..6 {
            assert!(
                (result[i] - sampling[i]).abs() < 0.01,
                "uniform sampling+external should be unchanged: component {} = {} vs {}",
                i,
                result[i],
                sampling[i]
            );
        }
    }

    #[test]
    fn best_char_directional_matches_basic() {
        let empty = [0.0_f32; 6];
        let no_external = [0.0_f32; 10];
        let ch = best_char_directional(&empty, &no_external, 1.0);
        assert_eq!(ch, ' ', "all-zero with no external should match space");
    }
}
