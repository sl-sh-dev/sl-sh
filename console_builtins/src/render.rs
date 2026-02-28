use std::io::Write;

use crate::panel::{DockEdge, Panel, PanelManager, Rect};

/// Reset the scroll region to the full terminal.
pub fn reset_scroll_region() {
    let mut out = std::io::stdout().lock();
    let _ = write!(out, "\x1B[r");
    let _ = out.flush();
}

/// Draw a horizontal separator line.
fn draw_horizontal_separator(out: &mut impl Write, col: u16, row: u16, width: u16) {
    let _ = write!(out, "{}", sl_console::cursor::Goto(col, row));
    for _ in 0..width {
        let _ = write!(out, "\u{2500}");
    }
}

/// Draw a vertical separator line.
fn draw_vertical_separator(out: &mut impl Write, col: u16, start_row: u16, height: u16) {
    for r in 0..height {
        let _ = write!(
            out,
            "{}\u{2502}",
            sl_console::cursor::Goto(col, start_row + r)
        );
    }
}

/// Render a single panel's separator and content.
fn render_panel(out: &mut impl Write, panel: &Panel) {
    let bounds = panel.bounds;
    if bounds.width == 0 || bounds.height == 0 {
        return;
    }

    match panel.edge {
        DockEdge::Top => {
            let sep_row = bounds.row + bounds.height - 1;
            draw_horizontal_separator(out, bounds.col, sep_row, bounds.width);
            render_content(out, panel, Rect {
                col: bounds.col,
                row: bounds.row,
                width: bounds.width,
                height: bounds.height.saturating_sub(1),
            });
        }
        DockEdge::Bottom => {
            draw_horizontal_separator(out, bounds.col, bounds.row, bounds.width);
            render_content(out, panel, Rect {
                col: bounds.col,
                row: bounds.row + 1,
                width: bounds.width,
                height: bounds.height.saturating_sub(1),
            });
        }
        DockEdge::Left => {
            let sep_col = bounds.col + bounds.width - 1;
            draw_vertical_separator(out, sep_col, bounds.row, bounds.height);
            render_content(out, panel, Rect {
                col: bounds.col,
                row: bounds.row,
                width: bounds.width.saturating_sub(1),
                height: bounds.height,
            });
        }
        DockEdge::Right => {
            draw_vertical_separator(out, bounds.col, bounds.row, bounds.height);
            render_content(out, panel, Rect {
                col: bounds.col + 1,
                row: bounds.row,
                width: bounds.width.saturating_sub(1),
                height: bounds.height,
            });
        }
    }
}

/// Render content lines into a specific rectangle, respecting scroll offset.
fn render_content(out: &mut impl Write, panel: &Panel, area: Rect) {
    let visible_height = area.height as usize;
    let content_width = area.width as usize;

    let total = panel.content.len();
    let start = if total > visible_height + panel.scroll_offset {
        total - visible_height - panel.scroll_offset
    } else {
        0
    };
    let end = (start + visible_height).min(total);

    let lines = &panel.content[start..end];

    for (i, line) in lines.iter().enumerate() {
        let row = area.row + i as u16;
        let _ = write!(out, "{}", sl_console::cursor::Goto(area.col, row));
        let display: String = line.chars().take(content_width).collect();
        let _ = write!(out, "{}", display);
        let displayed = display.chars().count();
        if displayed < content_width {
            for _ in 0..(content_width - displayed) {
                let _ = write!(out, " ");
            }
        }
    }

    // Clear any remaining empty rows
    for i in lines.len()..visible_height {
        let row = area.row + i as u16;
        let _ = write!(out, "{}", sl_console::cursor::Goto(area.col, row));
        for _ in 0..content_width {
            let _ = write!(out, " ");
        }
    }
}

/// Clear a rectangular area by writing spaces.
fn clear_rect(out: &mut impl Write, area: Rect) {
    for r in 0..area.height {
        let _ = write!(out, "{}", sl_console::cursor::Goto(area.col, area.row + r));
        for _ in 0..area.width {
            let _ = write!(out, " ");
        }
    }
}

/// Render all panels and set the scroll region to the main area.
/// Uses a single stdout lock for the entire operation and positions
/// the cursor inside the main area when done.
///
/// `closed_bounds` is an optional list of panel bounds that were just closed
/// and need to be cleared from the screen.
pub fn render_all_panels_with_cleanup(mgr: &PanelManager, closed_bounds: &[Rect]) {
    let mut out = std::io::stdout().lock();

    // Reset scroll region so Goto works across the full terminal
    let _ = write!(out, "\x1B[r");

    // Hide cursor during rendering
    let _ = write!(out, "{}", sl_console::cursor::Hide);

    // Clear areas where closed panels used to be
    for bounds in closed_bounds {
        clear_rect(&mut out, *bounds);
    }

    if mgr.has_panels() {
        for panel in mgr.panels_in_order() {
            render_panel(&mut out, panel);
        }

        // Set scroll region to main area
        let main = mgr.main_area;
        let scroll_bottom = main.row + main.height.saturating_sub(1);
        let _ = write!(out, "\x1B[{};{}r", main.row, scroll_bottom);

        // Move cursor to bottom of main area
        let _ = write!(out, "{}", sl_console::cursor::Goto(1, scroll_bottom));
    }
    // If no panels remain, scroll region stays reset to full terminal.
    // Cursor stays wherever it was (we don't move it).

    // Show cursor
    let _ = write!(out, "{}", sl_console::cursor::Show);

    let _ = out.flush();
}

/// Convenience wrapper for rendering when no panels were just closed.
pub fn render_all_panels(mgr: &PanelManager) {
    render_all_panels_with_cleanup(mgr, &[]);
}
