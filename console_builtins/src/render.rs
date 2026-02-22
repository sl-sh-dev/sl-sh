use std::io::Write;

use crate::panel::{DockEdge, Panel, PanelManager, Rect};

/// Set the terminal scroll region (DECSTBM).
/// `top` and `bottom` are 1-based row numbers.
pub fn set_scroll_region(top: u16, bottom: u16) {
    let mut out = std::io::stdout().lock();
    let _ = write!(out, "\x1B[{};{}r", top, bottom);
    let _ = out.flush();
}

/// Reset the scroll region to the full terminal.
pub fn reset_scroll_region() {
    let mut out = std::io::stdout().lock();
    let _ = write!(out, "\x1B[r");
    let _ = out.flush();
}

/// Draw a horizontal separator line for a top/bottom panel.
fn draw_horizontal_separator(col: u16, row: u16, width: u16) {
    let mut out = std::io::stdout().lock();
    let _ = write!(out, "{}", sl_console::cursor::Goto(col, row));
    for _ in 0..width {
        let _ = write!(out, "\u{2500}");
    }
    let _ = out.flush();
}

/// Draw a vertical separator line for a left/right panel.
fn draw_vertical_separator(col: u16, start_row: u16, height: u16) {
    let mut out = std::io::stdout().lock();
    for r in 0..height {
        let _ = write!(
            out,
            "{}\u{2502}",
            sl_console::cursor::Goto(col, start_row + r)
        );
    }
    let _ = out.flush();
}

/// Render a single panel's separator and content.
pub fn render_panel(panel: &Panel) {
    let bounds = panel.bounds;
    if bounds.width == 0 || bounds.height == 0 {
        return;
    }

    let mut out = std::io::stdout().lock();

    match panel.edge {
        DockEdge::Top => {
            // Separator at the bottom of the panel bounds
            let sep_row = bounds.row + bounds.height - 1;
            draw_horizontal_separator(bounds.col, sep_row, bounds.width);
            // Content area is above the separator
            render_content(&mut out, panel, Rect {
                col: bounds.col,
                row: bounds.row,
                width: bounds.width,
                height: bounds.height.saturating_sub(1),
            });
        }
        DockEdge::Bottom => {
            // Separator at the top of the panel bounds
            draw_horizontal_separator(bounds.col, bounds.row, bounds.width);
            // Content area is below the separator
            render_content(&mut out, panel, Rect {
                col: bounds.col,
                row: bounds.row + 1,
                width: bounds.width,
                height: bounds.height.saturating_sub(1),
            });
        }
        DockEdge::Left => {
            // Separator at the right edge
            let sep_col = bounds.col + bounds.width - 1;
            draw_vertical_separator(sep_col, bounds.row, bounds.height);
            // Content area is to the left of the separator
            render_content(&mut out, panel, Rect {
                col: bounds.col,
                row: bounds.row,
                width: bounds.width.saturating_sub(1),
                height: bounds.height,
            });
        }
        DockEdge::Right => {
            // Separator at the left edge
            draw_vertical_separator(bounds.col, bounds.row, bounds.height);
            // Content area is to the right of the separator
            render_content(&mut out, panel, Rect {
                col: bounds.col + 1,
                row: bounds.row,
                width: bounds.width.saturating_sub(1),
                height: bounds.height,
            });
        }
    }

    let _ = out.flush();
}

/// Render content lines into a specific rectangle, respecting scroll offset.
fn render_content(out: &mut impl Write, panel: &Panel, area: Rect) {
    let visible_height = area.height as usize;
    let content_width = area.width as usize;

    // Compute which lines to show based on scroll_offset
    let total = panel.content.len();
    let start = if total > visible_height + panel.scroll_offset {
        total - visible_height - panel.scroll_offset
    } else if panel.scroll_offset >= total {
        0
    } else {
        0
    };
    let end = (start + visible_height).min(total);

    let lines = &panel.content[start..end];

    for (i, line) in lines.iter().enumerate() {
        let row = area.row + i as u16;
        let _ = write!(out, "{}", sl_console::cursor::Goto(area.col, row));
        // Truncate line to panel width
        let display: String = line.chars().take(content_width).collect();
        let _ = write!(out, "{}", display);
        // Clear remainder of line within the panel
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

/// Render all panels and set the scroll region to the main area.
pub fn render_all_panels(mgr: &PanelManager) {
    if !mgr.has_panels() {
        reset_scroll_region();
        return;
    }

    // Save cursor position
    let mut out = std::io::stdout().lock();
    let _ = write!(out, "\x1B7"); // save cursor (DECSC)
    let _ = out.flush();
    drop(out);

    for panel in mgr.panels_in_order() {
        render_panel(panel);
    }

    // Set scroll region to main area
    let main = mgr.main_area;
    set_scroll_region(main.row, main.row + main.height.saturating_sub(1));

    // Restore cursor position
    let mut out = std::io::stdout().lock();
    let _ = write!(out, "\x1B8"); // restore cursor (DECRC)
    let _ = out.flush();
}
