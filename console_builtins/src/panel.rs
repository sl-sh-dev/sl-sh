use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::Mutex;

/// Which edge of the terminal a panel is docked to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DockEdge {
    Top,
    Bottom,
    Left,
    Right,
}

/// A rectangular region in the terminal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rect {
    pub col: u16,
    pub row: u16,
    pub width: u16,
    pub height: u16,
}

/// A docked panel that occupies a strip along one edge of the terminal.
#[derive(Debug, Clone)]
pub struct Panel {
    pub name: String,
    pub edge: DockEdge,
    pub requested_size: u16,
    pub bounds: Rect,
    pub content: Vec<String>,
    pub scroll_offset: usize,
}

impl Panel {
    pub fn new(name: String, edge: DockEdge, requested_size: u16) -> Self {
        Self {
            name,
            edge,
            requested_size,
            bounds: Rect {
                col: 1,
                row: 1,
                width: 0,
                height: 0,
            },
            content: Vec::new(),
            scroll_offset: 0,
        }
    }

    /// Append text to the panel. Splits on newlines and adds each line.
    /// Auto-scrolls so the most recent content is visible.
    pub fn write_text(&mut self, text: &str) {
        for line in text.split('\n') {
            self.content.push(line.to_string());
        }
        // Reset scroll to bottom (show newest content)
        self.scroll_offset = 0;
    }

    /// Clear all content from the panel.
    pub fn clear(&mut self) {
        self.content.clear();
        self.scroll_offset = 0;
    }

    /// Return the content height available (bounds minus separator).
    pub fn content_height(&self) -> u16 {
        self.bounds.height.saturating_sub(1)
    }

    /// Return the content width available (bounds minus separator for left/right).
    pub fn content_width(&self) -> u16 {
        match self.edge {
            DockEdge::Left | DockEdge::Right => self.bounds.width.saturating_sub(1),
            DockEdge::Top | DockEdge::Bottom => self.bounds.width,
        }
    }

    /// Return the visible lines for the current scroll position.
    pub fn visible_lines(&self) -> &[String] {
        let visible_height = self.content_height() as usize;
        let total = self.content.len();
        if total == 0 || visible_height == 0 {
            return &[];
        }

        let end = if self.scroll_offset >= total {
            0
        } else {
            total - self.scroll_offset
        };
        let start = end.saturating_sub(visible_height);
        &self.content[start..end]
    }
}

/// Manages all docked panels and computes the remaining main area.
pub struct PanelManager {
    panels: HashMap<String, Panel>,
    /// Insertion order for deterministic layout.
    order: Vec<String>,
    pub main_area: Rect,
    pub term_cols: u16,
    pub term_rows: u16,
}

impl PanelManager {
    pub fn new() -> Self {
        let (cols, rows) = sl_console::terminal_size().unwrap_or((80, 24));
        Self {
            panels: HashMap::new(),
            order: Vec::new(),
            main_area: Rect {
                col: 1,
                row: 1,
                width: cols,
                height: rows,
            },
            term_cols: cols,
            term_rows: rows,
        }
    }

    /// Recalculate all panel bounds and the main area from terminal dimensions.
    pub fn recalculate_layout(&mut self) {
        let mut remaining = Rect {
            col: 1,
            row: 1,
            width: self.term_cols,
            height: self.term_rows,
        };

        for name in &self.order.clone() {
            if let Some(panel) = self.panels.get_mut(name) {
                // 1 extra row/col for the separator line
                let size_with_sep = panel.requested_size.saturating_add(1);
                match panel.edge {
                    DockEdge::Top => {
                        let h = size_with_sep.min(remaining.height);
                        panel.bounds = Rect {
                            col: remaining.col,
                            row: remaining.row,
                            width: remaining.width,
                            height: h,
                        };
                        remaining.row = remaining.row.saturating_add(h);
                        remaining.height = remaining.height.saturating_sub(h);
                    }
                    DockEdge::Bottom => {
                        let h = size_with_sep.min(remaining.height);
                        let panel_row = remaining
                            .row
                            .saturating_add(remaining.height)
                            .saturating_sub(h);
                        panel.bounds = Rect {
                            col: remaining.col,
                            row: panel_row,
                            width: remaining.width,
                            height: h,
                        };
                        remaining.height = remaining.height.saturating_sub(h);
                    }
                    DockEdge::Left => {
                        let w = size_with_sep.min(remaining.width);
                        panel.bounds = Rect {
                            col: remaining.col,
                            row: remaining.row,
                            width: w,
                            height: remaining.height,
                        };
                        remaining.col = remaining.col.saturating_add(w);
                        remaining.width = remaining.width.saturating_sub(w);
                    }
                    DockEdge::Right => {
                        let w = size_with_sep.min(remaining.width);
                        let panel_col = remaining
                            .col
                            .saturating_add(remaining.width)
                            .saturating_sub(w);
                        panel.bounds = Rect {
                            col: panel_col,
                            row: remaining.row,
                            width: w,
                            height: remaining.height,
                        };
                        remaining.width = remaining.width.saturating_sub(w);
                    }
                }
            }
        }

        self.main_area = remaining;
    }

    /// Open a new panel docked to the given edge.
    pub fn open_panel(&mut self, name: String, edge: DockEdge, size: u16) {
        if self.panels.contains_key(&name) {
            // Update existing panel's parameters.
            if let Some(panel) = self.panels.get_mut(&name) {
                panel.edge = edge;
                panel.requested_size = size;
            }
        } else {
            let panel = Panel::new(name.clone(), edge, size);
            self.panels.insert(name.clone(), panel);
            self.order.push(name);
        }
        self.recalculate_layout();
    }

    /// Close and remove a panel by name.
    /// Returns the old panel's bounds if it existed, None otherwise.
    pub fn close_panel(&mut self, name: &str) -> Option<Rect> {
        if let Some(panel) = self.panels.remove(name) {
            let old_bounds = panel.bounds;
            self.order.retain(|n| n != name);
            self.recalculate_layout();
            Some(old_bounds)
        } else {
            None
        }
    }

    /// Get a reference to a panel by name.
    pub fn get_panel(&self, name: &str) -> Option<&Panel> {
        self.panels.get(name)
    }

    /// Get a mutable reference to a panel by name.
    pub fn get_panel_mut(&mut self, name: &str) -> Option<&mut Panel> {
        self.panels.get_mut(name)
    }

    /// Return the list of panel names in insertion order.
    pub fn panel_names(&self) -> &[String] {
        &self.order
    }

    /// Re-query terminal size and recalculate layout.
    pub fn handle_resize(&mut self) {
        if let Ok((cols, rows)) = sl_console::terminal_size() {
            self.term_cols = cols;
            self.term_rows = rows;
            self.recalculate_layout();
        }
    }

    /// Check if any panels are open.
    pub fn has_panels(&self) -> bool {
        !self.panels.is_empty()
    }

    /// Iterator over panels in insertion order.
    pub fn panels_in_order(&self) -> impl Iterator<Item = &Panel> {
        self.order.iter().filter_map(|n| self.panels.get(n))
    }
}

lazy_static! {
    pub static ref PANEL_MANAGER: Mutex<PanelManager> = Mutex::new(PanelManager::new());
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_manager(cols: u16, rows: u16) -> PanelManager {
        PanelManager {
            panels: HashMap::new(),
            order: Vec::new(),
            main_area: Rect {
                col: 1,
                row: 1,
                width: cols,
                height: rows,
            },
            term_cols: cols,
            term_rows: rows,
        }
    }

    #[test]
    fn test_empty_layout() {
        let mut mgr = make_manager(80, 24);
        mgr.recalculate_layout();
        assert_eq!(
            mgr.main_area,
            Rect {
                col: 1,
                row: 1,
                width: 80,
                height: 24
            }
        );
    }

    #[test]
    fn test_single_bottom_panel() {
        let mut mgr = make_manager(80, 24);
        mgr.open_panel("out".to_string(), DockEdge::Bottom, 5);
        // 5 lines + 1 separator = 6 rows consumed from bottom
        assert_eq!(
            mgr.main_area,
            Rect {
                col: 1,
                row: 1,
                width: 80,
                height: 18
            }
        );
        let panel = mgr.get_panel("out").unwrap();
        assert_eq!(
            panel.bounds,
            Rect {
                col: 1,
                row: 19,
                width: 80,
                height: 6
            }
        );
    }

    #[test]
    fn test_single_top_panel() {
        let mut mgr = make_manager(80, 24);
        mgr.open_panel("header".to_string(), DockEdge::Top, 3);
        // 3 lines + 1 separator = 4 rows consumed from top
        assert_eq!(
            mgr.main_area,
            Rect {
                col: 1,
                row: 5,
                width: 80,
                height: 20
            }
        );
        let panel = mgr.get_panel("header").unwrap();
        assert_eq!(
            panel.bounds,
            Rect {
                col: 1,
                row: 1,
                width: 80,
                height: 4
            }
        );
    }

    #[test]
    fn test_single_right_panel() {
        let mut mgr = make_manager(80, 24);
        mgr.open_panel("side".to_string(), DockEdge::Right, 20);
        // 20 cols + 1 separator = 21 cols consumed from right
        assert_eq!(
            mgr.main_area,
            Rect {
                col: 1,
                row: 1,
                width: 59,
                height: 24
            }
        );
        let panel = mgr.get_panel("side").unwrap();
        assert_eq!(
            panel.bounds,
            Rect {
                col: 60,
                row: 1,
                width: 21,
                height: 24
            }
        );
    }

    #[test]
    fn test_single_left_panel() {
        let mut mgr = make_manager(80, 24);
        mgr.open_panel("nav".to_string(), DockEdge::Left, 15);
        // 15 cols + 1 separator = 16 cols consumed from left
        assert_eq!(
            mgr.main_area,
            Rect {
                col: 17,
                row: 1,
                width: 64,
                height: 24
            }
        );
    }

    #[test]
    fn test_two_panels_bottom_and_right() {
        let mut mgr = make_manager(80, 24);
        mgr.open_panel("out".to_string(), DockEdge::Bottom, 5);
        mgr.open_panel("side".to_string(), DockEdge::Right, 20);
        // Bottom first: 6 rows consumed, main height=18
        // Right second: 21 cols consumed, but only 18 rows high (remaining area)
        assert_eq!(
            mgr.main_area,
            Rect {
                col: 1,
                row: 1,
                width: 59,
                height: 18
            }
        );
        let side = mgr.get_panel("side").unwrap();
        assert_eq!(side.bounds.height, 18);
        assert_eq!(side.bounds.col, 60);
    }

    #[test]
    fn test_close_panel() {
        let mut mgr = make_manager(80, 24);
        mgr.open_panel("out".to_string(), DockEdge::Bottom, 5);
        assert!(mgr.has_panels());
        mgr.close_panel("out");
        assert!(!mgr.has_panels());
        assert_eq!(
            mgr.main_area,
            Rect {
                col: 1,
                row: 1,
                width: 80,
                height: 24
            }
        );
    }

    #[test]
    fn test_panel_names_order() {
        let mut mgr = make_manager(80, 24);
        mgr.open_panel("a".to_string(), DockEdge::Bottom, 3);
        mgr.open_panel("b".to_string(), DockEdge::Top, 3);
        mgr.open_panel("c".to_string(), DockEdge::Right, 10);
        assert_eq!(mgr.panel_names(), &["a", "b", "c"]);
    }

    #[test]
    fn test_panel_too_large_clamped() {
        let mut mgr = make_manager(80, 24);
        // Request more rows than terminal has
        mgr.open_panel("huge".to_string(), DockEdge::Bottom, 100);
        // Should clamp to terminal height
        let panel = mgr.get_panel("huge").unwrap();
        assert_eq!(panel.bounds.height, 24);
        assert_eq!(mgr.main_area.height, 0);
    }

    #[test]
    fn test_write_text() {
        let mut panel = Panel::new("test".to_string(), DockEdge::Bottom, 3);
        panel.bounds = Rect {
            col: 1,
            row: 1,
            width: 80,
            height: 4,
        };
        panel.write_text("line1\nline2\nline3");
        assert_eq!(panel.content, vec!["line1", "line2", "line3"]);
    }

    #[test]
    fn test_write_text_multiple_calls() {
        let mut panel = Panel::new("test".to_string(), DockEdge::Bottom, 3);
        panel.bounds = Rect {
            col: 1,
            row: 1,
            width: 80,
            height: 4,
        };
        panel.write_text("line1");
        panel.write_text("line2");
        assert_eq!(panel.content, vec!["line1", "line2"]);
    }

    #[test]
    fn test_clear() {
        let mut panel = Panel::new("test".to_string(), DockEdge::Bottom, 3);
        panel.bounds = Rect {
            col: 1,
            row: 1,
            width: 80,
            height: 4,
        };
        panel.write_text("line1\nline2");
        panel.clear();
        assert!(panel.content.is_empty());
        assert_eq!(panel.scroll_offset, 0);
    }

    #[test]
    fn test_visible_lines_fits() {
        let mut panel = Panel::new("test".to_string(), DockEdge::Bottom, 5);
        panel.bounds = Rect {
            col: 1,
            row: 1,
            width: 80,
            height: 6, // 5 content + 1 separator
        };
        panel.write_text("a\nb\nc");
        let visible = panel.visible_lines();
        assert_eq!(visible, &["a", "b", "c"]);
    }

    #[test]
    fn test_visible_lines_overflow() {
        let mut panel = Panel::new("test".to_string(), DockEdge::Bottom, 2);
        panel.bounds = Rect {
            col: 1,
            row: 1,
            width: 80,
            height: 3, // 2 content + 1 separator
        };
        panel.write_text("a\nb\nc\nd\ne");
        // Should show the last 2 lines (scroll_offset=0 means bottom)
        let visible = panel.visible_lines();
        assert_eq!(visible, &["d", "e"]);
    }

    #[test]
    fn test_visible_lines_empty() {
        let panel = Panel::new("test".to_string(), DockEdge::Bottom, 5);
        let visible = panel.visible_lines();
        assert!(visible.is_empty());
    }

    #[test]
    fn test_reopen_panel_updates() {
        let mut mgr = make_manager(80, 24);
        mgr.open_panel("out".to_string(), DockEdge::Bottom, 5);
        mgr.open_panel("out".to_string(), DockEdge::Top, 3);
        // Should update in place, not duplicate
        assert_eq!(mgr.panel_names().len(), 1);
        let panel = mgr.get_panel("out").unwrap();
        assert_eq!(panel.edge, DockEdge::Top);
        assert_eq!(panel.requested_size, 3);
    }
}
