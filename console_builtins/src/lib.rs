use bridge_adapters::add_builtin;
use bridge_macros::sl_sh_fn;
use compile_state::state::SloshVm;
use slvm::{VMError, VMResult, Value};

pub mod panel;
pub mod render;

/// Usage: (terminal-size)
///
/// Returns a vector #(cols rows) with the current terminal dimensions.
/// Returns nil if the terminal size cannot be determined.
///
/// Section: terminal
///
/// Example:
/// (terminal-size)
#[sl_sh_fn(fn_name = "terminal-size", takes_env = true)]
fn terminal_size(environment: &mut SloshVm) -> VMResult<Value> {
    match sl_console::terminal_size() {
        Ok((cols, rows)) => {
            let v = vec![Value::from(cols as i64), Value::from(rows as i64)];
            Ok(environment.alloc_vector(v))
        }
        Err(_) => Ok(Value::Nil),
    }
}

/// Manual builtin for open-panel with keyword arg parsing.
/// Usage: (open-panel "name" :edge :height N)
/// where :edge is one of :top, :bottom, :left, :right
/// and :height/:width N is the panel size in rows/columns.
fn open_panel(vm: &mut SloshVm, registers: &[Value]) -> VMResult<Value> {
    let mut args = registers.iter();

    // First arg: panel name (string)
    let name = match args.next() {
        Some(Value::String(h)) => vm.get_string(*h).to_string(),
        Some(Value::StringConst(i)) => vm.get_interned(*i).to_string(),
        _ => {
            return Err(VMError::new(
                "console",
                "open-panel: first argument must be a panel name (string)".to_string(),
            ));
        }
    };

    let mut edge = None;
    let mut size: Option<u16> = None;

    // Parse remaining keyword args
    while let Some(arg) = args.next() {
        if let Value::Keyword(i) = arg {
            match vm.get_interned(*i) {
                "top" => edge = Some(panel::DockEdge::Top),
                "bottom" => edge = Some(panel::DockEdge::Bottom),
                "left" => edge = Some(panel::DockEdge::Left),
                "right" => edge = Some(panel::DockEdge::Right),
                "height" | "width" | "size" => {
                    let val = args.next().ok_or_else(|| {
                        VMError::new(
                            "console",
                            format!("open-panel: :{} requires a numeric value", vm.get_interned(*i)),
                        )
                    })?;
                    size = Some(match val {
                        Value::Int(n) => {
                            let n = slvm::from_i56(n);
                            if n < 1 {
                                return Err(VMError::new(
                                    "console",
                                    "open-panel: size must be positive".to_string(),
                                ));
                            }
                            n as u16
                        }
                        _ => {
                            return Err(VMError::new(
                                "console",
                                "open-panel: size must be an integer".to_string(),
                            ));
                        }
                    });
                }
                other => {
                    return Err(VMError::new(
                        "console",
                        format!(
                            "open-panel: unknown keyword :{other}, expected :top, :bottom, :left, :right, :height, :width, or :size"
                        ),
                    ));
                }
            }
        } else {
            return Err(VMError::new(
                "console",
                format!("open-panel: expected keyword argument, got {}", arg.display_type(vm)),
            ));
        }
    }

    let edge = edge.ok_or_else(|| {
        VMError::new(
            "console",
            "open-panel: must specify an edge (:top, :bottom, :left, or :right)".to_string(),
        )
    })?;
    let size = size.unwrap_or(10);

    let mut mgr = panel::PANEL_MANAGER.lock().unwrap();
    mgr.handle_resize();
    mgr.open_panel(name, edge, size);
    render::render_all_panels(&mgr);
    Ok(Value::Nil)
}

/// Usage: (close-panel name)
///
/// Close and remove a docked panel by name. Returns #t if the panel existed,
/// nil otherwise.
///
/// Section: terminal
///
/// Example:
/// (close-panel "out")
#[sl_sh_fn(fn_name = "close-panel", takes_env = true)]
fn close_panel_builtin(environment: &mut SloshVm, name: String) -> VMResult<Value> {
    let _ = environment;
    let mut mgr = panel::PANEL_MANAGER.lock().unwrap();
    if let Some(old_bounds) = mgr.close_panel(&name) {
        render::render_all_panels_with_cleanup(&mgr, &[old_bounds]);
        Ok(Value::True)
    } else {
        Ok(Value::Nil)
    }
}

/// Usage: (panel-list)
///
/// Returns a vector of panel names in the order they were opened.
///
/// Section: terminal
///
/// Example:
/// (panel-list)
#[sl_sh_fn(fn_name = "panel-list", takes_env = true)]
fn panel_list(environment: &mut SloshVm) -> VMResult<Value> {
    let mgr = panel::PANEL_MANAGER.lock().unwrap();
    let names: Vec<Value> = mgr
        .panel_names()
        .iter()
        .map(|n| environment.alloc_string(n.clone()))
        .collect();
    Ok(environment.alloc_vector(names))
}

/// Usage: (panel-write name text)
///
/// Append text to a panel's content buffer and re-render. Lines are split on
/// newlines. The panel auto-scrolls to show the most recent content.
/// Returns nil. Errors if the panel does not exist.
///
/// Section: terminal
///
/// Example:
/// (panel-write "out" "hello world")
#[sl_sh_fn(fn_name = "panel-write")]
fn panel_write(name: String, text: String) -> VMResult<Value> {
    let mut mgr = panel::PANEL_MANAGER.lock().unwrap();
    if mgr.get_panel(&name).is_none() {
        return Err(VMError::new(
            "console",
            format!("panel-write: no panel named \"{name}\""),
        ));
    }
    mgr.get_panel_mut(&name).unwrap().write_text(&text);
    render::render_all_panels(&mgr);
    Ok(Value::Nil)
}

/// Usage: (panel-clear name)
///
/// Clear all content from a panel and re-render.
/// Returns nil. Errors if the panel does not exist.
///
/// Section: terminal
///
/// Example:
/// (panel-clear "out")
#[sl_sh_fn(fn_name = "panel-clear")]
fn panel_clear(name: String) -> VMResult<Value> {
    let mut mgr = panel::PANEL_MANAGER.lock().unwrap();
    if mgr.get_panel(&name).is_none() {
        return Err(VMError::new(
            "console",
            format!("panel-clear: no panel named \"{name}\""),
        ));
    }
    mgr.get_panel_mut(&name).unwrap().clear();
    render::render_all_panels(&mgr);
    Ok(Value::Nil)
}

/// Usage: (panel-resize)
///
/// Re-query the terminal size and recalculate all panel layouts. Call this
/// after the terminal has been resized. Re-renders all panels.
///
/// Section: terminal
///
/// Example:
/// (panel-resize)
#[sl_sh_fn(fn_name = "panel-resize")]
fn panel_resize() -> VMResult<Value> {
    let mut mgr = panel::PANEL_MANAGER.lock().unwrap();
    mgr.handle_resize();
    render::render_all_panels(&mgr);
    Ok(Value::Nil)
}

/// Usage: (panel-close-after name millis)
///
/// Schedule a panel to close after the given number of milliseconds.
/// Returns immediately without blocking. The panel will be closed and
/// the display re-rendered from a background thread.
///
/// Section: terminal
///
/// Example:
/// (panel-close-after "out" 5000)
#[sl_sh_fn(fn_name = "panel-close-after")]
fn panel_close_after(name: String, millis: i64) -> VMResult<Value> {
    if millis < 0 {
        return Err(VMError::new(
            "console",
            "panel-close-after: delay must be non-negative".to_string(),
        ));
    }
    let panel_name = name.to_string();
    std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_millis(millis as u64));
        let mut mgr = panel::PANEL_MANAGER.lock().unwrap();
        if let Some(old_bounds) = mgr.close_panel(&panel_name) {
            render::render_all_panels_with_cleanup(&mgr, &[old_bounds]);
        }
    });
    Ok(Value::Nil)
}

/// Reset the terminal scroll region and clean up panel state.
/// Should be called before the shell exits to leave the terminal clean.
pub fn cleanup_console() {
    render::reset_scroll_region();
}

pub fn add_console_builtins(env: &mut SloshVm) {
    intern_terminal_size(env);

    add_builtin(
        env,
        "open-panel",
        open_panel,
        r#"Usage: (open-panel "name" :edge [:height/:width/:size N])

Open a docked terminal panel. The panel is attached to the specified edge
of the terminal and occupies N rows (for :top/:bottom) or columns
(for :left/:right). Default size is 10.

:edge is one of :top, :bottom, :left, :right
:height, :width, or :size specifies the panel dimension (default 10)

Section: terminal

Example:
(open-panel "out" :bottom :height 5)"#,
    );

    intern_close_panel_builtin(env);
    intern_panel_list(env);
    intern_panel_write(env);
    intern_panel_clear(env);
    intern_panel_resize(env);
    intern_panel_close_after(env);
}
