use crate::docs::{legacy, legacy_docs};
use bridge_macros::sl_sh_fn;
use compile_state::state::{SloshVm, SloshVmTrait};
use slosh_lib::load_test;
use slvm::{VMResult, Value};
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::iter;

pub const UNNECESSARY_IN_SLOSH: &str = "Not necessary in slosh.";

#[derive(Debug, Copy, Clone)]
enum ImplStatus {
    Implemented,
    NotYetImplemented,
    WillNotImplement,
}

impl Display for ImplStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ImplStatus::Implemented => {
                write!(f, "✅")
            }
            ImplStatus::NotYetImplemented => {
                write!(f, "❌")
            }
            ImplStatus::WillNotImplement => {
                write!(f, "☑️")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct StatusReport {
    completed: usize,
    yet_to_be_implemented: usize,
    status_entries: Vec<StatusEntry>,
}

#[derive(Debug, Clone)]
pub struct StatusEntry {
    resolved: ImplStatus,
    sl_sh_form: String,
    notes: String,
}

impl StatusEntry {
    fn new(resolved: ImplStatus, sl_sh_form: String, notes: String) -> Self {
        Self {
            resolved,
            sl_sh_form,
            notes,
        }
    }
}

pub(crate) fn build_report(vm: &mut SloshVm) -> VMResult<String> {
    let report = unimplemented_report(vm)?;
    let longest_old = report
        .status_entries
        .iter()
        .map(|e| e.sl_sh_form.len())
        .max()
        .unwrap_or(0);
    let longest_notes = report
        .status_entries
        .iter()
        .map(|e| e.notes.len())
        .max()
        .unwrap_or(0);
    let status_len = 3;
    let mut out = format!(
        r#"# tracking function Parity Between sl-sh and slosh
## Forms yet to be implemented: {}
## Forms implemented or skipped: {}"#,
        report.yet_to_be_implemented, report.completed
    );
    out = format!(
        "{}\n{:<status_len$} | {:<longest_old$} | {:<longest_notes$}|",
        out, "?", "Slosh Form", "Notes"
    );
    out = format!(
        "{out}\n{:<status_len$} | {:<longest_old$} | {:<longest_notes$}|",
        iter::repeat_n("-".to_string(), status_len - 1)
            .collect::<Vec<String>>()
            .join(""),
        iter::repeat_n("-".to_string(), longest_old - 1)
            .collect::<Vec<String>>()
            .join(""),
        iter::repeat_n("-".to_string(), longest_notes - 1)
            .collect::<Vec<String>>()
            .join(""),
    );

    for entry in report.status_entries {
        out = format!(
            "{}\n{:<status_len$} | `{:<longest_old$}` | {:<longest_notes$}|",
            out, entry.resolved, entry.sl_sh_form, entry.notes
        );
    }
    Ok(out)
}

pub(crate) fn unimplemented_report(vm: &mut SloshVm) -> VMResult<StatusReport> {
    let mut slosh_forms = HashSet::new();

    load_test(vm);
    // Add all global symbols
    for g in vm.globals().keys() {
        let sym = Value::Symbol(*g);
        let val: String = sym.display_value(vm);

        // Also add the symbol without namespace prefix if it has one
        if let Some(pos) = val.rfind("::") {
            let without_namespace = &val[pos + 2..];
            slosh_forms.insert(without_namespace.to_string());
        }

        slosh_forms.insert(val);
    }

    // Also check all namespaces for symbols
    let namespaces = builtins::get_namespaces_interned(vm);
    for ns_interned in namespaces {
        let symbols = builtins::retrieve_in_namespace(vm, &ns_interned);
        for symbol_value in symbols {
            if let Value::Symbol(_sym) = symbol_value {
                let full_name = symbol_value.display_value(vm);
                slosh_forms.insert(full_name.clone());

                // Also add the symbol without namespace prefix
                if let Some(pos) = full_name.rfind("::") {
                    let without_namespace = &full_name[pos + 2..];
                    slosh_forms.insert(without_namespace.to_string());
                }
            }
        }
    }

    let metadata = legacy_docs::full_legacy_sl_sh_forms_metadata();
    let mut completed = 0;
    let mut yet_to_be_implemented = 0;
    let mut status_entries = Vec::new();
    for (old_sym_name, will_implement, notes) in metadata {
        let (impl_status, form_name, notes) = match (will_implement, notes.is_empty()) {
            (true, true) => {
                // will implement and name is/has not yet been changed.
                // Check both the exact name and common namespace prefixes
                if slosh_forms.contains(*old_sym_name) {
                    (
                        ImplStatus::Implemented,
                        old_sym_name.to_string(),
                        "".to_string(),
                    )
                } else {
                    (
                        ImplStatus::NotYetImplemented,
                        old_sym_name.to_string(),
                        "".to_string(),
                    )
                }
            }
            (true, false) => {
                // will implement but with alternate name
                if slosh_forms.contains(*notes) {
                    (
                        ImplStatus::Implemented,
                        old_sym_name.to_string(),
                        format!("Renamed to: `{}`", notes),
                    )
                } else {
                    (
                        ImplStatus::NotYetImplemented,
                        old_sym_name.to_string(),
                        "".to_string(),
                    )
                }
            }
            (false, true) => {
                // will not implement and explanation is NOT provided
                (
                    ImplStatus::WillNotImplement,
                    old_sym_name.to_string(),
                    "".to_string(),
                )
            }
            (false, false) => {
                // will not implement and explanation is provided
                (
                    ImplStatus::WillNotImplement,
                    old_sym_name.to_string(),
                    notes.to_string(),
                )
            }
        };
        status_entries.push(StatusEntry::new(impl_status, form_name, notes));
    }
    for s in status_entries.iter() {
        match s.resolved {
            ImplStatus::NotYetImplemented => {
                yet_to_be_implemented += 1;
            }
            ImplStatus::Implemented | ImplStatus::WillNotImplement => {
                completed += 1;
            }
        }
    }
    Ok(StatusReport {
        completed,
        yet_to_be_implemented,
        status_entries,
    })
}

/// Usage: (legacy-report)
///
/// Outputs the current list of functions that have/haven't been implemented from sl-sh to slosh transition.
///
/// Section: doc
///
/// Example:
/// #t
#[sl_sh_fn(fn_name = "legacy-report", takes_env = true)]
pub fn legacy_report(environment: &mut SloshVm) -> VMResult<String> {
    legacy::build_report(environment)
}

pub fn get_legacy_sl_sh_form_syms(vm: &mut SloshVm, _registers: &[Value]) -> VMResult<Value> {
    let v = full_legacy_sl_sh_forms_metadata();
    let v = v
        .iter()
        .map(|x| {
            let name = x.0;
            vm.alloc_string_ro(name.to_string())
        })
        .collect();
    Ok(vm.alloc_vector(v))
}

/// Array of triples:
///     1. String name of the sl-sh form
///     2. Whether or not it will be implemented in slosh
///     3.  If 2 is true:
///             an optional non-null value listing the name the function is mapped to in slosh if
///             the name has changed.
///         If 2 is false:
///             a note a helpful explanation as to why the function is no longer needed or will not be implemented.
/// ** all values in .0 (or values in .2 if .1 is true) are assumed to match to some member of the set returned by
///     (get-globals-sorted), any values not found in this set are assumed to be unimplemented.
pub(crate) fn full_legacy_sl_sh_forms_metadata() -> &'static [(&'static str, bool, &'static str)] {
    [
        ("%", true, ""),
        ("*", true, ""),
        ("*active-ns*", true, "*ns*"),
        ("*bg-black*", true, ""),
        ("*bg-blue*", true, ""),
        ("*bg-cyan*", true, ""),
        ("*bg-default*", true, ""),
        ("*bg-green*", true, ""),
        ("*bg-magenta*", true, ""),
        ("*bg-red*", true, ""),
        ("*bg-white*", true, ""),
        ("*bg-yellow*", true, ""),
        ("*collection-src*", false, UNNECESSARY_IN_SLOSH),
        ("*core-src*", false, UNNECESSARY_IN_SLOSH),
        ("*euid*", true, ""),
        ("*euler*", true, ""),
        ("*fg-black*", true, ""),
        ("*fg-blue*", true, ""),
        ("*fg-cyan*", true, ""),
        ("*fg-default*", true, ""),
        ("*fg-green*", true, ""),
        ("*fg-magenta*", true, ""),
        ("*fg-red*", true, ""),
        ("*fg-white*", true, ""),
        ("*fg-yellow*", true, ""),
        ("*getopts-log*", false, UNNECESSARY_IN_SLOSH),
        ("*getopts-src*", false, UNNECESSARY_IN_SLOSH),
        ("*iterator-src*", false, UNNECESSARY_IN_SLOSH),
        ("*last-command*", true, ""),
        ("*last-status*", true, ""),
        ("*lib-src*", false, UNNECESSARY_IN_SLOSH),
        ("*load-path*", true, ""),
        ("*ns*", true, ""),
        ("*ns-exports*", false, UNNECESSARY_IN_SLOSH),
        ("*pi*", true, ""),
        ("*read-table*", false, UNNECESSARY_IN_SLOSH),
        ("*read-table-terminal*", false, UNNECESSARY_IN_SLOSH),
        ("*repl-settings*", true, ""),
        ("*run-script*", true, ""),
        ("*seq-src*", false, UNNECESSARY_IN_SLOSH),
        ("*shell-read-src*", false, UNNECESSARY_IN_SLOSH),
        ("*shell-src*", false, UNNECESSARY_IN_SLOSH),
        ("*slsh-std-src*", false, UNNECESSARY_IN_SLOSH),
        ("*slshrc-src*", false, UNNECESSARY_IN_SLOSH),
        ("*std-lib-exported-syms-hash*", true, ""),
        ("*std-lib-namespaces*", true, ""),
        ("*std-lib-syms-hash*", true, ""),
        ("*stderr*", true, ""),
        ("*stdin*", true, ""),
        ("*stdout*", true, ""),
        ("*string-read-table*", true, ""),
        ("*struct-src*", false, UNNECESSARY_IN_SLOSH),
        ("*test-src*", false, UNNECESSARY_IN_SLOSH),
        ("*uid*", true, ""),
        ("=+", true, ""),
        ("-", true, ""),
        ("->", true, ""),
        ("->>", true, ""),
        ("/", true, ""),
        ("2pow", true, ""),
        ("<", true, ""),
        ("<=", true, ""),
        ("=", true, ""),
        (">", true, ""),
        (">=", true, ""),
        ("^ns-stack-xyz^", true, ""),
        ("__prompt", true, ""),
        ("abs", true, ""),
        ("alias", true, ""),
        ("alias?", true, ""),
        ("and", true, ""),
        ("and-let*", true, ""),
        ("append", true, ""),
        ("append-iter", true, ""),
        ("append-to!", true, ""),
        ("apply", true, ""),
        ("apply-defaults", true, ""),
        ("arccos", true, ""),
        ("arcsin", true, ""),
        ("arctan", true, ""),
        ("args", true, "*args*"),
        ("arity-zero-can-not-be-required", true, ""),
        ("assert-equal", true, ""),
        ("assert-error", true, ""),
        ("assert-error-msg", true, ""),
        ("assert-false", true, ""),
        ("assert-includes", true, ""),
        ("assert-not-equal", true, ""),
        ("assert-not-includes", true, ""),
        ("assert-true", true, ""),
        ("back-quote", true, ""),
        ("bg", true, ""),
        ("bg-color-rgb", true, ""),
        ("block", true, ""),
        ("boolean?", true, ""),
        ("builtin?", true, ""),
        ("butlast", true, ""),
        ("caaar", true, ""),
        ("caadr", true, ""),
        ("caar", true, ""),
        ("cadar", true, ""),
        ("cadddr", true, ""),
        ("caddr", true, ""),
        ("cadr", true, ""),
        ("callable?", true, ""),
        ("car", true, ""),
        ("cd", true, ""),
        ("cdaar", true, ""),
        ("cdadr", true, ""),
        ("cdar", true, ""),
        ("cddar", true, ""),
        ("cdddr", true, ""),
        ("cddr", true, ""),
        ("cdr", true, ""),
        ("ceil", true, ""),
        ("chain", true, ""),
        ("chain-and", true, ""),
        ("chain-when", true, ""),
        ("char->int", true, "->int"),
        ("char-lower", true, ""),
        ("char-upper", true, ""),
        ("char-whitespace?", true, ""),
        ("char?", true, ""),
        ("check", true, ""),
        ("check-custom", true, ""),
        ("clear-dirs", true, ""),
        ("close", true, ""),
        ("codepoints", true, ""),
        ("collate-fs-changes", true, ""),
        ("collect", true, ""),
        ("collect-copy", true, ""),
        ("collect-str", true, ""),
        ("collect-vec", true, ""),
        ("cond", true, ""),
        ("consume-comment", true, ""),
        ("consume-whitespace", true, ""),
        ("cos", true, ""),
        ("dec!", true, ""),
        ("def", true, ""),
        ("def?", true, ""),
        ("defmacro", true, ""),
        ("defn", true, ""),
        (
            "defstruct",
            false,
            "Will consider other implementations if struct-like functionality if desired.",
        ),
        (
            "deftrait",
            false,
            "Will consider other implementations if trait-like functionality is desired.",
        ),
        ("dirs", true, ""),
        ("do", true, ""),
        ("do-unstr", true, ":>"),
        ("doc", true, ""),
        (
            "doc-raw",
            false,
            "Possible now with command: (get-prop 'symbol :doc-string)",
        ),
        ("dotimes", true, ""),
        ("dotimes-i", true, ""),
        ("double-ended-iter?", false, UNNECESSARY_IN_SLOSH),
        ("double-ended-iterator", false, UNNECESSARY_IN_SLOSH),
        ("dyn", true, ""),
        ("empty-seq?", true, ""),
        ("empty?", true, ""),
        ("enforce-constrains", true, ""),
        ("epoch", true, ""),
        ("eprint", true, "epr"),
        ("eprintln", true, "eprn"),
        ("err", true, ""),
        ("err>", true, ":2>"),   // TODO SLS what is it now?
        ("err>>", true, ":2>>"), // TODO SLS what is it now?
        ("err>null", true, ""),  // TODO SLS what is it now?
        ("error-stack-off", true, ""),
        ("error-stack-on", true, ""),
        ("eval", true, ""),
        ("exit", true, ""),
        ("exp", true, ""),
        ("expand-brace", true, ""),
        ("expand-dollar", true, ""),
        ("expand-macro", true, ""),
        ("expand-macro-all", true, ""),
        ("expand-macro1", true, ""),
        ("expand-tilde", true, ""),
        ("export", true, ""),
        ("false?", true, ""),
        ("falsey?", true, ""),
        ("fc", true, ""),
        ("fg", true, ""),
        ("fg-color-rgb", true, ""),
        ("file-iter", true, ""),
        ("file?", true, ""),
        ("filter", true, ""),
        ("filter-iter", true, ""),
        ("find-symbol", true, ""),
        ("first", true, ""),
        ("first-quartile", true, ""),
        ("fix-one-arg-bindings", true, ""),
        ("flatten-args", true, ""),
        ("float->int", true, "->int"),
        ("float?", true, ""),
        ("floor", true, ""),
        ("flush", true, "fflush"),
        ("fn", true, ""),
        ("fn-to-predicate", true, ""),
        ("fncall", true, ""),
        ("for", true, ""),
        ("for-i", true, ""),
        ("fork", true, ""),
        ("format", true, ""),
        ("fract", true, ""),
        ("fs-accessed", true, ""),
        ("fs-base", true, ""),
        ("fs-crawl", true, ""),
        ("fs-dir?", true, ""),
        ("fs-exists?", true, ""),
        ("fs-file?", true, ""),
        ("fs-len", true, ""),
        ("fs-modified", true, ""),
        ("fs-notify", true, ""),
        ("fs-parent", true, ""),
        ("fs-rm", true, ""),
        ("fs-same?", true, ""),
        ("func?", true, ""),
        ("gensym", true, ""),
        ("get-arity", true, ""),
        ("get-dirs", true, ""),
        ("get-env", true, "env"),
        ("get-error", true, ""),
        ("get-home", true, ""),
        ("get-next-params", true, ""),
        ("get-pid", true, ""),
        ("get-rgb-seq", true, ""),
        ("get-temp", true, ""),
        ("get-temp-file", true, ""),
        ("get_pwd", true, ""),
        ("getopts", true, ""),
        ("getopts-bad-first-arg", true, ""),
        ("getopts-bad-option-arity", true, ""),
        ("getopts-help", true, ""),
        ("getopts-help--options-map-is-map", true, ""),
        ("getopts-illegal-option", true, ""),
        ("getopts-invalid-type-function", true, ""),
        ("getopts-options-map-is-map", true, ""),
        ("getopts-type-error-message", true, ""),
        ("glob", true, ""),
        ("handle-last-command", true, ""),
        ("handle-process", true, ""),
        ("hash-clear!", true, ""),
        ("hash-get", true, ""),
        ("hash-haskey", true, "hash-haskey?"),
        ("hash-keys", true, ""),
        ("hash-remove!", true, ""),
        ("hash-set!", false, "(set! my-set.key \"value\")"),
        ("hash?", true, "hash-set?"),
        ("history-context", true, ""),
        ("history-empty?", true, ""),
        ("history-length", true, ""),
        ("history-nth", true, ""),
        ("history-push", true, ""),
        ("history-push-throwaway", true, ""),
        ("identity", true, ""),
        ("if", true, ""),
        ("import", true, ""),
        ("in?", true, ""),
        ("inc!", true, ""),
        ("int->float", true, "->float"),
        ("int?", true, ""),
        ("interleave", true, ""),
        ("interleave-iter", true, ""),
        ("intern-stats", true, ""),
        ("is-getopts-option-string", true, ""),
        ("is-multi-char-arg", true, ""),
        ("is-multi-single-char-args", true, ""),
        ("is-single-char-arg", true, ""),
        ("iter", true, ""),
        ("iter-or-single", true, ""),
        ("iter?", true, ""),
        ("iterator", true, ""),
        ("jobs", true, ""),
        ("join", true, ""),
        ("lambda?", true, "callable?"),
        ("last", true, ""),
        ("len0?", true, ""),
        ("len>0?", true, ""),
        ("length", true, ""),
        ("let", true, ""),
        ("let*", true, "let"),
        ("let-env", true, ""),
        ("list", true, ""),
        ("list-iter", true, ""),
        ("list-vec?", true, ""),
        ("list?", true, ""),
        ("lists=", true, ""),
        ("lne", true, ""),
        ("load", true, ""),
        ("log", true, ""),
        ("log2", true, ""),
        ("logger", true, ""),
        ("loop", true, ""),
        ("macro", true, ""),
        ("macro?", true, ""),
        ("make-hash", true, ""),
        ("make-hash-with-keys", true, ""),
        ("make-regex", true, ""),
        ("make-vec", true, ""),
        ("map", true, ""),
        ("map-iter", true, ""),
        ("match", true, ""),
        ("max", true, ""),
        ("maybe-docstring?", true, ""),
        ("maybe-glob?", true, ""),
        ("mean", true, ""),
        ("median", true, ""),
        ("meld", true, ""),
        ("meld-iter", true, ""),
        ("meta-add-tags", true, ""),
        ("meta-column-no", true, ""),
        ("meta-file-name", true, ""),
        ("meta-line-no", true, ""),
        ("meta-tag?", true, ""),
        ("method", true, ""),
        ("min", true, ""),
        ("mkli", true, ""),
        ("mode", true, ""),
        ("next!", true, ""),
        ("nil?", true, ""),
        ("non-empty-seq?", true, ""),
        ("none?", true, ""),
        ("not", true, ""),
        (
            "ns-auto-export",
            true,
            "All symbols defined in a namespace are exported in slosh.",
        ),
        ("ns-create", true, "with-ns"),
        ("ns-enter", true, "ns"),
        ("ns-exists?", true, ""),
        (
            "ns-export",
            false,
            "Everything in slosh is exported by default.",
        ),
        ("ns-import", true, "import"),
        ("ns-list", true, "get-namespaces"),
        ("ns-pop", false, UNNECESSARY_IN_SLOSH),
        ("ns-push", false, UNNECESSARY_IN_SLOSH),
        ("ns-symbols", true, "get-in-namespace"),
        ("nsubstitute!", true, ""),
        ("nth", true, ""),
        ("null", true, ""),
        ("nyi", true, ""),
        ("occurs", true, ""),
        ("open", true, "fopen"),
        ("or", true, ""),
        ("out-err>", true, ""),     // TODO SLS what is it now?
        ("out-err>>", true, ""),    // TODO SLS what is it now?
        ("out-err>null", true, ""), // TODO SLS what is it now?
        ("out>", true, ""),         // TODO SLS what is it now?
        ("out>>", true, ""),        // TODO SLS what is it now?
        ("out>null", true, ""),     // TODO SLS what is it now?
        (
            "pair=",
            false,
            "Do not need separate equals specifier for pair.",
        ),
        ("pair?", true, ""),
        ("path_list_trunc", true, ""),
        ("pid", true, ""),
        ("pipe", true, ""),
        ("pipe-err", false, UNNECESSARY_IN_SLOSH),
        ("popd", true, ""),
        ("pow", true, ""),
        ("print", true, "pr"),
        (
            "print-error",
            false,
            "Now one drops in to the debugger when an error occurs.",
        ),
        ("println", true, "prn"),
        ("probool", true, ""),
        ("process?", true, ""),
        ("prompt", true, ""),
        ("pushd", true, ""),
        ("qsort", true, ""),
        ("quote", true, ""),
        ("random", true, ""),
        ("random-str", true, ""),
        ("range", true, ""),
        ("range-iter", true, ""),
        ("re-color", true, ""),
        ("re-find", true, ""),
        ("re-find-all", true, ""),
        ("re-match", true, ""),
        ("re-replace", true, ""),
        ("read", true, ""),
        ("read-all", true, ""),
        ("read-line", true, ""),
        ("read-list", true, ""),
        ("read-string", true, ""),
        ("read-var", true, ""),
        ("read-var-bracket", true, ""),
        ("reader-macro-dot", true, ""),
        ("reap-jobs", true, ""),
        ("recur", true, ""),
        ("redir&>", true, ""),
        ("redir&>>", true, ""),
        ("redir2>", true, ""),
        ("redir2>>", true, ""),
        ("redir>", true, ""),
        ("redir>>", true, ""),
        ("reduce", true, ""),
        ("reduce-times", true, ""),
        ("ref", true, ""),
        ("regex?", true, ""),
        ("register-alias", true, ""),
        ("repeat", true, ""),
        ("repeat-iter", true, ""),
        ("repl", true, ""),
        ("repl-eof", true, ""),
        ("repl-line", true, ""),
        ("required-argument", true, ""),
        ("rest", true, ""),
        ("return-from", true, ""),
        ("reverse", true, ""),
        ("reverse-iter", true, ""),
        ("rm-esc", true, ""),
        ("round", true, ""),
        ("run-bg-first", true, ""),
        ("run-bg-prep-args", true, ""),
        ("run-example", true, ""),
        ("run-ns-example", true, ""),
        ("seq-for", true, ""),
        ("seq?", true, ""),
        ("set!", true, ""),
        ("set-dirs-max", true, ""),
        ("set_prompt_tail", true, ""),
        ("setnth!", true, ""),
        ("setup-chainer", true, ""),
        ("shell-read", true, ""),
        ("shell-read-int", true, ""),
        ("sin", true, ""),
        ("single-iter", true, ""),
        ("sleep", true, ""),
        ("slice", true, ""),
        ("slice-iter", true, ""),
        ("some?", true, ""),
        ("sqrt", true, ""),
        ("std-dev", true, ""),
        ("str", true, ""),
        ("str->float", true, "->float"),
        ("str->int", true, "->int"),
        ("str-append", true, ""),
        ("str-bytes", true, ""),
        ("str-cat-list", true, ""),
        ("str-clear!", true, ""),
        ("str-contains", true, ""),
        ("str-empty?", true, ""),
        ("str-iter-empty?", true, ""),
        ("str-iter-next!", true, ""),
        ("str-iter-peek", true, ""),
        ("str-iter-start", true, ""),
        ("str-lower", true, ""),
        ("str-ltrim", true, ""),
        ("str-map", true, ""),
        ("str-nth", true, ""),
        ("str-push!", true, ""),
        ("str-replace", true, ""),
        ("str-rsplit", true, ""),
        ("str-rsplitn", true, ""),
        ("str-rtrim", true, ""),
        ("str-split", true, ""),
        ("str-splitn", true, ""),
        ("str-starts-with", true, ""),
        ("str-sub", true, ""),
        ("str-trim", true, ""),
        ("str-upper", true, ""),
        ("string-iter", true, ""),
        ("string?", true, ""),
        ("substitute", true, ""),
        ("summary-stats", true, ""),
        ("supported-types-map", true, ""),
        ("sym", true, ""),
        ("sym->str", true, ""),
        ("symbol?", true, ""),
        ("syntax-off", true, ""),
        ("syntax-on", true, ""),
        ("sys-apply", true, ""),
        ("sys-command?", true, ""),
        ("syscall", true, "sh"),
        ("take", true, ""),
        ("take-iter", true, ""),
        ("tan", true, ""),
        ("temp-dir", true, ""),
        ("third-quartile", true, ""),
        ("time", true, ""),
        ("timer", true, ""),
        ("to-degrees", true, ""),
        ("to-radians", true, ""),
        ("tok-default-color", true, ""),
        ("tok-invalid-color", true, ""),
        ("tok-slsh-fcn-color", true, ""),
        ("tok-slsh-form-color", true, ""),
        ("tok-string-color", true, ""),
        ("tok-sys-alias-color", true, ""),
        ("tok-sys-command-color", true, ""),
        ("token-delim", true, ""),
        ("true?", true, ""),
        ("type", true, ""),
        ("umask", true, ""),
        ("unalias", true, ""),
        ("undef", true, ""),
        ("unexport", true, ""),
        ("unregister-alias", true, ""),
        ("unwind-protect", true, ""),
        ("valid-first-arg?", true, ""),
        ("values", true, ""),
        ("values-length", true, ""),
        ("values-nth", true, ""),
        ("values?", true, ""),
        ("var", true, ""),
        ("var-or-env", true, ""),
        ("vec", true, ""),
        ("vec-clear!", true, ""),
        ("vec-empty?", true, "empty?"),
        ("vec-insert!", true, ""),
        ("vec-iter", true, ""),
        ("vec-nth", true, "get"),
        ("vec-pop!", true, ""),
        ("vec-push!", true, ""),
        ("vec-remove!", true, ""),
        ("vec-set!", true, "set!"),
        ("vec-slice", true, ""),
        ("vec?", true, ""),
        (
            "verify-all-options-valid",
            false,
            "Leftover from getopts implementation.",
        ),
        (
            "verify-arity",
            false,
            "Leftover from getopts implementation.",
        ),
        ("version", true, ""),
        ("wait", true, ""),
        ("when", true, ""),
        ("with-padding", true, ""),
        ("with-temp", true, ""),
        ("with-temp-file", true, ""),
        ("write-line", true, ""),
        ("write-string", true, ""),
        ("xar!", true, ""),
        ("xdr!", true, ""),
    ]
    .as_slice()
}
