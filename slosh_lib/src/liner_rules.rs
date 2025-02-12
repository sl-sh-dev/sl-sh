use sl_liner::{
    last_non_ws_char_was_not_backslash, Buffer, DefaultEditorRules, EditorRules, NewlineRule,
    WordDivideRule,
};

/// ensures all provided delimiters are balanced for a lisp sexp.
fn check_balanced_delimiters_lisp(input: &str) -> bool {
    let mut parens: i32 = 0;
    let mut brackets: i32 = 0;
    let mut braces: i32 = 0;
    let mut double_quote = false;
    let mut escape = false;
    // TODO, should probably handle multiline comments #|...|#, docstrings #!...!# and string literals #"X...X"
    for ch in input.chars() {
        if escape {
            escape = false;
            continue;
        }
        if double_quote && ch == '"' {
            double_quote = false;
            continue;
        }
        if double_quote && ch != '\\' {
            continue;
        }
        match ch {
            '(' => parens += 1,
            ')' => parens -= 1,
            '[' => brackets += 1,
            ']' => brackets -= 1,
            '{' => braces += 1,
            '}' => braces -= 1,
            '"' => double_quote = true,
            '\\' => escape = true,
            _ => {}
        }
    }

    parens <= 0 && brackets <= 0 && braces <= 0 && !double_quote
}

/// ensures all provided delimiters are balanced for a shell line.
fn check_balanced_delimiters_shell(input: &str) -> bool {
    let mut parens: i32 = 0;
    let mut braces: i32 = 0;
    let mut single_quote = false;
    let mut double_quote = false;
    let mut escape = false;
    for ch in input.chars() {
        if escape {
            escape = false;
            continue;
        }
        if double_quote && ch == '"' {
            double_quote = false;
            continue;
        }
        if single_quote && ch == '\'' {
            single_quote = false;
            continue;
        }
        if double_quote || single_quote {
            continue;
        }
        match ch {
            '(' => parens += 1,
            ')' => parens -= 1,
            '{' => braces += 1,
            '}' => braces -= 1,
            '"' => double_quote = true,
            '\'' => single_quote = true,
            '\\' => escape = true,
            _ => {}
        }
    }

    parens <= 0 && braces <= 0 && !single_quote && !double_quote
}

/// If first non-whitespace char is '(' assume this is an sexp.
fn is_sexp(input: &str) -> bool {
    for ch in input.chars() {
        if ch.is_whitespace() {
            continue;
        }
        return ch == '(';
    }
    false
}
/// ensures all provided delimiters are balanced.
fn check_balanced_delimiters(input: &str) -> bool {
    if is_sexp(input) {
        check_balanced_delimiters_lisp(input)
    } else {
        check_balanced_delimiters_shell(input)
    }
}

// Like the liner default but make '(' and ')' their own words for cleaner completions.
fn get_liner_words(buf: &Buffer) -> Vec<(usize, usize)> {
    let mut res = Vec::new();

    let mut word_start = None;
    let mut just_had_backslash = false;

    for (i, str) in buf.range_graphemes_all().enumerate() {
        if str == "\\" {
            just_had_backslash = true;
            continue;
        }

        if let Some(start) = word_start {
            if (str == " " || str == "(" || str == ")") && !just_had_backslash {
                res.push((start, i));
                if str == "(" || str == ")" {
                    res.push((i, i + 1));
                }
                word_start = None;
            }
        } else if str == "(" || str == ")" {
            res.push((i, i + 1));
        } else if str != " " {
            word_start = Some(i);
        }

        just_had_backslash = false;
    }

    if let Some(start) = word_start {
        res.push((start, buf.num_graphemes()));
    }

    res
}

pub struct NewlineForBackSlashAndSlShSyntaxRule {}

impl NewlineRule for NewlineForBackSlashAndSlShSyntaxRule {
    fn evaluate_on_newline(&self, buf: &Buffer) -> bool {
        last_non_ws_char_was_not_backslash(buf)
            && check_balanced_delimiters(buf.range_graphemes_all().slice())
    }
}

pub struct LinerWordDividerRule {}

impl WordDivideRule for LinerWordDividerRule {
    fn divide_words(&self, buf: &Buffer) -> Vec<(usize, usize)> {
        get_liner_words(buf)
    }
}

pub fn make_editor_rules() -> Box<dyn EditorRules> {
    let editor_rules = DefaultEditorRules::custom(
        LinerWordDividerRule {},
        NewlineForBackSlashAndSlShSyntaxRule {},
    );
    Box::new(editor_rules)
}
