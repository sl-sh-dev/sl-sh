use sl_liner::{
    last_non_ws_char_was_not_backslash, Buffer, DefaultEditorRules, EditorRules, NewlineRule,
    WordDivideRule,
};
use std::cmp::Ordering;
use std::collections::HashMap;
use unicode_width::UnicodeWidthStr;

/// Delimiters like ( and ) are open and close respectively. delimiters like " are identical.
#[derive(Debug, Clone, Copy)]
enum DelimiterIdx {
    Identical(usize),
    Open(usize),
    Close(usize),
}

impl DelimiterIdx {
    fn idx(&self, other: &Self) -> (usize, usize) {
        let self_loc = match self {
            DelimiterIdx::Identical(i) => i,
            DelimiterIdx::Open(i) => i,
            DelimiterIdx::Close(i) => i,
        };
        let other_loc = match other {
            DelimiterIdx::Identical(i) => i,
            DelimiterIdx::Open(i) => i,
            DelimiterIdx::Close(i) => i,
        };
        (*self_loc, *other_loc)
    }
}

impl DelimiterIdx {
    fn get_idx(&self) -> usize {
        match self {
            DelimiterIdx::Identical(i) => *i,
            DelimiterIdx::Open(i) => *i,
            DelimiterIdx::Close(i) => *i,
        }
    }
}

impl PartialEq<Self> for DelimiterIdx {
    fn eq(&self, other: &Self) -> bool {
        let (this, other) = self.idx(other);
        this.eq(&other)
    }
}

impl PartialOrd for DelimiterIdx {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let (this, other) = self.idx(other);
        this.partial_cmp(&other)
    }
}

fn order_sorted_lists<T, U, V>(open_indices: T, close_indices: T) -> Vec<U>
where
    U: AsRef<V> + Copy,
    T: IntoIterator<Item = U>,
    V: PartialOrd,
{
    let mut vec = vec![];
    let mut open_indices = open_indices.into_iter();
    let mut close_indices = close_indices.into_iter();
    let (mut open_curr, mut close_curr) = (open_indices.next(), close_indices.next());
    loop {
        match (open_curr, close_curr) {
            (Some(open), Some(close)) => {
                if open.as_ref() <= close.as_ref() {
                    vec.push(open);
                    open_curr = open_indices.next();
                } else {
                    vec.push(close);
                    close_curr = close_indices.next();
                }
            }
            (Some(open), None) => {
                vec.push(open);
                open_curr = open_indices.next();
            }
            (None, Some(close)) => {
                vec.push(close);
                close_curr = close_indices.next();
            }
            (None, None) => {
                break;
            }
        }
    }
    vec
}

#[derive(Debug, Copy, Clone)]
enum DelimiterType<'a> {
    EscapeSequence(&'a str),
    MultilineComment(&'a str),
    StringDelimiter(&'a str),
    BalancedDelimiter(&'a str),
}

#[derive(Debug, Copy, Clone)]
struct Delimiter<'a> {
    delimiter_type: DelimiterType<'a>,
    delimiter_place: DelimiterIdx,
}

impl AsRef<DelimiterIdx> for DelimiterIdx {
    fn as_ref(&self) -> &DelimiterIdx {
        self
    }
}

impl AsRef<DelimiterIdx> for Delimiter<'_> {
    fn as_ref(&self) -> &DelimiterIdx {
        &self.delimiter_place
    }
}

fn get_delimiters<'a>(
    str: &'a str,
    pat: &'a str,
    to_type: &dyn Fn() -> DelimiterType<'a>,
    to_idx: &dyn Fn(usize) -> DelimiterIdx,
) -> Vec<Delimiter<'a>> {
    str.match_indices(pat)
        .map(|(i, _)| Delimiter {
            delimiter_type: to_type(),
            delimiter_place: to_idx(i),
        })
        .collect::<Vec<Delimiter>>()
}

fn get_indices_of_tokens<'a>(
    str: &'a str,
    delimiters: &[(&'a str, &'a str)],
    multiline_comment: &'a (&str, &str),
    string_delimiter: &'a str,
    escape_sequence: &'a str,
) -> Vec<Delimiter<'a>> {
    let (open, close) = multiline_comment;
    let open_indices = get_delimiters(str, open, &|| DelimiterType::MultilineComment(open), &|i| {
        DelimiterIdx::Open(i)
    });
    let close_indices = get_delimiters(
        str,
        close,
        &|| DelimiterType::MultilineComment(close),
        &DelimiterIdx::Close,
    );
    let all_delimiters = order_sorted_lists(open_indices, close_indices);
    let string_indices = get_delimiters(
        str,
        string_delimiter,
        &|| DelimiterType::StringDelimiter(string_delimiter),
        &DelimiterIdx::Identical,
    );

    let all_delimiters = order_sorted_lists(all_delimiters, string_indices);
    let backslash_indices = get_delimiters(
        str,
        escape_sequence,
        &|| DelimiterType::EscapeSequence(escape_sequence),
        &DelimiterIdx::Identical,
    );
    let mut all_delimiters = order_sorted_lists(all_delimiters, backslash_indices);

    for (open, close) in delimiters {
        let open_indices = get_delimiters(
            str,
            open,
            &|| DelimiterType::BalancedDelimiter(open),
            &DelimiterIdx::Open,
        );
        let close_indices = get_delimiters(
            str,
            close,
            &|| DelimiterType::BalancedDelimiter(close),
            &DelimiterIdx::Close,
        );
        let ordered_balanced = order_sorted_lists(open_indices, close_indices);
        all_delimiters = order_sorted_lists(ordered_balanced, all_delimiters);
    }
    all_delimiters
}

fn map_right_to_left_delimiters<'a>(
    delimiters: &[(&'a str, &'a str)],
) -> HashMap<&'a str, &'a str> {
    let mut delim_map = HashMap::new();
    for (left, right) in delimiters {
        delim_map.insert(*right, *left);
    }
    delim_map
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum DelimiterState {
    InStringDelimiter,
    InMultilineComment(usize),
    Outside,
}

fn should_count_delimiters(
    delimiter: Delimiter,
    prev_delimiter: Option<Delimiter>,
    state: DelimiterState,
) -> DelimiterState {
    let is_escaped = is_preceded_by_escape_character(delimiter, prev_delimiter);
    match (state, is_escaped) {
        (DelimiterState::InStringDelimiter, false) => {
            if let DelimiterType::StringDelimiter(_) = delimiter.delimiter_type {
                DelimiterState::Outside
            } else {
                DelimiterState::InStringDelimiter
            }
        }
        (DelimiterState::InMultilineComment(x), false) => {
            if let DelimiterType::MultilineComment(_) = delimiter.delimiter_type {
                match delimiter.delimiter_place {
                    DelimiterIdx::Open(_) => DelimiterState::InMultilineComment(x + 1),
                    DelimiterIdx::Close(_) => {
                        if x > 1 {
                            DelimiterState::InMultilineComment(x - 1)
                        } else {
                            DelimiterState::Outside
                        }
                    }
                    DelimiterIdx::Identical(_) => {
                        unreachable!()
                    }
                }
            } else {
                DelimiterState::InMultilineComment(1)
            }
        }
        (DelimiterState::Outside, false) => {
            match (delimiter.delimiter_type, delimiter.delimiter_place) {
                (DelimiterType::MultilineComment(_), DelimiterIdx::Open(_)) => {
                    DelimiterState::InMultilineComment(1)
                }
                (DelimiterType::StringDelimiter(_), DelimiterIdx::Identical(_)) => {
                    DelimiterState::InStringDelimiter
                }
                _ => state,
            }
        }
        _ => state,
    }
}

fn is_preceded_by_escape_character(
    delimiter: Delimiter,
    prev_delimiter: Option<Delimiter>,
) -> bool {
    if let Some(prev_delimiter) = prev_delimiter {
        if let DelimiterType::EscapeSequence(escape) = prev_delimiter.delimiter_type {
            let after_escape = prev_delimiter.delimiter_place.get_idx() + escape.width();
            let next_delimiter_start_idx = delimiter.delimiter_place.get_idx();
            if after_escape.eq(&next_delimiter_start_idx) {
                return true;
            }
        }
    }
    false
}

/// ensures all provided delimiters are balanced.
fn check_balanced_delimiters(
    str: &str,
    delimiters: &[(&str, &str)],
    multiline_comment: &(&str, &str),
    string_delimiter: &str,
    escape_sequence: &str,
) -> bool {
    let delim_map = map_right_to_left_delimiters(delimiters);
    let all_delimiters = get_indices_of_tokens(
        str,
        delimiters,
        multiline_comment,
        string_delimiter,
        escape_sequence,
    );

    let mut open_delims = HashMap::new();

    let mut prev_state = DelimiterState::Outside;
    let mut prev_delimiter = None;
    for delimiter in all_delimiters {
        let new_state = should_count_delimiters(delimiter, prev_delimiter, prev_state);
        if let DelimiterState::Outside = new_state {
            match (delimiter.delimiter_type, delimiter.delimiter_place) {
                (DelimiterType::BalancedDelimiter(str), DelimiterIdx::Open(_)) => {
                    if let Some(&count) = open_delims.get(str) {
                        open_delims.insert(str, count + 1);
                    } else {
                        open_delims.insert(str, 1);
                    }
                }
                (DelimiterType::BalancedDelimiter(str), DelimiterIdx::Close(_)) => {
                    let opposite = delim_map.get(str);
                    if let Some(opposite) = opposite {
                        if let Some(&count) = open_delims.get(opposite) {
                            if count == 1 {
                                open_delims.remove(opposite);
                            } else {
                                open_delims.insert(opposite, count - 1);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        prev_state = new_state;
        prev_delimiter = Some(delimiter);
    }
    prev_state == DelimiterState::Outside && open_delims.is_empty()
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

pub struct NewlineForBackSlashAndSlShSyntaxRule<'a> {
    escape_character: &'a str,
    string_delimiter: &'a str,
    delimiters: Vec<(&'a str, &'a str)>,
    multiline_comment: (&'a str, &'a str),
}

impl NewlineRule for NewlineForBackSlashAndSlShSyntaxRule<'_> {
    fn evaluate_on_newline(&self, buf: &Buffer) -> bool {
        last_non_ws_char_was_not_backslash(buf)
            && check_balanced_delimiters(
                buf.range_graphemes_all().slice(),
                &self.delimiters,
                &self.multiline_comment,
                self.string_delimiter,
                self.escape_character,
            )
    }
}

pub struct LinerWordDividerRule {}

impl WordDivideRule for LinerWordDividerRule {
    fn divide_words(&self, buf: &Buffer) -> Vec<(usize, usize)> {
        get_liner_words(buf)
    }
}

pub fn make_editor_rules() -> Box<dyn EditorRules> {
    let delimiters = vec![("{", "}"), ("(", ")"), ("[", "]")];
    let multiline_comment = ("#|", "|#");
    let string_delimiter = "\"";
    let escape_character = "\\";
    let editor_rules = DefaultEditorRules::custom(
        LinerWordDividerRule {},
        NewlineForBackSlashAndSlShSyntaxRule {
            escape_character,
            delimiters,
            string_delimiter,
            multiline_comment,
        },
    );
    Box::new(editor_rules)
}
