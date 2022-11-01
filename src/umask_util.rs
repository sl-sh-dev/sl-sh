use crate::{ExpEnum, LispError};
use nix::sys::stat::Mode;

static NIX_PERMISSIONS: &[Mode] = &[
    Mode::S_IRUSR,
    Mode::S_IWUSR,
    Mode::S_IXUSR,
    Mode::S_IRGRP,
    Mode::S_IWGRP,
    Mode::S_IXGRP,
    Mode::S_IROTH,
    Mode::S_IWOTH,
    Mode::S_IXOTH,
];

pub fn to_octal_string(mode: Mode, fn_name: &str) -> Result<String, LispError> {
    let mut octal = format!("{:o}", mode.bits());
    if octal.len() < 4 {
        while octal.len() < 4 {
            octal = "0".to_owned() + &octal;
        }
        Ok(octal)
    } else {
        let msg = format!("{}: Encountered invalid umask {}.", fn_name, octal);
        Err(LispError::new(msg))
    }
}

fn get_class(str: &str, fn_name: &str) -> Result<u32, LispError> {
    if str.is_empty() {
        Ok(0b111111111)
    } else {
        let next = str.chars().find(|x| !is_user_access_token(*x));
        if next.is_some() {
            let msg = format!(
                "{}: symbolic mode string before the '+' can only contain u, g, o, or a.",
                fn_name
            );
            Err(LispError::new(msg))
        } else {
            let mut class: u32 = 0;
            for c in str.chars() {
                class |= match c {
                    'u' => 0b111000000,
                    'g' => 0b000111000,
                    'o' => 0b000000111,
                    'a' => 0b111111111,
                    c if c.is_whitespace() => 0b111111111,
                    _ => 0,
                }
            }
            Ok(class)
        }
    }
}

fn get_perms(str: &str, fn_name: &str) -> Result<u32, LispError> {
    if str.is_empty() {
        Ok(0b111111111)
    } else {
        let next = str.chars().find(|x| !is_permission_token(*x));
        if next.is_some() {
            let msg = format!(
                "{}: symbolic mode string before the '+' can only contain r, w, or x.",
                fn_name
            );
            Err(LispError::new(msg))
        } else {
            let mut class: u32 = 0;
            for c in str.chars() {
                class |= match c {
                    'r' => 0b100100100,
                    'w' => 0b010010010,
                    'x' => 0b001001001,
                    _ => 0,
                }
            }
            Ok(class)
        }
    }
}

fn decode_symbolic_mode_string(
    str: &str,
    split_char: char,
    fn_name: &str,
) -> Result<(u32, u32), LispError> {
    let mode_strings = str.split(split_char).collect::<Vec<&str>>();
    if mode_strings.len() == 2 {
        if let (Some(c), Some(p)) = (mode_strings.first(), mode_strings.get(1)) {
            if c.is_empty() && p.is_empty() {
                let msg = format!(
                    "{}: symbolic mode string must have a valid character before and/or \
                        after the '{}' character.",
                    fn_name, split_char,
                );
                Err(LispError::new(msg))
            } else {
                let class = get_class(c, fn_name)?;
                let perms = get_perms(p, fn_name)?;
                Ok((class, perms))
            }
        } else {
            let msg = format!(
                "{}: symbolic mode string contains too many '{}' characters.",
                fn_name, split_char,
            );
            Err(LispError::new(msg))
        }
    } else {
        let msg = format!(
            "{}: symbolic mode string contains too many '{}' characters.",
            fn_name, split_char,
        );
        Err(LispError::new(msg))
    }
}

enum PermissionOperator {
    Plus,
    Minus,
    Equal,
}

struct MaskType {
    class: u32,
    perms: u32,
    mask_type: PermissionOperator,
}

impl MaskType {
    fn combine(&self, mode: Mode) -> Mode {
        let m = match &self.mask_type {
            PermissionOperator::Plus => !(self.class & self.perms) & mode.bits() as u32,
            PermissionOperator::Minus => mode.bits() as u32 | (self.class & self.perms),
            PermissionOperator::Equal => {
                ((self.class & self.perms) ^ 0o777)
                    & ((mode.bits() as u32 & !self.class) ^ self.class)
            }
        };
        to_mode(m)
    }
}

fn to_mask_type(str: &str, fn_name: &str) -> Result<MaskType, LispError> {
    let decode = |split_char| -> Result<(u32, u32), LispError> {
        decode_symbolic_mode_string(str, split_char, fn_name)
    };
    if str.contains('+') {
        let (class, perms) = decode('+')?;
        Ok(MaskType {
            class,
            perms,
            mask_type: PermissionOperator::Plus,
        })
    } else if str.contains('-') {
        let (class, perms) = decode('-')?;
        Ok(MaskType {
            class,
            perms,
            mask_type: PermissionOperator::Minus,
        })
    } else if str.contains('=') {
        let (class, perms) = decode('=')?;
        Ok(MaskType {
            class,
            perms,
            mask_type: PermissionOperator::Equal,
        })
    } else {
        let msg = format!(
            "{}: symbolic mode string must contain one of '+', '-', or '='.",
            fn_name
        );
        Err(LispError::new(msg))
    }
}

fn get_umask_tokens(str: &str, fn_name: &str) -> Result<Vec<MaskType>, LispError> {
    match str.lines().count() {
        1 => {
            let mut masks = vec![];
            for x in str.split(',') {
                let mask_type = to_mask_type(x, fn_name)?;
                masks.push(mask_type);
            }
            Ok(masks)
        }
        _ => {
            let msg = format!("{}: Must supply only one line as input.", fn_name);
            Err(LispError::new(msg))
        }
    }
}

fn with_umask_tokens(mut umask: Mode, masks: Vec<MaskType>) -> Mode {
    for x in masks {
        umask = x.combine(umask)
    }
    umask
}

/// makes sure the returned string is 4 characters and the first character is 0.
fn make_parsable_octal_string(str: &str, fn_name: &str) -> Result<String, LispError> {
    if str.is_empty() {
        let msg = format!("{}: no input.", fn_name);
        Err(LispError::new(msg))
    } else if str.len() > 4 {
        let msg = format!(
            "{}: no more than 4 characters can be used to specify a umask, e.g.\
             644 or 0222.",
            fn_name
        );
        Err(LispError::new(msg))
    } else if str.len() == 4 && !str.starts_with('0') {
        let msg = format!(
            "{}: Most significant octal character can only be 0.",
            fn_name
        );
        Err(LispError::new(msg))
    } else {
        let mut ret = String::from(str);
        while ret.len() < 4 {
            ret = "0".to_owned() + &ret;
        }
        Ok(ret)
    }
}

fn build_mask(to_shift: usize, c: char, fn_name: &str) -> Result<u32, LispError> {
    let apply = |m| Ok((m << (to_shift * 3)) as u32);
    match c {
        '0' => apply(0b000),
        '1' => apply(0b001),
        '2' => apply(0b010),
        '3' => apply(0b011),
        '4' => apply(0b100),
        '5' => apply(0b101),
        '6' => apply(0b110),
        '7' => apply(0b111),
        _ => {
            let msg = format!(
                "{}: Octal format can only take on values between 0 and 7 inclusive.",
                fn_name
            );
            Err(LispError::new(msg))
        }
    }
}

fn octal_string_to_u32(str: &str, fn_name: &str) -> Result<u32, LispError> {
    let mut val = 0;
    let mut err = false;
    for (usize, c) in str.chars().rev().enumerate() {
        match usize {
            0..=2 => val |= build_mask(usize, c, fn_name)?,
            3 => {}
            _ => {
                err = true;
                break;
            }
        }
    }
    if err {
        let msg = format!("{}: Failed to parse provided octal.", fn_name);
        Err(LispError::new(msg))
    } else {
        Ok(val)
    }
}

fn to_mode(i: u32) -> Mode {
    NIX_PERMISSIONS.iter().fold(Mode::empty(), |acc, x| {
        if (x.bits() as u32 & i) == x.bits() as u32 {
            acc | *x
        } else {
            acc
        }
    })
}

fn octal_string_to_mode(str: &str, fn_name: &str) -> Result<Mode, LispError> {
    let str = make_parsable_octal_string(str, fn_name)?;
    let val = octal_string_to_u32(&str, fn_name)?;
    Ok(to_mode(val))
}

fn is_permission_token(ch: char) -> bool {
    matches!(ch, 'r' | 'w' | 'x')
}

fn is_user_access_token(ch: char) -> bool {
    matches!(ch, 'u' | 'g' | 'o' | 'a')
}

fn is_digit(ch: char) -> bool {
    matches!(
        ch,
        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    )
}

pub fn with_umask(umask: Mode, exp_enum: &ExpEnum, fn_name: &str) -> Result<Mode, LispError> {
    match exp_enum {
        ExpEnum::Int(i) => {
            let mode = octal_string_to_mode(&format!("{}", i), fn_name)?;
            nix::sys::stat::umask(mode);
            Ok(mode)
        }
        ExpEnum::String(s, _) => {
            if s.len() > 0 {
                let mode = if is_digit(s.chars().next().unwrap()) {
                    octal_string_to_mode(s.as_ref(), fn_name)?
                } else {
                    let masks = get_umask_tokens(s.as_ref(), fn_name)?;
                    with_umask_tokens(umask, masks)
                };
                Ok(mode)
            } else {
                let msg = format!("{}: no input.", fn_name);
                Err(LispError::new(msg))
            }
        }
        _ => {
            let msg = format!(
                "{}: requires string or octal to use as file creation mask",
                fn_name
            );
            Err(LispError::new(msg))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_perms() {
        let fn_name = "umask";
        let perms_str = "rwx";
        let perms = get_perms(perms_str, fn_name).unwrap();
        assert_eq!(0b111111111, perms);

        let perms_str = "r";
        let perms = get_perms(perms_str, fn_name).unwrap();
        assert_eq!(0b100100100, perms);

        let perms_str = "w";
        let perms = get_perms(perms_str, fn_name).unwrap();
        assert_eq!(0b010010010, perms);

        let perms_str = "x";
        let perms = get_perms(perms_str, fn_name).unwrap();
        assert_eq!(0b001001001, perms);

        let perms_str = "rw";
        let perms = get_perms(perms_str, fn_name).unwrap();
        assert_eq!(0b110110110, perms);
    }

    #[test]
    fn test_parse_class() {
        let fn_name = "umask";
        let class_str = "ugo";
        let class = get_class(class_str, fn_name).unwrap();
        assert_eq!(0b111111111, class);

        let class_str = "a";
        let class = get_class(class_str, fn_name).unwrap();
        assert_eq!(0b111111111, class);

        let class_str = "u";
        let class = get_class(class_str, fn_name).unwrap();
        assert_eq!(0b111000000, class);

        let class_str = "g";
        let class = get_class(class_str, fn_name).unwrap();
        assert_eq!(0b000111000, class);

        let class_str = "o";
        let class = get_class(class_str, fn_name).unwrap();
        assert_eq!(0b000000111, class);

        let class_str = "uo";
        let class = get_class(class_str, fn_name).unwrap();
        assert_eq!(0b111000111, class);
    }

    #[test]
    fn test_umask_octal() {
        let fn_name = "umask";
        let bs = 0b001001001;
        let m = to_mode(bs);
        assert_eq!(bs, m.bits() as u32);

        let m = octal_string_to_mode("0522", fn_name).unwrap();
        assert_eq!(338, m.bits() as u32);

        let m = octal_string_to_mode("522", fn_name).unwrap();
        assert_eq!(338, m.bits() as u32);

        let m = octal_string_to_mode("713", fn_name).unwrap();
        assert_eq!(0b111001011, m.bits() as u32);

        let m = octal_string_to_mode("466", fn_name).unwrap();
        assert_eq!(0b100110110, m.bits() as u32);

        let m = octal_string_to_mode("0", fn_name).unwrap();
        assert_eq!(0b000000000, m.bits() as u32);

        let m = octal_string_to_mode("45", fn_name).unwrap();
        assert_eq!(0b000100101, m.bits() as u32);

        assert!(octal_string_to_mode("a+n", fn_name).is_err());

        assert!(octal_string_to_mode("1111", fn_name).is_err());

        assert!(octal_string_to_mode("11111", fn_name).is_err());

        assert!(octal_string_to_mode("0S11", fn_name).is_err());
    }

    #[test]
    fn test_umask_parser() {
        let fn_name = "umask";
        let umask = to_mode(0o022);

        let m = with_umask_tokens(umask, get_umask_tokens("go+rx", fn_name).unwrap());
        assert_eq!(0o022, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("+w", fn_name).unwrap());
        assert_eq!(0o0, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("a-rw", fn_name).unwrap());
        assert_eq!(0o666, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("g-rw", fn_name).unwrap());
        assert_eq!(0o062, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("ug=rw", fn_name).unwrap());
        assert_eq!(0o0112, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("a=r,ug=rw", fn_name).unwrap());
        assert_eq!(0o0113, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("a=r,g+w", fn_name).unwrap());
        assert_eq!(0o0313, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("a=r,a-r", fn_name).unwrap());
        assert_eq!(0o0777, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("ugo+x", fn_name).unwrap());
        assert_eq!(0o0022, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("+x", fn_name).unwrap());
        assert_eq!(0o0022, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("a+rw", fn_name).unwrap());
        assert_eq!(0o0, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("a=r", fn_name).unwrap());
        assert_eq!(0o333, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("ug+rwx", fn_name).unwrap());
        assert_eq!(0o002, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("ug+", fn_name).unwrap());
        assert_eq!(0o002, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("o-rwx", fn_name).unwrap());
        assert_eq!(0o0027, m.bits() as u32);

        let m = with_umask_tokens(umask, get_umask_tokens("u-x,g=r,o+w", fn_name).unwrap());
        assert_eq!(0o0130, m.bits() as u32);

        assert!(get_umask_tokens("glo+rx", fn_name).is_err());

        assert!(get_umask_tokens("go+nrx", fn_name).is_err());

        assert!(get_umask_tokens("+n", fn_name).is_err());

        assert!(get_umask_tokens("a+n", fn_name).is_err());

        assert!(get_umask_tokens("ar", fn_name).is_err());

        assert!(get_umask_tokens("+a+r", fn_name).is_err());

        assert!(get_umask_tokens("a++r", fn_name).is_err());

        assert!(get_umask_tokens("+ar+", fn_name).is_err());

        assert!(get_umask_tokens("", fn_name).is_err());
    }
}
