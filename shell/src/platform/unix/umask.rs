use nix::sys::stat::Mode;
use std::io;
use std::io::ErrorKind;

pub use nix::libc::mode_t;

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

fn get_class(str: &str) -> Result<mode_t, io::Error> {
    if str.is_empty() {
        Ok(0b111111111)
    } else {
        let next = str.chars().find(|x| !is_user_access_token(*x));
        if next.is_some() {
            let msg =
                "symbolic mode string before the '+' can only contain u, g, o, or a.".to_string();
            Err(io::Error::new(ErrorKind::Other, msg))
        } else {
            let mut class: mode_t = 0;
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

fn get_perms(str: &str) -> Result<mode_t, io::Error> {
    if str.is_empty() {
        Ok(0b111111111)
    } else {
        let next = str.chars().find(|x| !is_permission_token(*x));
        if next.is_some() {
            let msg =
                "symbolic mode string before the '+' can only contain r, w, or x.".to_string();
            Err(io::Error::new(ErrorKind::Other, msg))
        } else {
            let mut class: mode_t = 0;
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

fn decode_symbolic_mode_string(str: &str, split_char: char) -> Result<(mode_t, mode_t), io::Error> {
    let mode_strings = str.split(split_char).collect::<Vec<&str>>();
    if mode_strings.len() == 2 {
        if let (Some(c), Some(p)) = (mode_strings.first(), mode_strings.get(1)) {
            if c.is_empty() && p.is_empty() {
                let msg = format!(
                    "symbolic mode string must have a valid character before and/or \
                        after the '{}' character.",
                    split_char,
                );
                Err(io::Error::new(ErrorKind::Other, msg))
            } else {
                let class = get_class(c)?;
                let perms = get_perms(p)?;
                Ok((class, perms))
            }
        } else {
            let msg = format!(
                "symbolic mode string contains too many '{}' characters.",
                split_char,
            );
            Err(io::Error::new(ErrorKind::Other, msg))
        }
    } else {
        let msg = format!(
            "symbolic mode string contains too many '{}' characters.",
            split_char,
        );
        Err(io::Error::new(ErrorKind::Other, msg))
    }
}

enum PermissionOperator {
    Plus,
    Minus,
    Equal,
}

struct MaskType {
    class: mode_t,
    perms: mode_t,
    mask_type: PermissionOperator,
}

impl MaskType {
    #[allow(clippy::unnecessary_cast)]
    fn combine(&self, mode: mode_t) -> mode_t {
        let m = match &self.mask_type {
            PermissionOperator::Plus => !(self.class & self.perms) & mode,
            PermissionOperator::Minus => mode | (self.class & self.perms),
            PermissionOperator::Equal => {
                ((self.class & self.perms) ^ 0o777) & ((mode & !self.class) ^ self.class)
            }
        };
        to_mode(m).bits() as mode_t
    }
}

fn to_mask_type(str: &str) -> Result<MaskType, io::Error> {
    let decode = |split_char| -> Result<(mode_t, mode_t), io::Error> {
        decode_symbolic_mode_string(str, split_char)
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
        let msg = "symbolic mode string must contain one of '+', '-', or '='.".to_string();
        Err(io::Error::new(ErrorKind::Other, msg))
    }
}

fn get_umask_tokens(str: &str) -> Result<Vec<MaskType>, io::Error> {
    match str.lines().count() {
        1 => {
            let mut masks = vec![];
            for x in str.split(',') {
                let mask_type = to_mask_type(x)?;
                masks.push(mask_type);
            }
            Ok(masks)
        }
        _ => {
            let msg = "must supply only one line as input.".to_string();
            Err(io::Error::new(ErrorKind::Other, msg))
        }
    }
}

fn with_umask_tokens(mut umask: mode_t, masks: Vec<MaskType>) -> mode_t {
    for x in masks {
        umask = x.combine(umask)
    }
    umask
}

/// makes sure the returned string is 4 characters and the first character is 0.
fn make_parsable_octal_string(str: &str) -> Result<String, io::Error> {
    if str.is_empty() {
        let msg = "no input.".to_string();
        Err(io::Error::new(ErrorKind::Other, msg))
    } else if str.len() > 4 {
        let msg = "no more than 4 characters can be used to specify a umask, e.g.\
             644 or 0222."
            .to_string();
        Err(io::Error::new(ErrorKind::Other, msg))
    } else if str.len() == 4 && !str.starts_with('0') {
        let msg = "most significant octal character can only be 0.".to_string();
        Err(io::Error::new(ErrorKind::Other, msg))
    } else {
        let mut ret = String::from(str);
        while ret.len() < 4 {
            ret = "0".to_owned() + &ret;
        }
        Ok(ret)
    }
}

fn build_mask(to_shift: usize, c: char) -> Result<mode_t, io::Error> {
    let apply = |m| Ok((m << (to_shift * 3)) as mode_t);
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
            let msg = "octal format can only take on values between 0 and 7 inclusive.".to_string();
            Err(io::Error::new(ErrorKind::Other, msg))
        }
    }
}

fn octal_string_to_mode_t(str: &str) -> Result<mode_t, io::Error> {
    let mut val = 0;
    let mut err = false;
    for (usize, c) in str.chars().rev().enumerate() {
        match usize {
            0..=2 => val |= build_mask(usize, c)?,
            3 => {}
            _ => {
                err = true;
                break;
            }
        }
    }
    if err {
        let msg = "failed to parse provided octal.".to_string();
        Err(io::Error::new(ErrorKind::Other, msg))
    } else {
        Ok(val)
    }
}

#[allow(clippy::unnecessary_cast)]
fn to_mode(i: mode_t) -> Mode {
    NIX_PERMISSIONS.iter().fold(Mode::empty(), |acc, x| {
        if (x.bits() as mode_t & i) == x.bits() as mode_t {
            acc | *x
        } else {
            acc
        }
    })
}

fn octal_string_to_mode(str: &str) -> Result<Mode, io::Error> {
    let str = make_parsable_octal_string(str)?;
    let val = octal_string_to_mode_t(&str)?;
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

/// If mask_string is a mode string then merge it with umask and set the current umask.
/// If mask_string is an int then treat it as a umask and set the current umask (no merge)).
pub fn merge_and_set_umask(current_umask: mode_t, mask_string: &str) -> Result<mode_t, io::Error> {
    if mask_string.parse::<u32>().is_ok() {
        let mode = octal_string_to_mode(mask_string)?;
        nix::sys::stat::umask(mode);
        Ok(mode.bits())
    } else if !mask_string.is_empty() {
        #[allow(clippy::unnecessary_cast)]
        let mode = if is_digit(mask_string.chars().next().unwrap()) {
            octal_string_to_mode(mask_string)?.bits() as mode_t
        } else {
            let masks = get_umask_tokens(mask_string)?;
            with_umask_tokens(current_umask, masks)
        };
        if let Some(umask) = Mode::from_bits(mode) {
            nix::sys::stat::umask(umask);
            Ok(mode)
        } else {
            Err(io::Error::new(
                ErrorKind::Other,
                "invalid umask".to_string(),
            ))
        }
    } else {
        let msg = "no input.".to_string();
        Err(io::Error::new(ErrorKind::Other, msg))
    }
}

/// Cears the current umask and returns the previous umask.
#[allow(clippy::unnecessary_cast)]
pub fn get_and_clear_umask() -> mode_t {
    nix::sys::stat::umask(Mode::empty()).bits() as mode_t
}

/// Set current umask to umask.
#[allow(clippy::unnecessary_cast)]
pub fn set_umask(umask: mode_t) -> Result<(), io::Error> {
    if let Some(umask) = Mode::from_bits(umask) {
        nix::sys::stat::umask(umask);
        Ok(())
    } else {
        Err(io::Error::new(ErrorKind::Other, "invalid mode"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_perms() {
        let perms_str = "rwx";
        let perms = get_perms(perms_str).unwrap();
        assert_eq!(0b111111111, perms);

        let perms_str = "r";
        let perms = get_perms(perms_str).unwrap();
        assert_eq!(0b100100100, perms);

        let perms_str = "w";
        let perms = get_perms(perms_str).unwrap();
        assert_eq!(0b010010010, perms);

        let perms_str = "x";
        let perms = get_perms(perms_str).unwrap();
        assert_eq!(0b001001001, perms);

        let perms_str = "rw";
        let perms = get_perms(perms_str).unwrap();
        assert_eq!(0b110110110, perms);
    }

    #[test]
    fn test_parse_class() {
        let class_str = "ugo";
        let class = get_class(class_str).unwrap();
        assert_eq!(0b111111111, class);

        let class_str = "a";
        let class = get_class(class_str).unwrap();
        assert_eq!(0b111111111, class);

        let class_str = "u";
        let class = get_class(class_str).unwrap();
        assert_eq!(0b111000000, class);

        let class_str = "g";
        let class = get_class(class_str).unwrap();
        assert_eq!(0b000111000, class);

        let class_str = "o";
        let class = get_class(class_str).unwrap();
        assert_eq!(0b000000111, class);

        let class_str = "uo";
        let class = get_class(class_str).unwrap();
        assert_eq!(0b111000111, class);
    }

    #[test]
    fn test_umask_octal() {
        let bs = 0b001001001;
        let m = to_mode(bs);
        assert_eq!(bs, m.bits());

        let m = octal_string_to_mode("0522").unwrap();
        assert_eq!(338, m.bits() as u32);

        let m = octal_string_to_mode("522").unwrap();
        assert_eq!(338, m.bits() as u32);

        let m = octal_string_to_mode("713").unwrap();
        assert_eq!(0b111001011, m.bits() as u32);

        let m = octal_string_to_mode("466").unwrap();
        assert_eq!(0b100110110, m.bits() as u32);

        let m = octal_string_to_mode("0").unwrap();
        assert_eq!(0b000000000, m.bits() as u32);

        let m = octal_string_to_mode("45").unwrap();
        assert_eq!(0b000100101, m.bits() as u32);

        assert!(octal_string_to_mode("a+n").is_err());

        assert!(octal_string_to_mode("1111").is_err());

        assert!(octal_string_to_mode("11111").is_err());

        assert!(octal_string_to_mode("0S11").is_err());
    }

    #[test]
    #[allow(clippy::unnecessary_cast)]
    fn test_umask_parser() {
        let umask = to_mode(0o022).bits() as mode_t;

        let m = with_umask_tokens(umask, get_umask_tokens("go+rx").unwrap());
        assert_eq!(0o022, m);

        let m = with_umask_tokens(umask, get_umask_tokens("+w").unwrap());
        assert_eq!(0o0, m);

        let m = with_umask_tokens(umask, get_umask_tokens("a-rw").unwrap());
        assert_eq!(0o666, m);

        let m = with_umask_tokens(umask, get_umask_tokens("g-rw").unwrap());
        assert_eq!(0o062, m);

        let m = with_umask_tokens(umask, get_umask_tokens("ug=rw").unwrap());
        assert_eq!(0o0112, m);

        let m = with_umask_tokens(umask, get_umask_tokens("a=r,ug=rw").unwrap());
        assert_eq!(0o0113, m);

        let m = with_umask_tokens(umask, get_umask_tokens("a=r,g+w").unwrap());
        assert_eq!(0o0313, m);

        let m = with_umask_tokens(umask, get_umask_tokens("a=r,a-r").unwrap());
        assert_eq!(0o0777, m);

        let m = with_umask_tokens(umask, get_umask_tokens("ugo+x").unwrap());
        assert_eq!(0o0022, m);

        let m = with_umask_tokens(umask, get_umask_tokens("+x").unwrap());
        assert_eq!(0o0022, m);

        let m = with_umask_tokens(umask, get_umask_tokens("a+rw").unwrap());
        assert_eq!(0o0, m);

        let m = with_umask_tokens(umask, get_umask_tokens("a=r").unwrap());
        assert_eq!(0o333, m);

        let m = with_umask_tokens(umask, get_umask_tokens("ug+rwx").unwrap());
        assert_eq!(0o002, m);

        let m = with_umask_tokens(umask, get_umask_tokens("ug+").unwrap());
        assert_eq!(0o002, m);

        let m = with_umask_tokens(umask, get_umask_tokens("o-rwx").unwrap());
        assert_eq!(0o0027, m);

        let m = with_umask_tokens(umask, get_umask_tokens("u-x,g=r,o+w").unwrap());
        assert_eq!(0o0130, m);

        assert!(get_umask_tokens("glo+rx").is_err());

        assert!(get_umask_tokens("go+nrx").is_err());

        assert!(get_umask_tokens("+n").is_err());

        assert!(get_umask_tokens("a+n").is_err());

        assert!(get_umask_tokens("ar").is_err());

        assert!(get_umask_tokens("+a+r").is_err());

        assert!(get_umask_tokens("a++r").is_err());

        assert!(get_umask_tokens("+ar+").is_err());

        assert!(get_umask_tokens("").is_err());
    }
}
