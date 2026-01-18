use extui::{Color, Style, vt::Modifier};

fn numericalize_unchecked_iter(text: &str) -> impl Iterator<Item = u8> {
    let mut bytes = text.as_bytes().iter();
    std::iter::from_fn(move || {
        let mut num = bytes.next()?.wrapping_sub(b'0');
        while let Some(&ch) = bytes.next() {
            if ch == b';' {
                return Some(num);
            }
            num = num.wrapping_mul(10).wrapping_add(ch.wrapping_sub(b'0'));
        }
        Some(num)
    })
}

fn apply_raw_display_mode_vt_to_style(style: &mut Style, escape: &str) {
    if escape.is_empty() {
        *style = Style::DEFAULT;
        return;
    }
    let mut digits = numericalize_unchecked_iter(escape);
    macro_rules! next {
        () => {
            if let Some(digit) = digits.next() {
                digit
            } else {
                return;
            }
        };
    }

    while let Some(digit) = digits.next() {
        *style = match digit {
            0 => Style::DEFAULT,
            1 => style.with_modifier(Modifier::BOLD),
            2 => style.with_modifier(Modifier::DIM),
            3 => style.with_modifier(Modifier::ITALIC),
            4 => style.with_modifier(Modifier::UNDERLINED),
            5 => style.with_modifier(Modifier::UNDERLINED),
            7 => style.with_modifier(Modifier::REVERSED),
            8 => style.with_modifier(Modifier::HIDDEN),
            9 => style.with_modifier(Modifier::CROSSED_OUT),
            21 => style.without_modifier(Modifier::BOLD),
            22 => style.without_modifier(Modifier::DIM),
            23 => style.without_modifier(Modifier::ITALIC),
            24 => style.without_modifier(Modifier::UNDERLINED),
            25 => style.without_modifier(Modifier::UNDERLINED),
            27 => style.without_modifier(Modifier::REVERSED),
            28 => style.without_modifier(Modifier::HIDDEN),
            29 => style.without_modifier(Modifier::CROSSED_OUT),
            fg @ 30..=37 => style.with_fg(extui::Color(fg - 30)),
            38 => match next!() {
                5 => style.with_fg(Color(next!())),
                _ => return,
            },
            39 => style.without_fg(),
            fg @ 90..=97 => style.with_fg(extui::Color(fg - 90 + 8)),
            bg @ 40..=47 => style.with_fg(extui::Color(bg - 30)),
            bg @ 100..=107 => style.with_bg(extui::Color(bg - 100 + 8)),
            49 => style.without_bg(),
            48 => match next!() {
                5 => style.with_bg(Color(next!())),
                _ => return,
            },
            _ => return,
        }
    }
}

#[derive(Debug)]
enum Segment<'a> {
    Text(&'a str),
    DisplayModifier(&'a str),
}

impl<'a> Segment<'a> {
    fn iterator(mut text: &'a str) -> impl Iterator<Item = Segment<'a>> {
        let mut dm = None::<&'a str>;
        std::iter::from_fn(move || {
            if let Some(segment) = dm.take() {
                return Some(Segment::DisplayModifier(segment));
            }
            if text.is_empty() {
                return None;
            }
            if let Some((prefix, encoded)) = text.split_once("\x1b[") {
                let (encoded, remaining) = encoded
                    .split_once('m')
                    .expect("Unterminated display escape code");
                text = remaining;
                if prefix.is_empty() {
                    return Some(Segment::DisplayModifier(encoded));
                } else {
                    dm = Some(encoded);
                    return Some(Segment::Text(prefix));
                }
            }
            Some(Segment::Text(std::mem::replace(&mut text, "")))
        })
    }
}

use extui::vt::BufferWrite;
macro_rules! splat {
    ($out: expr; $($expr:expr)*) => {{
        let out: &mut Vec<u8> = $out;
        $(
            $expr.write_to_buffer(out);
        )*
    }};
}

#[test]
fn dothing() {
    let mut buf: Vec<u8> = Vec::new();
    splat!(&mut buf;
        "hello"
        Color(32).with_fg(Color(54)).delta().with_previous(Style::DEFAULT)
        "nice"
    );
    let text = std::str::from_utf8(&buf).unwrap();
    for segment in Segment::iterator(text) {
        match segment {
            Segment::Text(text) => println!("{text}"),
            Segment::DisplayModifier(fire) => {
                println!("{}", fire);
                let mut style = Style::DEFAULT;
                apply_raw_display_mode_vt_to_style(&mut style, fire);
                println!("{:?}", style);
            }
        }
    }
}
