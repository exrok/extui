use extui::{AnsiColor, Color, Rgb, Style, vt::Modifier};

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
            fg @ 30..=37 => style.with_fg(AnsiColor(fg - 30)),
            38 => match next!() {
                5 => style.with_fg(AnsiColor(next!())),
                2 => {
                    let (r, g, b) = (next!(), next!(), next!());
                    style.with_fg(Color::Rgb(Rgb(r, g, b)))
                }
                _ => return,
            },
            39 => style.without_fg(),
            fg @ 90..=97 => style.with_fg(AnsiColor(fg - 90 + 8)),
            bg @ 40..=47 => style.with_bg(AnsiColor(bg - 40)),
            bg @ 100..=107 => style.with_bg(AnsiColor(bg - 100 + 8)),
            49 => style.without_bg(),
            48 => match next!() {
                5 => style.with_bg(AnsiColor(next!())),
                2 => {
                    let (r, g, b) = (next!(), next!(), next!());
                    style.with_bg(Color::Rgb(Rgb(r, g, b)))
                }
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
        AnsiColor(32).with_fg(AnsiColor(54)).delta().with_previous(Style::DEFAULT)
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

#[test]
fn rgb_style_roundtrip() {
    use extui::vt::BufferWrite;
    let cases = [
        Style::DEFAULT.with_fg(Color::rgb(0, 0, 0)),
        Style::DEFAULT.with_fg(Color::rgb(255, 255, 255)),
        Style::DEFAULT.with_bg(Color::rgb(12, 34, 56)),
        Style::DEFAULT
            .with_fg(Color::rgb(1, 2, 3))
            .with_bg(Color::rgb(4, 5, 6))
            .with_modifier(Modifier::BOLD),
        Style::DEFAULT
            .with_fg(Color::rgb(200, 100, 50))
            .with_bg(AnsiColor::Red1),
    ];

    for original in cases {
        let mut buf: Vec<u8> = Vec::new();
        original.write_to_buffer(&mut buf);
        let text = std::str::from_utf8(&buf).expect("valid utf8");
        // strip leading "\x1b[" and trailing "m"
        let inner = text
            .strip_prefix("\x1b[")
            .and_then(|s| s.strip_suffix('m'))
            .expect("SGR wrapper");

        let mut parsed = Style::DEFAULT;
        apply_raw_display_mode_vt_to_style(&mut parsed, inner);
        assert_eq!(parsed.fg(), original.fg(), "fg mismatch for {text:?}");
        assert_eq!(parsed.bg(), original.bg(), "bg mismatch for {text:?}");
        assert_eq!(
            parsed.modifiers(),
            original.modifiers(),
            "modifier mismatch for {text:?}"
        );
    }
}
