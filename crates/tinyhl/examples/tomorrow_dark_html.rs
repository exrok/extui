//! HTML preview that mirrors the giallo-verify color mapping.
//!
//! This renders a source file through tinyhl's [`RenderSpans`] and colors each
//! span with the exact base16-tomorrow palette and mapping the `giallo-verify`
//! harness uses. The statistical harness reports *where* tinyhl and giallo
//! disagree. This preview lets you *see* tinyhl's own output under the same
//! palette, so a suspected mis-color can be confirmed by eye. Click any token
//! to inspect the lexical kind, semantic role, delimiter depth, and resolved
//! color that produced it.
//!
//! ```text
//! cargo run --example tomorrow_dark_html -- rust src/lib.rs > preview.html
//! ```

use std::env;
use std::fs;
use std::io::{self, Read};

use tinyhl::{Highlighter, Language, RenderSpan, RenderSpans, SemanticKind, Source, Span, kind};

// base16-tomorrow palette, identical to `giallo-verify`'s constants (as
// `Color::as_hex` emits them: uppercase, 6-digit). Keep these in sync with
// `crates/giallo-verify/src/main.rs` so the preview matches the harness.
const FG: &str = "#B5B8B6"; // editor.foreground / default text
const TYPE: &str = "#E6C890"; // entity.name.type (any UpperCamel / primitive / const)
const FUNC: &str = "#92AABC"; // entity.name.function
const BINDING: &str = "#C47D7A"; // variable.declaration / variable.field / parameter
const NAMESPACE: &str = "#D4A07A"; // entity.name.namespace (lowercase path segments)
const KEYWORD: &str = "#B6A2BA"; // keyword / storage / self / crate
const STRING: &str = "#BBBE85"; // string
const NUMBER: &str = "#CCCCCC"; // constant.numeric
const COMMENT: &str = "#7E7F7E"; // comment

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = env::args().skip(1);
    let language = match args.next() {
        Some(arg) => parse_language(&arg).map_err(io::Error::other)?,
        None => {
            eprintln!("usage: cargo run --example tomorrow_dark_html -- <language> [path]");
            std::process::exit(2);
        }
    };

    let input = match args.next() {
        Some(path) => fs::read_to_string(path)?,
        None => {
            let mut buf = String::new();
            io::stdin().read_to_string(&mut buf)?;
            buf
        }
    };

    println!("{}", render_html(language, &input));
    Ok(())
}

fn render_html(language: Language, src: &str) -> String {
    let source: &dyn Source = &src;
    let mut hl = Highlighter::new(language);
    hl.rebuild(source);
    let full = Span::new(0, hl.source_len().unwrap_or(0));

    let mut html = String::new();
    html.push_str(
        r#"<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>TinyHL base16-tomorrow Preview</title>
  <style>
    :root {
      --bg: #1e1e1f;
      --fg: #b5b8b6;
      --gutter: #969896;
      --selection: #373b41;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      background: var(--bg);
      color: var(--fg);
      font: 16px/1.55 Menlo, Consolas, "Liberation Mono", monospace;
    }
    main {
      min-height: 100vh;
      padding: 32px;
      display: grid;
      grid-template-columns: minmax(0, 1fr) 320px;
      gap: 24px;
      background: var(--bg);
    }
    .frame {
      max-width: 1100px;
      margin: 0 auto;
      border: 1px solid #282a2e;
      background: #161617;
      box-shadow: 0 24px 60px rgba(0,0,0,0.35);
      overflow: hidden;
    }
    .inspector {
      align-self: start;
      position: sticky;
      top: 24px;
      border: 1px solid #282a2e;
      background: rgba(22,22,23,0.94);
      box-shadow: 0 24px 60px rgba(0,0,0,0.28);
      backdrop-filter: blur(8px);
    }
    .toolbar {
      display: flex;
      align-items: center;
      gap: 10px;
      padding: 12px 16px;
      border-bottom: 1px solid #282a2e;
      background: #202225;
      color: var(--gutter);
      text-transform: uppercase;
      letter-spacing: 0.08em;
      font-size: 12px;
    }
    .dot {
      width: 10px;
      height: 10px;
      border-radius: 50%;
      background: #373b41;
    }
    .toolbar .dot:nth-child(1) { background: #cc6666; }
    .toolbar .dot:nth-child(2) { background: #f0c674; }
    .toolbar .dot:nth-child(3) { background: #b5bd68; }
    pre {
      margin: 0;
      padding: 24px;
      overflow: auto;
      white-space: pre-wrap;
      word-break: break-word;
    }
    .token {
      border-radius: 4px;
      cursor: pointer;
      transition: background-color 120ms ease, box-shadow 120ms ease;
    }
    .token:hover {
      background: rgba(255,255,255,0.06);
    }
    .token.is-active {
      background: rgba(129,162,190,0.18);
      box-shadow: inset 0 0 0 1px rgba(129,162,190,0.55);
    }
    .panel {
      padding: 18px 20px 20px;
    }
    .panel h2 {
      margin: 0 0 12px;
      font-size: 14px;
      letter-spacing: 0.06em;
      text-transform: uppercase;
      color: var(--gutter);
    }
    .panel-row {
      margin-top: 14px;
    }
    .panel-label {
      margin-bottom: 4px;
      font-size: 11px;
      letter-spacing: 0.08em;
      text-transform: uppercase;
      color: var(--gutter);
    }
    .panel-value {
      color: var(--fg);
      white-space: pre-wrap;
      word-break: break-word;
    }
    .panel-code {
      display: inline-block;
      padding: 2px 6px;
      border-radius: 4px;
      background: var(--selection);
    }
    .swatch {
      display: inline-block;
      width: 11px;
      height: 11px;
      border-radius: 3px;
      margin-right: 6px;
      vertical-align: -1px;
      border: 1px solid rgba(255,255,255,0.2);
    }
    .panel-list {
      margin: 0;
      padding-left: 18px;
      color: var(--fg);
    }
    .panel-list li + li {
      margin-top: 6px;
    }
    @media (max-width: 900px) {
      main {
        grid-template-columns: 1fr;
      }
      .inspector {
        position: static;
      }
    }
  </style>
</head>
<body>
  <main>
    <section class="frame">
      <div class="toolbar">
        <span class="dot"></span>
        <span class="dot"></span>
        <span class="dot"></span>
"#,
    );
    html.push_str(&escape_html(&format!(
        "{language:?} - TinyHL base16-tomorrow"
    )));
    html.push_str(
        r#"
      </div>
      <pre><code id="code-view">"#,
    );

    for rspan in RenderSpans::new(&hl, full) {
        let text = &src[rspan.span.offset as usize..rspan.span.end() as usize];
        if rspan.local_kind == kind::WHITESPACE {
            html.push_str(&escape_html(text));
            continue;
        }

        let color = render_color(&rspan).unwrap_or(FG);
        let role = color_name(color);
        let lang = Language::from_tag(rspan.lang_tag);
        let semantic_name = match rspan.semantic {
            Some(kind) => semantic_kind_name(kind),
            None => "none",
        };
        let delimiter = match rspan.delimiter {
            Some(depth) => format!("rainbow depth {depth}"),
            None => "none".to_string(),
        };
        let explanation = explanation_lines(&rspan, color, role, lang);

        html.push_str(r#"<span tabindex="0" class="token" style="color: "#);
        html.push_str(color);
        html.push_str(r#"" data-text=""#);
        html.push_str(&escape_html_attr(text));
        html.push_str(r#"" data-language=""#);
        html.push_str(rspan.lang_tag.to_string().as_str());
        html.push_str(r#"" data-language-name=""#);
        html.push_str(&escape_html_attr(&format!("{lang:?}")));
        html.push_str(r#"" data-offset=""#);
        html.push_str(rspan.span.offset.to_string().as_str());
        html.push_str(r#"" data-len=""#);
        html.push_str(rspan.span.len.to_string().as_str());
        html.push_str(r#"" data-color=""#);
        html.push_str(color);
        html.push_str(r#"" data-role=""#);
        html.push_str(role);
        html.push_str(r#"" data-local-kind=""#);
        html.push_str(&escape_html_attr(local_kind_name(rspan.local_kind)));
        html.push_str(r#"" data-semantic=""#);
        html.push_str(&escape_html_attr(semantic_name));
        html.push_str(r#"" data-delimiter=""#);
        html.push_str(&escape_html_attr(&delimiter));
        html.push_str(r#"" data-explanation=""#);
        html.push_str(&escape_html_attr(&explanation.join("\n")));
        html.push_str(r#"">"#);
        html.push_str(&escape_html(text));
        html.push_str("</span>");
    }

    html.push_str(
        r#"</code></pre>
    </section>
    <aside class="inspector frame">
      <div class="toolbar">
        <span class="dot"></span>
        <span class="dot"></span>
        <span class="dot"></span>
        Segment Inspector
      </div>
      <div class="panel">
        <h2>Color Explanation</h2>
        <div class="panel-row">
          <div class="panel-label">Token</div>
          <div class="panel-value"><code id="inspector-text" class="panel-code">Click any colored segment</code></div>
        </div>
        <div class="panel-row">
          <div class="panel-label">Language</div>
          <div class="panel-value" id="inspector-language">-</div>
        </div>
        <div class="panel-row">
          <div class="panel-label">Span</div>
          <div class="panel-value" id="inspector-span">-</div>
        </div>
        <div class="panel-row">
          <div class="panel-label">Resolved Color</div>
          <div class="panel-value" id="inspector-color">-</div>
        </div>
        <div class="panel-row">
          <div class="panel-label">Lexical Kind</div>
          <div class="panel-value" id="inspector-base">-</div>
        </div>
        <div class="panel-row">
          <div class="panel-label">Semantic Role</div>
          <div class="panel-value" id="inspector-semantic">-</div>
        </div>
        <div class="panel-row">
          <div class="panel-label">Delimiter Depth</div>
          <div class="panel-value" id="inspector-delimiter">-</div>
        </div>
        <div class="panel-row">
          <div class="panel-label">Reasoning</div>
          <ul class="panel-list" id="inspector-why">
            <li>Click any non-whitespace token in the preview.</li>
          </ul>
        </div>
      </div>
    </aside>
  </main>
  <script>
    (() => {
      const tokens = document.querySelectorAll('.token');
      const text = document.getElementById('inspector-text');
      const language = document.getElementById('inspector-language');
      const span = document.getElementById('inspector-span');
      const color = document.getElementById('inspector-color');
      const base = document.getElementById('inspector-base');
      const semantic = document.getElementById('inspector-semantic');
      const delimiter = document.getElementById('inspector-delimiter');
      const why = document.getElementById('inspector-why');
      let active = null;

      function setActive(node) {
        if (active) active.classList.remove('is-active');
        active = node;
        if (active) active.classList.add('is-active');
      }

      function update(node) {
        setActive(node);
        text.textContent = node.dataset.text;
        language.textContent = `${node.dataset.languageName} (tag ${node.dataset.language})`;
        span.textContent = `offset ${node.dataset.offset}, len ${node.dataset.len}`;
        const hex = node.dataset.color;
        color.innerHTML = `<span class="swatch" style="background:${hex}"></span>${node.dataset.role} (${hex})`;
        base.textContent = node.dataset.localKind;
        semantic.textContent = node.dataset.semantic;
        delimiter.textContent = node.dataset.delimiter;
        why.innerHTML = '';
        for (const line of node.dataset.explanation.split('\n')) {
          const li = document.createElement('li');
          li.textContent = line;
          why.appendChild(li);
        }
      }

      for (const node of tokens) {
        node.addEventListener('click', () => update(node));
        node.addEventListener('keydown', (event) => {
          if (event.key === 'Enter' || event.key === ' ') {
            event.preventDefault();
            update(node);
          }
        });
      }

      if (tokens.length > 0) update(tokens[0]);
    })();
  </script>
</body>
</html>"#,
    );
    html
}

/// Maps a tinyhl [`RenderSpan`] onto the base16-tomorrow palette. This is the
/// exact mapping `giallo-verify`'s `render_color` uses: base16 has no rainbow
/// brackets and no distinct operator color, so delimiters, punctuation, errors,
/// and plain text all take the default foreground.
fn render_color(span: &RenderSpan) -> Option<&'static str> {
    if span.delimiter.is_some() {
        return Some(FG);
    }
    Some(match span.local_kind {
        kind::COMMENT | kind::DOC_COMMENT => COMMENT,
        kind::STRING | kind::CHAR | kind::TEMPLATE_STRING | kind::REGEX => STRING,
        kind::NUMBER => NUMBER,
        kind::KEYWORD => match span.semantic {
            Some(SemanticKind::TypeName | SemanticKind::TypeDefinition) => TYPE,
            _ => KEYWORD,
        },
        // Identifiers, the lifetime name, and the macro `!` take their color
        // from the semantic role. Untagged ones fall back to plain text.
        _ => semantic_color(span.semantic).unwrap_or(FG),
    })
}

/// Maps a render-ready [`SemanticKind`] to its base16 color, or [`None`] for
/// the roles base16 leaves as plain text.
fn semantic_color(semantic: Option<SemanticKind>) -> Option<&'static str> {
    Some(match semantic? {
        SemanticKind::TypeDefinition | SemanticKind::TypeName => TYPE,
        SemanticKind::FunctionDefinition
        | SemanticKind::FunctionCall
        | SemanticKind::MethodDefinition
        | SemanticKind::MethodCall
        | SemanticKind::MacroCall => FUNC,
        SemanticKind::PathComponent => NAMESPACE,
        SemanticKind::VariableDefinition
        | SemanticKind::Parameter
        | SemanticKind::FieldDefinition
        | SemanticKind::Field
        | SemanticKind::Lifetime => BINDING,
        SemanticKind::FieldAccess
        | SemanticKind::Variable
        | SemanticKind::Argument
        | SemanticKind::MetaVariable => return None,
    })
}

/// Human-readable palette role for a resolved color, for the inspector.
fn color_name(color: &str) -> &'static str {
    match color {
        TYPE => "type",
        FUNC => "function",
        BINDING => "binding",
        NAMESPACE => "namespace",
        KEYWORD => "keyword",
        STRING => "string",
        NUMBER => "number",
        COMMENT => "comment",
        _ => "default",
    }
}

fn parse_language(arg: &str) -> Result<Language, String> {
    match arg.to_ascii_lowercase().as_str() {
        "json" => Ok(Language::Json),
        "rust" | "rs" => Ok(Language::Rust),
        "c" => Ok(Language::C),
        "csv" => Ok(Language::Csv),
        "ts" | "typescript" => Ok(Language::Ts),
        "tsx" | "jsx" => Ok(Language::Tsx),
        "toml" => Ok(Language::Toml),
        "md" | "markdown" => Ok(Language::Markdown),
        "xml" => Ok(Language::Xml),
        "css" => Ok(Language::Css),
        "html" | "htm" => Ok(Language::Html),
        "go" | "golang" => Ok(Language::Go),
        "sh" | "shell" | "bash" => Ok(Language::Sh),
        "make" | "makefile" | "mk" => Ok(Language::Make),
        "cmake" => Ok(Language::Cmake),
        "proto" | "protobuf" => Ok(Language::Protobuf),
        "ini" | "dosini" => Ok(Language::Ini),
        "conf" | "config" | "cfg" => Ok(Language::Conf),
        "wgsl" | "wesl" => Ok(Language::Wgsl),
        other => Err(format!("unsupported language: {other}")),
    }
}

fn explanation_lines(span: &RenderSpan, color: &str, role: &str, lang: Language) -> Vec<String> {
    let mut lines = Vec::new();
    lines.push(format!(
        "Lexical kind {}.",
        local_kind_name(span.local_kind)
    ));
    match span.semantic {
        Some(kind) => lines.push(format!(
            "Semantic overlay marks this token '{}', which drives the color.",
            semantic_kind_name(kind)
        )),
        None => lines
            .push("No semantic overlay applies, so the lexical kind drives the color.".to_string()),
    }
    if let Some(depth) = span.delimiter {
        lines.push(format!(
            "Bracket at rainbow depth {depth}. base16-tomorrow has no rainbow brackets, so it stays default text."
        ));
    }
    lines.push(format!(
        "Resolved to the {role} color {color} under the base16-tomorrow mapping."
    ));
    lines.push(format!(
        "Language {lang:?}, byte span [{}..{}).",
        span.span.offset,
        span.span.end()
    ));
    lines
}

fn semantic_kind_name(kind: SemanticKind) -> &'static str {
    match kind {
        SemanticKind::TypeDefinition => "type-definition",
        SemanticKind::TypeName => "type-name",
        SemanticKind::FunctionDefinition => "function-definition",
        SemanticKind::FunctionCall => "function-call",
        SemanticKind::MethodDefinition => "method-definition",
        SemanticKind::MethodCall => "method-call",
        SemanticKind::Parameter => "parameter",
        SemanticKind::Argument => "argument",
        SemanticKind::VariableDefinition => "variable-definition",
        SemanticKind::Variable => "variable",
        SemanticKind::FieldDefinition => "field-definition",
        SemanticKind::Field => "field",
        SemanticKind::FieldAccess => "field-access",
        SemanticKind::PathComponent => "path-component",
        SemanticKind::MetaVariable => "meta-variable",
        SemanticKind::MacroCall => "macro-call",
        SemanticKind::Lifetime => "lifetime",
    }
}

fn local_kind_name(local: u16) -> &'static str {
    match local {
        kind::WHITESPACE => "WHITESPACE",
        kind::COMMENT => "COMMENT",
        kind::DOC_COMMENT => "DOC_COMMENT",
        kind::STRING => "STRING",
        kind::TEMPLATE_STRING => "TEMPLATE_STRING",
        kind::NUMBER => "NUMBER",
        kind::CHAR => "CHAR",
        kind::REGEX => "REGEX",
        kind::IDENT => "IDENT",
        kind::KEYWORD => "KEYWORD",
        kind::LIFETIME => "LIFETIME",
        kind::ERROR => "ERROR",
        kind::OPEN_BRACE => "OPEN_BRACE",
        kind::CLOSE_BRACE => "CLOSE_BRACE",
        kind::OPEN_PAREN => "OPEN_PAREN",
        kind::CLOSE_PAREN => "CLOSE_PAREN",
        kind::OPEN_BRACKET => "OPEN_BRACKET",
        kind::CLOSE_BRACKET => "CLOSE_BRACKET",
        kind::COMMA => "COMMA",
        kind::SEMI => "SEMI",
        kind::COLON => "COLON",
        kind::DOT => "DOT",
        kind::QUESTION => "QUESTION",
        kind::AT => "AT",
        kind::HASH => "HASH",
        kind::TILDE => "TILDE",
        kind::DOLLAR => "DOLLAR",
        kind::BANG => "BANG",
        kind::BACKTICK => "BACKTICK",
        kind::EQ => "EQ",
        kind::LT => "LT",
        kind::GT => "GT",
        kind::PLUS => "PLUS",
        kind::MINUS => "MINUS",
        kind::STAR => "STAR",
        kind::SLASH => "SLASH",
        kind::PERCENT => "PERCENT",
        kind::AMP => "AMP",
        kind::PIPE => "PIPE",
        kind::CARET => "CARET",
        kind::OPTIONAL_CHAIN => "OPTIONAL_CHAIN",
        kind::THIN_ARROW => "THIN_ARROW",
        kind::FAT_ARROW => "FAT_ARROW",
        kind::LT_COLON => "LT_COLON",
        kind::COLON_GT => "COLON_GT",
        kind::LT_PERCENT => "LT_PERCENT",
        kind::PERCENT_GT => "PERCENT_GT",
        kind::COLON_COLON => "COLON_COLON",
        kind::TEXT => "TEXT",
        kind::HEADING_MARKER => "HEADING_MARKER",
        kind::HEADING_TEXT => "HEADING_TEXT",
        kind::EMPHASIS => "EMPHASIS",
        kind::CODE_INLINE => "CODE_INLINE",
        kind::CODE_FENCE => "CODE_FENCE",
        kind::CODE_BLOCK => "CODE_BLOCK",
        kind::LINK_TEXT => "LINK_TEXT",
        kind::LINK_URL => "LINK_URL",
        kind::BLOCKQUOTE => "BLOCKQUOTE",
        kind::LIST_MARKER => "LIST_MARKER",
        kind::TAG_NAME => "TAG_NAME",
        kind::ATTR_NAME => "ATTR_NAME",
        kind::ENTITY_REF => "ENTITY_REF",
        kind::CDATA => "CDATA",
        kind::DOCTYPE => "DOCTYPE",
        kind::AT_KEYWORD => "AT_KEYWORD",
        kind::HASH_TOKEN => "HASH_TOKEN",
        _ => "<other>",
    }
}

fn escape_html(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(ch),
        }
    }
    out
}

fn escape_html_attr(text: &str) -> String {
    escape_html(text).replace('\n', "&#10;")
}
