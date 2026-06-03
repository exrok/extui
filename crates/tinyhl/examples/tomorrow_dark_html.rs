use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::io::{self, Read};

use tinyhl::{
    DelimiterTable, Language, SemanticKind, SemanticTable, Source, Span, Token, TokenTable,
};

#[derive(Clone, Copy)]
struct SpanInfo {
    semantic: Option<SemanticKind>,
    delimiter: Option<(u16, bool)>,
}

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
    let tokens = TokenTable::new(language, source);
    let semantic = SemanticTable::new(&tokens, source);
    let delimiters = DelimiterTable::new(&tokens);

    let semantic_by_offset: BTreeMap<u32, SemanticKind> = semantic
        .query(Span::new(0, semantic.source_len()))
        .map(|token| (token.span.offset, token.kind))
        .collect();
    let delimiter_by_offset: BTreeMap<u32, (u16, bool)> = delimiters
        .query(Span::new(0, delimiters.source_len()))
        .map(|token| (token.span.offset, (token.depth, token.is_open)))
        .collect();

    let mut html = String::new();
    html.push_str(
        r#"<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>TinyHL Preview</title>
  <style>
    :root {
      --bg: #1d1f21;
      --fg: #c5c8c6;
      --gutter: #969896;
      --comment: #969896;
      --red: #cc6666;
      --orange: #de935f;
      --yellow: #f0c674;
      --green: #b5bd68;
      --aqua: #8abeb7;
      --blue: #81a2be;
      --purple: #b294bb;
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
      background:
        radial-gradient(circle at top left, rgba(129,162,190,0.16), transparent 28rem),
        radial-gradient(circle at bottom right, rgba(178,148,187,0.12), transparent 24rem),
        var(--bg);
    }
    .frame {
      max-width: 1100px;
      margin: 0 auto;
      border: 1px solid #282a2e;
      background: #151618;
      box-shadow: 0 24px 60px rgba(0,0,0,0.35);
      overflow: hidden;
    }
    .inspector {
      align-self: start;
      position: sticky;
      top: 24px;
      border: 1px solid #282a2e;
      background: rgba(21,22,24,0.94);
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
    .toolbar .dot:nth-child(1) { background: var(--red); }
    .toolbar .dot:nth-child(2) { background: var(--yellow); }
    .toolbar .dot:nth-child(3) { background: var(--green); }
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
    .tok-comment { color: var(--comment); font-style: italic; }
    .tok-string { color: var(--green); }
    .tok-number { color: var(--orange); }
    .tok-keyword { color: var(--purple); }
    .tok-error { color: var(--red); background: rgba(204,102,102,0.16); }
    .tok-punct { color: #c5c8c6; }
    .tok-markup { color: var(--blue); }
    .tok-entity { color: var(--orange); }
    .tok-heading { color: var(--blue); font-weight: 700; }
    .tok-link { color: var(--aqua); text-decoration: underline; text-decoration-color: rgba(138,190,183,0.45); }
    .sem-type { color: var(--yellow); }
    .sem-fn { color: var(--blue); }
    .sem-param { color: #d7ba7d; }
    .sem-arg { color: #e5c07b; }
    .sem-var-def { color: #cc6666; }
    .sem-field { color: var(--aqua); }
    .sem-path { color: #d19a66; }
    .rainbow-0 { color: var(--purple); font-weight: 700; }
    .rainbow-1 { color: var(--blue); font-weight: 700; }
    .rainbow-2 { color: var(--green); font-weight: 700; }
    .rainbow-3 { color: var(--orange); font-weight: 700; }
    .rainbow-4 { color: var(--aqua); font-weight: 700; }
    .rainbow-5 { color: var(--red); font-weight: 700; }
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
        "{language:?} - TinyHL Tomorrow Dark Preview"
    )));
    html.push_str(
        r#"
      </div>
      <pre><code id="code-view">"#,
    );

    for token in tokens.query(Span::new(0, tokens.source_len())) {
        let text = &src[token.span.offset as usize..token.span.end() as usize];
        if is_whitespace(token) {
            html.push_str(&escape_html(text));
            continue;
        }

        let info = SpanInfo {
            semantic: semantic_by_offset.get(&token.span.offset).copied(),
            delimiter: delimiter_by_offset.get(&token.span.offset).copied(),
        };
        let mut classes = Vec::new();
        classes.push("token");
        classes.push(base_class(token));
        if let Some(kind) = info.semantic {
            if let Some(class) = semantic_class(kind) {
                classes.push(class);
            }
        }
        if let Some((depth, _is_open)) = info.delimiter {
            classes.push(rainbow_class(depth));
        }

        let explanation = explanation_lines(token, info);
        html.push_str(r#"<span tabindex="0" class=""#);
        html.push_str(classes.join(" ").as_str());
        html.push_str(r#"" data-text=""#);
        html.push_str(&escape_html_attr(text));
        html.push_str(r#"" data-language=""#);
        html.push_str(token.language().tag().to_string().as_str());
        html.push_str(r#"" data-language-name=""#);
        html.push_str(&escape_html_attr(&format!("{:?}", token.language())));
        html.push_str(r#"" data-offset=""#);
        html.push_str(token.span.offset.to_string().as_str());
        html.push_str(r#"" data-len=""#);
        html.push_str(token.span.len.to_string().as_str());
        html.push_str(r#"" data-base=""#);
        html.push_str(&escape_html_attr(base_label(token)));
        html.push_str(r#"" data-local-kind=""#);
        html.push_str(&escape_html_attr(local_kind_name(token)));
        html.push_str(r#"" data-semantic=""#);
        html.push_str(&escape_html_attr(
            &info
                .semantic
                .map(semantic_kind_name)
                .unwrap_or("none")
                .to_string(),
        ));
        html.push_str(r#"" data-delimiter=""#);
        html.push_str(&escape_html_attr(&delimiter_label(info.delimiter)));
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
          <div class="panel-label">Lexical Class</div>
          <div class="panel-value" id="inspector-base">-</div>
        </div>
        <div class="panel-row">
          <div class="panel-label">Semantic Class</div>
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
        base.textContent = `${node.dataset.base} via lexical kind ${node.dataset.localKind}`;
        semantic.textContent = node.dataset.semantic === 'none' ? 'none' : node.dataset.semantic;
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
        other => Err(format!("unsupported language: {other}")),
    }
}

fn is_whitespace(token: Token) -> bool {
    token.local_kind() == tinyhl::kind::WHITESPACE
}

fn base_class(token: Token) -> &'static str {
    use tinyhl::kind;
    match token.local_kind() {
        kind::STRING | kind::TEMPLATE_STRING | kind::REGEX | kind::CHAR => "tok-string",
        kind::NUMBER => "tok-number",
        kind::KEYWORD => "tok-keyword",
        kind::COMMENT | kind::DOC_COMMENT => "tok-comment",
        kind::ERROR => "tok-error",
        kind::TAG_NAME | kind::ATTR_NAME => "tok-markup",
        kind::ENTITY_REF => "tok-entity",
        kind::CDATA => "tok-string",
        kind::DOCTYPE => "tok-keyword",
        kind::HEADING_MARKER | kind::HEADING_TEXT => "tok-heading",
        kind::LINK_URL | kind::LINK_TEXT => "tok-link",
        kind::CODE_INLINE | kind::CODE_FENCE | kind::CODE_BLOCK => "tok-string",
        _ => "tok-punct",
    }
}

fn base_label(token: Token) -> &'static str {
    match base_class(token) {
        "tok-comment" => "comment",
        "tok-string" => "string",
        "tok-number" => "number",
        "tok-keyword" => "keyword",
        "tok-error" => "error",
        "tok-markup" => "markup name",
        "tok-entity" => "entity reference",
        "tok-heading" => "heading",
        "tok-link" => "link",
        _ => "punctuation / plain token",
    }
}

fn semantic_class(kind: SemanticKind) -> Option<&'static str> {
    match kind {
        SemanticKind::TypeDefinition | SemanticKind::TypeName => Some("sem-type"),
        SemanticKind::FunctionDefinition
        | SemanticKind::FunctionCall
        | SemanticKind::MethodDefinition
        | SemanticKind::MethodCall => Some("sem-fn"),
        SemanticKind::Parameter => Some("sem-param"),
        SemanticKind::Argument => Some("sem-arg"),
        SemanticKind::VariableDefinition => Some("sem-var-def"),
        SemanticKind::FieldDefinition | SemanticKind::Field => Some("sem-field"),
        SemanticKind::PathComponent => Some("sem-path"),
        SemanticKind::Variable => None,
    }
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
        SemanticKind::PathComponent => "path-component",
    }
}

fn delimiter_label(info: Option<(u16, bool)>) -> String {
    match info {
        Some((depth, true)) => format!("opening delimiter, rainbow depth {depth}"),
        Some((depth, false)) => format!("closing delimiter, rainbow depth {depth}"),
        None => "none".to_string(),
    }
}

fn rainbow_class(depth: u16) -> &'static str {
    match depth % 6 {
        0 => "rainbow-0",
        1 => "rainbow-1",
        2 => "rainbow-2",
        3 => "rainbow-3",
        4 => "rainbow-4",
        _ => "rainbow-5",
    }
}

fn explanation_lines(token: Token, info: SpanInfo) -> Vec<String> {
    let mut lines = Vec::new();
    lines.push(format!(
        "Base color comes from lexical class '{}' mapped from token kind '{}'.",
        base_label(token),
        local_kind_name(token)
    ));

    if let Some(kind) = info.semantic {
        lines.push(format!(
            "Semantic overlay marks this token as '{}', which refines the base lexical color.",
            semantic_kind_name(kind)
        ));
    } else {
        lines.push(
            "No semantic overlay applies here, so the lexical class drives the color.".to_string(),
        );
    }

    if let Some((depth, is_open)) = info.delimiter {
        let direction = if is_open { "opening" } else { "closing" };
        lines.push(format!(
            "Rainbow delimiter coloring is active because this token is a {direction} delimiter at nesting depth {depth}."
        ));
    }

    lines.push(format!(
        "Rendered in language {:?} at byte span [{}..{}).",
        token.language(),
        token.span.offset,
        token.span.end()
    ));
    lines
}

fn local_kind_name(token: Token) -> &'static str {
    use tinyhl::kind;
    match token.local_kind() {
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
