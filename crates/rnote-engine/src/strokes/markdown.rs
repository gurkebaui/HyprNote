// Imports
use super::latex::{self, MathMetrics};
use super::textstroke::{TextAlignment, TextStyle};
use kurbo::{Point, Rect, RoundedRect};
use p2d::math::Vector2;
use piet::{FontFamily, FontStyle, FontWeight, RenderContext, Text, TextAttribute, TextLayout, TextLayoutBuilder};
use rnote_compose::Color;

#[derive(Debug, Clone, PartialEq)]
enum Block {
    Heading {
        level: usize,
        inlines: Vec<Inline>,
    },
    Paragraph {
        lines: Vec<Vec<Inline>>,
    },
    DisplayMath {
        latex: String,
    },
    CodeBlock {
        code: String,
    },
    Blockquote {
        lines: Vec<Vec<Inline>>,
    },
    ListItem {
        indent: usize,
        marker: String,
        inlines: Vec<Inline>,
    },
    HorizontalRule,
}

#[derive(Debug, Clone, PartialEq)]
enum Inline {
    Text {
        text: String,
        bold: bool,
        italic: bool,
        strike: bool,
        code: bool,
    },
    Math {
        latex: String,
    },
}

// --- Inline Parser ---

#[derive(Clone, Copy, Default)]
struct InlineFmt {
    bold: bool,
    italic: bool,
    strike: bool,
}

fn push_text(out: &mut Vec<Inline>, text: String, fmt: InlineFmt, code: bool) {
    if text.is_empty() {
        return;
    }
    if let Some(Inline::Text {
        text: prev,
        bold,
        italic,
        strike,
        code: prev_code,
    }) = out.last_mut()
        && *bold == fmt.bold
        && *italic == fmt.italic
        && *strike == fmt.strike
        && *prev_code == code
    {
        prev.push_str(&text);
        return;
    }
    out.push(Inline::Text {
        text,
        bold: fmt.bold,
        italic: fmt.italic,
        strike: fmt.strike,
        code,
    });
}

fn find_closing_delim(s: &str, delim: &str) -> Option<usize> {
    let mut i = 0;
    while i < s.len() {
        if s[i..].starts_with('\\') {
            let mut chars = s[i..].chars();
            chars.next();
            let step = 1 + chars.next().map(|c| c.len_utf8()).unwrap_or(0);
            i += step;
            continue;
        }
        if s[i..].starts_with(delim) {
            return Some(i);
        }
        let ch = s[i..].chars().next()?;
        i += ch.len_utf8();
    }
    None
}

fn find_closing_math(s: &str) -> Option<usize> {
    let mut i = 0;
    while i < s.len() {
        let ch = s[i..].chars().next()?;
        if ch == '\\' {
            let mut chars = s[i..].chars();
            chars.next();
            let step = 1 + chars.next().map(|c| c.len_utf8()).unwrap_or(0);
            i += step;
            continue;
        }
        if ch == '$' {
            return Some(i);
        }
        i += ch.len_utf8();
    }
    None
}

fn parse_inlines_with_fmt(input: &str, fmt: InlineFmt, out: &mut Vec<Inline>) {
    let mut i = 0;
    let mut buf = String::new();

    while i < input.len() {
        let rest = &input[i..];

        // Escape sequence
        if rest.starts_with('\\') && rest.len() > 1 {
            let mut chars = rest.chars();
            chars.next();
            if let Some(escaped) = chars.next() {
                if matches!(escaped, '\\' | '$' | '*' | '_' | '`' | '~' | '#' | '[' | ']') {
                    buf.push(escaped);
                    i += 1 + escaped.len_utf8();
                    continue;
                }
            }
        }

        // Inline math $...$
        if rest.starts_with('$') && !rest.starts_with("$$") {
            if let Some(end) = find_closing_math(&rest[1..])
                && end > 0
            {
                push_text(out, std::mem::take(&mut buf), fmt, false);
                out.push(Inline::Math {
                    latex: rest[1..1 + end].to_string(),
                });
                i += 1 + end + 1;
                continue;
            }
        }

        // Inline code `...`
        if rest.starts_with('`') {
            if let Some(end) = rest[1..].find('`') {
                push_text(out, std::mem::take(&mut buf), fmt, false);
                push_text(out, rest[1..1 + end].to_string(), fmt, true);
                i += 1 + end + 1;
                continue;
            }
        }

        // Bold + Italic ***...***
        if rest.starts_with("***") {
            if let Some(end) = find_closing_delim(&rest[3..], "***")
                && end > 0
            {
                push_text(out, std::mem::take(&mut buf), fmt, false);
                parse_inlines_with_fmt(
                    &rest[3..3 + end],
                    InlineFmt {
                        bold: true,
                        italic: true,
                        ..fmt
                    },
                    out,
                );
                i += 3 + end + 3;
                continue;
            }
        }

        // Bold **...** or __...__
        for delim in ["**", "__"] {
            if rest.starts_with(delim) {
                if let Some(end) = find_closing_delim(&rest[2..], delim)
                    && end > 0
                {
                    push_text(out, std::mem::take(&mut buf), fmt, false);
                    parse_inlines_with_fmt(
                        &rest[2..2 + end],
                        InlineFmt { bold: true, ..fmt },
                        out,
                    );
                    i += 2 + end + 2;
                    continue;
                }
            }
        }

        // Strikethrough ~~...~~
        if rest.starts_with("~~") {
            if let Some(end) = find_closing_delim(&rest[2..], "~~")
                && end > 0
            {
                push_text(out, std::mem::take(&mut buf), fmt, false);
                parse_inlines_with_fmt(
                    &rest[2..2 + end],
                    InlineFmt { strike: true, ..fmt },
                    out,
                );
                i += 2 + end + 2;
                continue;
            }
        }

        // Italic *...* or _..._
        if rest.starts_with('*') {
            if let Some(end) = find_closing_delim(&rest[1..], "*")
                && end > 0
                && !rest[1..1 + end].starts_with(' ')
            {
                push_text(out, std::mem::take(&mut buf), fmt, false);
                parse_inlines_with_fmt(
                    &rest[1..1 + end],
                    InlineFmt { italic: true, ..fmt },
                    out,
                );
                i += 1 + end + 1;
                continue;
            }
        }
        if rest.starts_with('_') {
            let prev_alnum = i > 0
                && input[..i]
                    .chars()
                    .next_back()
                    .map(|c| c.is_alphanumeric())
                    .unwrap_or(false);
            if !prev_alnum
                && let Some(end) = find_closing_delim(&rest[1..], "_")
                && end > 0
                && !rest[1..1 + end].starts_with(' ')
            {
                push_text(out, std::mem::take(&mut buf), fmt, false);
                parse_inlines_with_fmt(
                    &rest[1..1 + end],
                    InlineFmt { italic: true, ..fmt },
                    out,
                );
                i += 1 + end + 1;
                continue;
            }
        }

        let ch = rest.chars().next().unwrap();
        buf.push(ch);
        i += ch.len_utf8();
    }

    push_text(out, buf, fmt, false);
}

fn parse_inlines(input: &str) -> Vec<Inline> {
    let mut out = Vec::new();
    parse_inlines_with_fmt(input, InlineFmt::default(), &mut out);
    out
}

// --- Block Parser ---

fn is_horizontal_rule(line: &str) -> bool {
    let t = line.trim();
    if t.len() < 3 {
        return false;
    }
    for ch in ['-', '*', '_'] {
        if t.chars().all(|c| c == ch || c == ' ') && t.chars().filter(|&c| c == ch).count() >= 3 {
            return true;
        }
    }
    false
}

fn parse_heading(line: &str) -> Option<(usize, &str)> {
    let t = line.trim_start();
    let hashes = t.chars().take_while(|&c| c == '#').count();
    if (1..=6).contains(&hashes) {
        let rest = &t[hashes..];
        if rest.is_empty() || rest.starts_with(' ') || rest.starts_with('\t') {
            return Some((hashes, rest.trim()));
        }
    }
    None
}

fn parse_list_item(line: &str) -> Option<(usize, String, &str)> {
    let leading_spaces = line.chars().take_while(|&c| c == ' ').count();
    let indent = leading_spaces / 2;
    let t = &line[leading_spaces..];

    // Unordered list
    for prefix in ["- ", "* ", "+ "] {
        if let Some(rest) = t.strip_prefix(prefix) {
            if let Some(after) = rest.strip_prefix("[ ] ") {
                return Some((indent, "☐ ".to_string(), after));
            }
            if let Some(after) = rest
                .strip_prefix("[x] ")
                .or_else(|| rest.strip_prefix("[X] "))
            {
                return Some((indent, "☑ ".to_string(), after));
            }
            return Some((indent, "• ".to_string(), rest));
        }
    }

    // Ordered list: digits followed by ". "
    let digits = t.chars().take_while(|c| c.is_ascii_digit()).count();
    if digits > 0 && t[digits..].starts_with(". ") {
        let marker = format!("{}. ", &t[..digits]);
        let rest = &t[digits + 2..];
        return Some((indent, marker, rest));
    }

    None
}

fn parse_blocks(input: &str) -> Vec<Block> {
    let lines: Vec<&str> = input.lines().collect();
    let mut blocks = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i];
        let trimmed = line.trim();

        if trimmed.is_empty() {
            i += 1;
            continue;
        }

        // Fenced code block
        if trimmed.starts_with("```") {
            let mut code_lines = Vec::new();
            i += 1;
            while i < lines.len() && !lines[i].trim().starts_with("```") {
                code_lines.push(lines[i]);
                i += 1;
            }
            if i < lines.len() {
                i += 1;
            }
            blocks.push(Block::CodeBlock {
                code: code_lines.join("\n"),
            });
            continue;
        }

        // Display math block $$...$$
        if let Some(after_open) = trimmed.strip_prefix("$$") {
            if let Some(inner) = after_open.strip_suffix("$$")
                && !after_open.is_empty()
            {
                blocks.push(Block::DisplayMath {
                    latex: inner.trim().to_string(),
                });
                i += 1;
                continue;
            }
            let mut math_lines = Vec::new();
            if !after_open.trim().is_empty() {
                math_lines.push(after_open.trim());
            }
            i += 1;
            while i < lines.len() {
                let l_trim = lines[i].trim();
                if let Some(before_close) = l_trim.strip_suffix("$$") {
                    if !before_close.trim().is_empty() {
                        math_lines.push(before_close.trim());
                    }
                    i += 1;
                    break;
                }
                math_lines.push(lines[i]);
                i += 1;
            }
            blocks.push(Block::DisplayMath {
                latex: math_lines.join(" "),
            });
            continue;
        }

        // Heading
        if let Some((level, content)) = parse_heading(line) {
            blocks.push(Block::Heading {
                level,
                inlines: parse_inlines(content),
            });
            i += 1;
            continue;
        }

        // Horizontal rule
        if is_horizontal_rule(line) {
            blocks.push(Block::HorizontalRule);
            i += 1;
            continue;
        }

        // Blockquote
        if trimmed.starts_with('>') {
            let mut quote_lines = Vec::new();
            while i < lines.len() && lines[i].trim().starts_with('>') {
                let q = lines[i].trim().strip_prefix('>').unwrap_or("").trim_start();
                quote_lines.push(parse_inlines(q));
                i += 1;
            }
            blocks.push(Block::Blockquote { lines: quote_lines });
            continue;
        }

        // List item
        if let Some((indent, marker, rest)) = parse_list_item(line) {
            blocks.push(Block::ListItem {
                indent,
                marker,
                inlines: parse_inlines(rest),
            });
            i += 1;
            continue;
        }

        // Paragraph: collect consecutive plain lines, preserving single-Enter line breaks
        let mut para_lines = Vec::new();
        while i < lines.len() {
            let l = lines[i];
            let lt = l.trim();
            if lt.is_empty()
                || lt.starts_with("```")
                || lt.starts_with("$$")
                || lt.starts_with('>')
                || parse_heading(l).is_some()
                || is_horizontal_rule(l)
                || parse_list_item(l).is_some()
            {
                break;
            }
            para_lines.push(parse_inlines(lt));
            i += 1;
        }
        blocks.push(Block::Paragraph { lines: para_lines });
    }

    blocks
}

// --- Line Wrapping & Rendering ---

#[derive(Clone)]
enum Atom {
    Word {
        text: String,
        bold: bool,
        italic: bool,
        strike: bool,
        code: bool,
        width: f64,
        ascent: f64,
        descent: f64,
        is_space: bool,
    },
    Math {
        latex: String,
        metrics: MathMetrics,
    },
}

impl Atom {
    fn width(&self) -> f64 {
        match self {
            Atom::Word { width, .. } => *width,
            Atom::Math { metrics, .. } => metrics.width,
        }
    }

    fn ascent(&self) -> f64 {
        match self {
            Atom::Word { ascent, .. } => *ascent,
            Atom::Math { metrics, .. } => metrics.ascent,
        }
    }

    fn descent(&self) -> f64 {
        match self {
            Atom::Word { descent, .. } => *descent,
            Atom::Math { metrics, .. } => metrics.descent,
        }
    }

    fn is_space(&self) -> bool {
        matches!(self, Atom::Word { is_space: true, .. })
    }
}

struct WrappedLine {
    atoms: Vec<Atom>,
    width: f64,
    ascent: f64,
    descent: f64,
}

fn build_run_layout<T: Text>(
    piet_text: &mut T,
    text: &str,
    font_size: f64,
    bold: bool,
    italic: bool,
    strike: bool,
    code: bool,
    style: &TextStyle,
) -> Option<T::TextLayout> {
    let family = if code {
        piet_text
            .font_family("monospace")
            .unwrap_or(FontFamily::MONOSPACE)
    } else {
        piet_text
            .font_family(&style.font_family)
            .unwrap_or(FontFamily::SERIF)
    };
    let effective_size = if code { font_size * 0.88 } else { font_size };
    let weight = if bold {
        FontWeight::BOLD
    } else {
        FontWeight::new(style.font_weight)
    };
    let font_style = if italic {
        FontStyle::Italic
    } else {
        style.font_style.into()
    };

    let mut builder = piet_text
        .new_text_layout(text.to_string())
        .font(family, effective_size)
        .default_attribute(TextAttribute::Weight(weight))
        .default_attribute(TextAttribute::Style(font_style))
        .text_color(piet::Color::from(style.color));

    if strike {
        builder = builder.default_attribute(TextAttribute::Strikethrough(true));
    }

    builder.build().ok()
}

fn measure_word<T: Text>(
    piet_text: &mut T,
    word: &str,
    font_size: f64,
    bold: bool,
    italic: bool,
    strike: bool,
    code: bool,
    style: &TextStyle,
) -> (f64, f64, f64) {
    if let Some(layout) =
        build_run_layout(piet_text, word, font_size, bold, italic, strike, code, style)
    {
        let size = layout.size();
        let pad = if code { font_size * 0.22 } else { 0.0 };
        let mut w = size.width.max(layout.trailing_whitespace_width());
        if w <= 0.0 && word.chars().all(char::is_whitespace) {
            w = word.chars().count() as f64 * font_size * 0.28;
        }
        if let Some(lm) = layout.line_metric(0) {
            return (
                w + pad,
                lm.baseline.max(font_size * 0.7),
                (lm.height - lm.baseline).max(font_size * 0.2),
            );
        }
        return (w + pad, size.height * 0.8, size.height * 0.2);
    }
    (
        word.chars().count() as f64 * font_size * 0.5,
        font_size * 0.8,
        font_size * 0.2,
    )
}

fn tokenize_inlines<T: Text>(
    piet_text: &mut T,
    inlines: &[Inline],
    font_size: f64,
    force_bold: bool,
    style: &TextStyle,
) -> Vec<Atom> {
    let mut atoms = Vec::new();
    for inline in inlines {
        match inline {
            Inline::Math { latex } => {
                let metrics =
                    latex::measure_latex(piet_text, latex, font_size, false, &style.font_family);
                atoms.push(Atom::Math {
                    latex: latex.clone(),
                    metrics,
                });
            }
            Inline::Text {
                text,
                bold,
                italic,
                strike,
                code,
            } => {
                let bold = *bold || force_bold;
                if *code {
                    let (w, a, d) =
                        measure_word(piet_text, text, font_size, bold, *italic, *strike, true, style);
                    atoms.push(Atom::Word {
                        text: text.clone(),
                        bold,
                        italic: *italic,
                        strike: *strike,
                        code: true,
                        width: w,
                        ascent: a,
                        descent: d,
                        is_space: false,
                    });
                    continue;
                }

                // Split into alternating non-whitespace words and whitespace runs
                let mut cur = String::new();
                let mut cur_space = None;
                for ch in text.chars() {
                    let sp = ch.is_whitespace();
                    if cur_space.is_some() && cur_space != Some(sp) {
                        let tok = std::mem::take(&mut cur);
                        let (w, a, d) = measure_word(
                            piet_text, &tok, font_size, bold, *italic, *strike, false, style,
                        );
                        atoms.push(Atom::Word {
                            text: tok,
                            bold,
                            italic: *italic,
                            strike: *strike,
                            code: false,
                            width: w,
                            ascent: a,
                            descent: d,
                            is_space: cur_space.unwrap(),
                        });
                    }
                    cur_space = Some(sp);
                    cur.push(ch);
                }
                if !cur.is_empty() {
                    let (w, a, d) = measure_word(
                        piet_text, &cur, font_size, bold, *italic, *strike, false, style,
                    );
                    atoms.push(Atom::Word {
                        text: cur,
                        bold,
                        italic: *italic,
                        strike: *strike,
                        code: false,
                        width: w,
                        ascent: a,
                        descent: d,
                        is_space: cur_space.unwrap_or(false),
                    });
                }
            }
        }
    }
    atoms
}

fn wrap_atoms(atoms: Vec<Atom>, max_width: f64, default_font_size: f64) -> Vec<WrappedLine> {
    let mut lines = Vec::new();
    let mut cur_atoms: Vec<Atom> = Vec::new();
    let mut cur_w = 0.0;

    for atom in atoms {
        if cur_atoms.is_empty() && atom.is_space() {
            continue;
        }
        let aw = atom.width();
        if !cur_atoms.is_empty() && !atom.is_space() && cur_w + aw > max_width {
            while cur_atoms.last().map(|a| a.is_space()).unwrap_or(false) {
                let removed = cur_atoms.pop().unwrap();
                cur_w -= removed.width();
            }
            let ascent = cur_atoms
                .iter()
                .map(|a| a.ascent())
                .fold(default_font_size * 0.75, f64::max);
            let descent = cur_atoms
                .iter()
                .map(|a| a.descent())
                .fold(default_font_size * 0.25, f64::max);
            lines.push(WrappedLine {
                atoms: std::mem::take(&mut cur_atoms),
                width: cur_w,
                ascent,
                descent,
            });
            cur_w = 0.0;
        }
        cur_w += aw;
        cur_atoms.push(atom);
    }

    while cur_atoms.last().map(|a| a.is_space()).unwrap_or(false) {
        let removed = cur_atoms.pop().unwrap();
        cur_w -= removed.width();
    }

    if !cur_atoms.is_empty() || lines.is_empty() {
        let ascent = cur_atoms
            .iter()
            .map(|a| a.ascent())
            .fold(default_font_size * 0.75, f64::max);
        let descent = cur_atoms
            .iter()
            .map(|a| a.descent())
            .fold(default_font_size * 0.25, f64::max);
        lines.push(WrappedLine {
            atoms: cur_atoms,
            width: cur_w,
            ascent,
            descent,
        });
    }

    lines
}

fn draw_wrapped_lines(
    cx: &mut impl RenderContext,
    lines: &[WrappedLine],
    start_x: f64,
    mut y: f64,
    avail_w: f64,
    font_size: f64,
    style: &TextStyle,
) -> (f64, f64) {
    let mut max_w = 0.0_f64;
    let line_gap = font_size * 0.22;

    for (idx, line) in lines.iter().enumerate() {
        let baseline_y = y + line.ascent;
        let offset_x = if avail_w.is_finite() {
            match style.alignment {
                TextAlignment::Start | TextAlignment::Fill => 0.0,
                TextAlignment::Center => ((avail_w - line.width) * 0.5).max(0.0),
                TextAlignment::End => (avail_w - line.width).max(0.0),
            }
        } else {
            0.0
        };
        let mut cur_x = start_x + offset_x;

        for atom in &line.atoms {
            match atom {
                Atom::Math { latex, metrics } => {
                    latex::draw_latex(
                        cx,
                        latex,
                        cur_x,
                        baseline_y,
                        font_size,
                        false,
                        style.color,
                        &style.font_family,
                    );
                    cur_x += metrics.width;
                }
                Atom::Word {
                    text,
                    bold,
                    italic,
                    strike,
                    code,
                    width,
                    ascent,
                    descent,
                    ..
                } => {
                    if *code {
                        let bg_color = piet::Color::rgba8(128, 128, 128, 38);
                        let rect = RoundedRect::new(
                            cur_x,
                            baseline_y - ascent * 0.9,
                            cur_x + *width,
                            baseline_y + descent * 0.75,
                            font_size * 0.16,
                        );
                        cx.fill(rect, &bg_color);
                    }
                    if let Some(layout) = build_run_layout(
                        cx.text(),
                        text,
                        font_size,
                        *bold,
                        *italic,
                        *strike,
                        *code,
                        style,
                    ) {
                        let pad = if *code { font_size * 0.11 } else { 0.0 };
                        let lm_base = layout
                            .line_metric(0)
                            .map(|lm| lm.baseline)
                            .unwrap_or(*ascent);
                        cx.draw_text(&layout, Point::new(cur_x + pad, baseline_y - lm_base));
                    }
                    cur_x += *width;
                }
            }
        }

        max_w = max_w.max(line.width);
        y += line.ascent + line.descent;
        if idx + 1 < lines.len() {
            y += line_gap;
        }
    }

    (max_w, y)
}

fn measure_wrapped_lines(lines: &[WrappedLine], font_size: f64) -> (f64, f64) {
    let mut max_w = 0.0_f64;
    let mut total_h = 0.0_f64;
    let line_gap = font_size * 0.22;
    for (idx, line) in lines.iter().enumerate() {
        max_w = max_w.max(line.width);
        total_h += line.ascent + line.descent;
        if idx + 1 < lines.len() {
            total_h += line_gap;
        }
    }
    (max_w, total_h)
}

fn heading_scale(level: usize) -> f64 {
    match level {
        1 => 1.75,
        2 => 1.45,
        3 => 1.22,
        4 => 1.1,
        5 => 1.0,
        _ => 0.95,
    }
}

pub fn measure_markdown<T: Text>(piet_text: &mut T, text: &str, style: &TextStyle) -> Vector2 {
    let blocks = parse_blocks(text);
    let max_w_limit = style.max_width().unwrap_or(f64::INFINITY);
    let fs = style.font_size;
    let block_gap = fs * 0.45;
    let mut total_w = 0.0_f64;
    let mut total_h = 0.0_f64;

    for (idx, block) in blocks.iter().enumerate() {
        match block {
            Block::Heading { level, inlines } => {
                let h_fs = fs * heading_scale(*level);
                let atoms = tokenize_inlines(piet_text, inlines, h_fs, true, style);
                let lines = wrap_atoms(atoms, max_w_limit, h_fs);
                let (w, h) = measure_wrapped_lines(&lines, h_fs);
                total_w = total_w.max(w);
                total_h += h + if *level <= 2 { fs * 0.2 } else { 0.0 };
            }
            Block::Paragraph { lines: para_lines } => {
                let line_gap = fs * 0.22;
                for (li, l_inlines) in para_lines.iter().enumerate() {
                    let atoms = tokenize_inlines(piet_text, l_inlines, fs, false, style);
                    let wrapped = wrap_atoms(atoms, max_w_limit, fs);
                    let (w, h) = measure_wrapped_lines(&wrapped, fs);
                    total_w = total_w.max(w);
                    total_h += h;
                    if li + 1 < para_lines.len() {
                        total_h += line_gap;
                    }
                }
            }
            Block::DisplayMath { latex } => {
                let m = latex::measure_latex(piet_text, latex, fs * 1.15, true, &style.font_family);
                total_w = total_w.max(m.width);
                total_h += m.height() + fs * 0.2;
            }
            Block::CodeBlock { code } => {
                let c_fs = fs * 0.85;
                let pad = fs * 0.4;
                if let Some(layout) =
                    build_run_layout(piet_text, code, c_fs, false, false, false, true, style)
                {
                    let sz = layout.size();
                    total_w = total_w.max(sz.width + 2.0 * pad);
                    total_h += sz.height + 2.0 * pad;
                }
            }
            Block::Blockquote { lines } => {
                let indent = fs * 0.85;
                let avail = (max_w_limit - indent).max(fs * 2.0);
                let mut q_h = 0.0;
                for (li, l_inlines) in lines.iter().enumerate() {
                    let atoms = tokenize_inlines(piet_text, l_inlines, fs, false, style);
                    let wrapped = wrap_atoms(atoms, avail, fs);
                    let (w, h) = measure_wrapped_lines(&wrapped, fs);
                    total_w = total_w.max(indent + w);
                    q_h += h + if li + 1 < lines.len() { fs * 0.2 } else { 0.0 };
                }
                total_h += q_h;
            }
            Block::ListItem {
                indent,
                marker,
                inlines,
            } => {
                let left = (*indent as f64) * fs * 1.2;
                let (mw, _, _) =
                    measure_word(piet_text, marker, fs, false, false, false, false, style);
                let avail = (max_w_limit - left - mw).max(fs * 2.0);
                let atoms = tokenize_inlines(piet_text, inlines, fs, false, style);
                let lines = wrap_atoms(atoms, avail, fs);
                let (w, h) = measure_wrapped_lines(&lines, fs);
                total_w = total_w.max(left + mw + w);
                total_h += h;
            }
            Block::HorizontalRule => {
                let w = if max_w_limit.is_finite() {
                    max_w_limit
                } else {
                    fs * 12.0
                };
                total_w = total_w.max(w);
                total_h += fs * 0.5;
            }
        }
        if idx + 1 < blocks.len() {
            total_h += block_gap;
        }
    }

    Vector2::new(total_w.max(1.0), total_h.max(1.0))
}

pub fn draw_markdown(
    cx: &mut impl RenderContext,
    text: &str,
    style: &TextStyle,
) -> anyhow::Result<()> {
    let blocks = parse_blocks(text);
    let max_w_limit = style.max_width().unwrap_or(f64::INFINITY);
    let measured = measure_markdown(cx.text(), text, style);
    let box_w = if max_w_limit.is_finite() {
        max_w_limit
    } else {
        measured.x
    };
    let fs = style.font_size;
    let block_gap = fs * 0.45;
    let mut y = 0.0;

    for (idx, block) in blocks.iter().enumerate() {
        match block {
            Block::Heading { level, inlines } => {
                let h_fs = fs * heading_scale(*level);
                let atoms = tokenize_inlines(cx.text(), inlines, h_fs, true, style);
                let lines = wrap_atoms(atoms, max_w_limit, h_fs);
                let (_w, next_y) = draw_wrapped_lines(cx, &lines, 0.0, y, box_w, h_fs, style);
                y = next_y;
                if *level <= 2 {
                    let rule_y = y + fs * 0.08;
                    let rule_color = piet::Color::from(Color {
                        a: (style.color.a * 0.25).clamp(0.0, 1.0),
                        ..style.color
                    });
                    cx.stroke(
                        kurbo::Line::new((0.0, rule_y), (box_w, rule_y)),
                        &rule_color,
                        (fs * 0.04).max(1.0),
                    );
                    y += fs * 0.2;
                }
            }
            Block::Paragraph { lines: para_lines } => {
                let line_gap = fs * 0.22;
                for (li, l_inlines) in para_lines.iter().enumerate() {
                    let atoms = tokenize_inlines(cx.text(), l_inlines, fs, false, style);
                    let wrapped = wrap_atoms(atoms, max_w_limit, fs);
                    let (_w, next_y) = draw_wrapped_lines(cx, &wrapped, 0.0, y, box_w, fs, style);
                    y = next_y;
                    if li + 1 < para_lines.len() {
                        y += line_gap;
                    }
                }
            }
            Block::DisplayMath { latex } => {
                let m_fs = fs * 1.15;
                let m = latex::measure_latex(cx.text(), latex, m_fs, true, &style.font_family);
                let x = ((box_w - m.width) * 0.5).max(0.0);
                let baseline_y = y + fs * 0.1 + m.ascent;
                latex::draw_latex(
                    cx,
                    latex,
                    x,
                    baseline_y,
                    m_fs,
                    true,
                    style.color,
                    &style.font_family,
                );
                y += m.height() + fs * 0.2;
            }
            Block::CodeBlock { code } => {
                let c_fs = fs * 0.85;
                let pad = fs * 0.4;
                if let Some(layout) =
                    build_run_layout(cx.text(), code, c_fs, false, false, false, true, style)
                {
                    let sz = layout.size();
                    let w = sz.width + 2.0 * pad;
                    let h = sz.height + 2.0 * pad;
                    let bg = piet::Color::rgba8(128, 128, 128, 28);
                    let border = piet::Color::rgba8(128, 128, 128, 70);
                    let rr = RoundedRect::new(0.0, y, w.max(box_w.min(sz.width + 2.0 * pad)), y + h, fs * 0.2);
                    cx.fill(rr, &bg);
                    cx.stroke(rr, &border, 1.0);
                    cx.draw_text(&layout, Point::new(pad, y + pad));
                    y += h;
                }
            }
            Block::Blockquote { lines } => {
                let indent = fs * 0.85;
                let avail = (max_w_limit - indent).max(fs * 2.0);
                let start_y = y;
                for (li, l_inlines) in lines.iter().enumerate() {
                    let atoms = tokenize_inlines(cx.text(), l_inlines, fs, false, style);
                    let wrapped = wrap_atoms(atoms, avail, fs);
                    let (_w, next_y) = draw_wrapped_lines(
                        cx,
                        &wrapped,
                        indent,
                        y,
                        (box_w - indent).max(1.0),
                        fs,
                        style,
                    );
                    y = next_y;
                    if li + 1 < lines.len() {
                        y += fs * 0.2;
                    }
                }
                let bar_color = piet::Color::rgba8(120, 120, 140, 140);
                cx.fill(
                    Rect::new(fs * 0.15, start_y, fs * 0.32, y),
                    &bar_color,
                );
            }
            Block::ListItem {
                indent,
                marker,
                inlines,
            } => {
                let left = (*indent as f64) * fs * 1.2;
                let (mw, ma, _md) =
                    measure_word(cx.text(), marker, fs, false, false, false, false, style);
                if let Some(m_layout) =
                    build_run_layout(cx.text(), marker, fs, false, false, false, false, style)
                {
                    let base = m_layout.line_metric(0).map(|lm| lm.baseline).unwrap_or(ma);
                    let atoms = tokenize_inlines(cx.text(), inlines, fs, false, style);
                    let avail = (max_w_limit - left - mw).max(fs * 2.0);
                    let lines = wrap_atoms(atoms, avail, fs);
                    let first_ascent = lines.first().map(|l| l.ascent).unwrap_or(ma);
                    cx.draw_text(&m_layout, Point::new(left, y + first_ascent - base));
                    let (_w, next_y) = draw_wrapped_lines(
                        cx,
                        &lines,
                        left + mw,
                        y,
                        (box_w - left - mw).max(1.0),
                        fs,
                        style,
                    );
                    y = next_y;
                }
            }
            Block::HorizontalRule => {
                let rule_y = y + fs * 0.25;
                let rule_color = piet::Color::rgba8(128, 128, 128, 110);
                cx.stroke(
                    kurbo::Line::new((0.0, rule_y), (box_w, rule_y)),
                    &rule_color,
                    (fs * 0.05).max(1.2),
                );
                y += fs * 0.5;
            }
        }

        if idx + 1 < blocks.len() {
            y += block_gap;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_inline_formatting_and_math() {
        let inlines = parse_inlines("Hallo **fett** und *kursiv* mit $x^2 + \\alpha$ und `code`");
        assert!(inlines.iter().any(|i| matches!(
            i,
            Inline::Text {
                text,
                bold: true,
                ..
            } if text == "fett"
        )));
        assert!(inlines.iter().any(|i| matches!(
            i,
            Inline::Text {
                text,
                italic: true,
                ..
            } if text == "kursiv"
        )));
        assert!(inlines.iter().any(|i| matches!(
            i,
            Inline::Math { latex } if latex == "x^2 + \\alpha"
        )));
        assert!(inlines.iter().any(|i| matches!(
            i,
            Inline::Text {
                text,
                code: true,
                ..
            } if text == "code"
        )));
    }

    #[test]
    fn parse_escaped_dollar_and_utf8() {
        let inlines = parse_inlines("Preis: \\$100 für Café & Überlegung");
        assert_eq!(inlines.len(), 1);
        match &inlines[0] {
            Inline::Text { text, .. } => assert_eq!(text, "Preis: $100 für Café & Überlegung"),
            other => panic!("Expected plain text, got {other:?}"),
        }
    }

    #[test]
    fn parse_blocks_all_types() {
        let md = "\
# Überschrift

Ein Absatz mit **Text** und $a^2$.

$$\\int_0^1 x^2 dx = \\frac{1}{3}$$

- Punkt 1
  - [x] Erledigt
- [ ] Offen

> Zitat

```
let x = 1;
```

---
";
        let blocks = parse_blocks(md);
        assert!(matches!(blocks[0], Block::Heading { level: 1, .. }));
        assert!(matches!(blocks[1], Block::Paragraph { .. }));
        assert!(matches!(blocks[2], Block::DisplayMath { .. }));
        assert!(matches!(blocks[3], Block::ListItem { indent: 0, .. }));
        assert!(matches!(blocks[4], Block::ListItem { indent: 1, .. }));
        assert!(matches!(blocks[5], Block::ListItem { indent: 0, .. }));
        assert!(matches!(blocks[6], Block::Blockquote { .. }));
        assert!(matches!(blocks[7], Block::CodeBlock { .. }));
        assert!(matches!(blocks[8], Block::HorizontalRule));
    }

    #[test]
    fn measure_markdown_returns_positive_size() {
        let mut text = piet_cairo::CairoText::new();
        let mut style = TextStyle::default();
        style.set_max_width(Some(500.0));
        let size = measure_markdown(
            &mut text,
            "# Mathe\n\nSatz des Pythagoras: $a^2 + b^2 = c^2$\n\n$$\\frac{-b \\pm \\sqrt{b^2 - 4ac}}{2a}$$",
            &style,
        );
        assert!(size.x > 50.0);
        assert!(size.y > 50.0);
    }

    #[test]
    fn single_newline_preserves_line_break() {
        let blocks = parse_blocks("Zeile 1\nZeile 2\nZeile 3");
        match &blocks[0] {
            Block::Paragraph { lines } => assert_eq!(lines.len(), 3),
            other => panic!("Expected Paragraph, got {other:?}"),
        }

        let mut text = piet_cairo::CairoText::new();
        let style = TextStyle::default();
        let single = measure_markdown(&mut text, "Zeile 1", &style);
        let multi = measure_markdown(&mut text, "Zeile 1\nZeile 2\nZeile 3", &style);
        assert!(multi.y > 2.0 * single.y);
    }

    #[test]
    fn space_between_math_and_text_has_width() {
        let mut text = piet_cairo::CairoText::new();
        let style = TextStyle::default();
        let no_space = measure_markdown(&mut text, "$f(x)$f", &style);
        let with_space = measure_markdown(&mut text, "$f(x)$ f", &style);
        assert!(with_space.x > no_space.x);
    }
}
