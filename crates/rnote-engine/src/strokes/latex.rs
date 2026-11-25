// Imports
use kurbo::{BezPath, Point};
use piet::{FontFamily, FontStyle, FontWeight, RenderContext, Text, TextAttribute, TextLayout, TextLayoutBuilder};
use rnote_compose::Color;

/// Metrics of a laid-out LaTeX math expression relative to its baseline.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MathMetrics {
    pub width: f64,
    pub ascent: f64,
    pub descent: f64,
}

impl MathMetrics {
    pub fn height(&self) -> f64 {
        self.ascent + self.descent
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccentKind {
    Vec,
    Hat,
    Bar,
    Underline,
    Dot,
    Ddot,
    Tilde,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathStyleKind {
    Text,
    Roman,
    Bold,
    Italic,
    Sans,
    Mono,
    Blackboard,
    Calligraphic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatrixDelim {
    None,
    Paren,
    Bracket,
    Brace,
    Pipe,
    Cases,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MathNode {
    Empty,
    Row(Vec<MathNode>),
    Ident(String),
    Number(String),
    Text(String),
    Operator(String),
    Space(f64),
    Frac {
        num: Box<MathNode>,
        den: Box<MathNode>,
    },
    Binom {
        top: Box<MathNode>,
        bottom: Box<MathNode>,
    },
    Sqrt {
        index: Option<Box<MathNode>>,
        body: Box<MathNode>,
    },
    Scripts {
        base: Box<MathNode>,
        sub: Option<Box<MathNode>>,
        sup: Option<Box<MathNode>>,
    },
    BigOp {
        symbol: String,
        lower: Option<Box<MathNode>>,
        upper: Option<Box<MathNode>>,
        limits_above_below: bool,
    },
    Delimited {
        left: String,
        right: String,
        body: Box<MathNode>,
    },
    Accent {
        kind: AccentKind,
        body: Box<MathNode>,
    },
    Styled {
        kind: MathStyleKind,
        body: Box<MathNode>,
    },
    Matrix {
        delim: MatrixDelim,
        rows: Vec<Vec<MathNode>>,
    },
}

// --- Parser ---

struct Parser<'a> {
    src: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Self {
        Self { src, pos: 0 }
    }

    fn rest(&self) -> &'a str {
        &self.src[self.pos..]
    }

    fn peek(&self) -> Option<char> {
        self.rest().chars().next()
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.pos += ch.len_utf8();
            } else {
                break;
            }
        }
    }

    fn starts_with(&self, pat: &str) -> bool {
        self.rest().starts_with(pat)
    }

    fn parse_row(&mut self, stop_tokens: &[&str]) -> MathNode {
        let mut nodes = Vec::new();
        loop {
            self.skip_whitespace();
            if self.pos >= self.src.len() {
                break;
            }
            if stop_tokens.iter().any(|tok| self.starts_with(tok)) {
                break;
            }
            if self.peek() == Some('}') {
                break;
            }
            nodes.push(self.parse_atom_with_scripts());
        }
        match nodes.len() {
            0 => MathNode::Empty,
            1 => nodes.into_iter().next().unwrap(),
            _ => MathNode::Row(nodes),
        }
    }

    fn parse_group_or_atom(&mut self) -> MathNode {
        self.skip_whitespace();
        if self.peek() == Some('{') {
            self.next_char();
            let inner = self.parse_row(&[]);
            if self.peek() == Some('}') {
                self.next_char();
            }
            inner
        } else {
            self.parse_atom()
        }
    }

    fn parse_bracket_optional(&mut self) -> Option<MathNode> {
        self.skip_whitespace();
        if self.peek() == Some('[') {
            self.next_char();
            let inner = self.parse_row(&["]"]);
            if self.peek() == Some(']') {
                self.next_char();
            }
            Some(inner)
        } else {
            None
        }
    }

    fn parse_atom_with_scripts(&mut self) -> MathNode {
        let base = self.parse_atom();
        let mut sub: Option<MathNode> = None;
        let mut sup: Option<MathNode> = None;

        loop {
            self.skip_whitespace();
            match self.peek() {
                Some('_') if sub.is_none() => {
                    self.next_char();
                    sub = Some(self.parse_group_or_atom());
                }
                Some('^') if sup.is_none() => {
                    self.next_char();
                    sup = Some(self.parse_group_or_atom());
                }
                _ => break,
            }
        }

        if sub.is_none() && sup.is_none() {
            return base;
        }

        if let MathNode::BigOp {
            symbol,
            lower,
            upper,
            limits_above_below,
        } = base
        {
            return MathNode::BigOp {
                symbol,
                lower: sub.map(Box::new).or(lower),
                upper: sup.map(Box::new).or(upper),
                limits_above_below,
            };
        }

        MathNode::Scripts {
            base: Box::new(base),
            sub: sub.map(Box::new),
            sup: sup.map(Box::new),
        }
    }

    fn parse_raw_braced_text(&mut self) -> String {
        self.skip_whitespace();
        if self.peek() == Some('{') {
            self.next_char();
            let start = self.pos;
            let mut depth = 1usize;
            while let Some(ch) = self.next_char() {
                if ch == '{' {
                    depth += 1;
                } else if ch == '}' {
                    depth -= 1;
                    if depth == 0 {
                        return self.src[start..self.pos - 1].to_string();
                    }
                }
            }
            self.src[start..].to_string()
        } else if let Some(ch) = self.next_char() {
            ch.to_string()
        } else {
            String::new()
        }
    }

    fn parse_delimiter(&mut self) -> String {
        self.skip_whitespace();
        if self.peek() == Some('\\') {
            self.next_char();
            let cmd = self.read_command_name();
            match cmd.as_str() {
                "{" => "{".to_string(),
                "}" => "}".to_string(),
                "|" => "‖".to_string(),
                "langle" => "⟨".to_string(),
                "rangle" => "⟩".to_string(),
                "lfloor" => "⌊".to_string(),
                "rfloor" => "⌋".to_string(),
                "lceil" => "⌈".to_string(),
                "rceil" => "⌉".to_string(),
                "" => String::new(),
                other => other.to_string(),
            }
        } else if let Some(ch) = self.next_char() {
            if ch == '.' {
                String::new()
            } else {
                ch.to_string()
            }
        } else {
            String::new()
        }
    }

    fn read_command_name(&mut self) -> String {
        let Some(first) = self.peek() else {
            return String::new();
        };
        if !first.is_ascii_alphabetic() {
            self.next_char();
            return first.to_string();
        }
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphabetic() {
                self.pos += ch.len_utf8();
            } else {
                break;
            }
        }
        self.src[start..self.pos].to_string()
    }

    fn parse_atom(&mut self) -> MathNode {
        self.skip_whitespace();
        let Some(ch) = self.peek() else {
            return MathNode::Empty;
        };

        match ch {
            '{' => {
                self.next_char();
                let inner = self.parse_row(&[]);
                if self.peek() == Some('}') {
                    self.next_char();
                }
                inner
            }
            '\\' => {
                self.next_char();
                let cmd = self.read_command_name();
                self.parse_command(&cmd)
            }
            '0'..='9' => {
                let start = self.pos;
                while let Some(c) = self.peek() {
                    if c.is_ascii_digit() || c == '.' {
                        self.pos += c.len_utf8();
                    } else {
                        break;
                    }
                }
                MathNode::Number(self.src[start..self.pos].to_string())
            }
            '+' | '-' | '=' | '<' | '>' | '*' | '/' | ':' => {
                self.next_char();
                let sym = match ch {
                    '-' => '−',
                    '*' => '∗',
                    other => other,
                };
                MathNode::Operator(sym.to_string())
            }
            ',' | ';' | '!' | '(' | ')' | '[' | ']' | '|' => {
                self.next_char();
                MathNode::Text(ch.to_string())
            }
            _ => {
                self.next_char();
                if ch.is_alphabetic() {
                    MathNode::Ident(ch.to_string())
                } else {
                    MathNode::Text(ch.to_string())
                }
            }
        }
    }

    fn parse_command(&mut self, cmd: &str) -> MathNode {
        match cmd {
            // Fractions & binomials
            "frac" | "dfrac" | "tfrac" | "cfrac" => {
                let num = self.parse_group_or_atom();
                let den = self.parse_group_or_atom();
                MathNode::Frac {
                    num: Box::new(num),
                    den: Box::new(den),
                }
            }
            "binom" | "dbinom" | "tbinom" => {
                let top = self.parse_group_or_atom();
                let bottom = self.parse_group_or_atom();
                MathNode::Binom {
                    top: Box::new(top),
                    bottom: Box::new(bottom),
                }
            }
            // Roots
            "sqrt" => {
                let index = self.parse_bracket_optional().map(Box::new);
                let body = self.parse_group_or_atom();
                MathNode::Sqrt {
                    index,
                    body: Box::new(body),
                }
            }
            // Delimiters
            "left" => {
                let left = self.parse_delimiter();
                let body = self.parse_row(&["\\right"]);
                let right = if self.starts_with("\\right") {
                    self.pos += "\\right".len();
                    self.parse_delimiter()
                } else {
                    String::new()
                };
                MathNode::Delimited {
                    left,
                    right,
                    body: Box::new(body),
                }
            }
            "right" => MathNode::Empty,
            // Environments (matrices, cases, align)
            "begin" => {
                let env = self.parse_raw_braced_text();
                let end_tag = format!("\\end{{{env}}}");
                let delim = match env.as_str() {
                    "pmatrix" => MatrixDelim::Paren,
                    "bmatrix" => MatrixDelim::Bracket,
                    "vmatrix" | "Vmatrix" => MatrixDelim::Pipe,
                    "Bmatrix" => MatrixDelim::Brace,
                    "cases" => MatrixDelim::Cases,
                    _ => MatrixDelim::None,
                };
                let mut rows = Vec::new();
                let mut current_row = Vec::new();
                loop {
                    let cell = self.parse_row(&["&", "\\\\", &end_tag]);
                    current_row.push(cell);
                    if self.starts_with("&") {
                        self.pos += 1;
                    } else if self.starts_with("\\\\") {
                        self.pos += 2;
                        rows.push(std::mem::take(&mut current_row));
                    } else {
                        break;
                    }
                }
                if !current_row.is_empty() {
                    rows.push(current_row);
                }
                if self.starts_with(&end_tag) {
                    self.pos += end_tag.len();
                }
                MathNode::Matrix { delim, rows }
            }
            // Accents
            "vec" => MathNode::Accent {
                kind: AccentKind::Vec,
                body: Box::new(self.parse_group_or_atom()),
            },
            "hat" | "widehat" => MathNode::Accent {
                kind: AccentKind::Hat,
                body: Box::new(self.parse_group_or_atom()),
            },
            "bar" | "overline" => MathNode::Accent {
                kind: AccentKind::Bar,
                body: Box::new(self.parse_group_or_atom()),
            },
            "underline" => MathNode::Accent {
                kind: AccentKind::Underline,
                body: Box::new(self.parse_group_or_atom()),
            },
            "dot" => MathNode::Accent {
                kind: AccentKind::Dot,
                body: Box::new(self.parse_group_or_atom()),
            },
            "ddot" => MathNode::Accent {
                kind: AccentKind::Ddot,
                body: Box::new(self.parse_group_or_atom()),
            },
            "tilde" | "widetilde" => MathNode::Accent {
                kind: AccentKind::Tilde,
                body: Box::new(self.parse_group_or_atom()),
            },
            // Styled text
            "text" | "textrm" | "mbox" => MathNode::Text(self.parse_raw_braced_text()),
            "mathrm" | "operatorname" => MathNode::Styled {
                kind: MathStyleKind::Roman,
                body: Box::new(self.parse_group_or_atom()),
            },
            "mathbf" | "textbf" | "boldsymbol" => MathNode::Styled {
                kind: MathStyleKind::Bold,
                body: Box::new(self.parse_group_or_atom()),
            },
            "mathit" | "textit" => MathNode::Styled {
                kind: MathStyleKind::Italic,
                body: Box::new(self.parse_group_or_atom()),
            },
            "mathsf" => MathNode::Styled {
                kind: MathStyleKind::Sans,
                body: Box::new(self.parse_group_or_atom()),
            },
            "mathtt" | "texttt" => MathNode::Styled {
                kind: MathStyleKind::Mono,
                body: Box::new(self.parse_group_or_atom()),
            },
            "mathbb" => {
                let raw = self.parse_raw_braced_text();
                let mapped: String = raw
                    .chars()
                    .map(|c| match c {
                        'R' => 'ℝ',
                        'N' => 'ℕ',
                        'Z' => 'ℤ',
                        'Q' => 'ℚ',
                        'C' => 'ℂ',
                        'P' => 'ℙ',
                        'E' => '𝔼',
                        'H' => 'ℍ',
                        other => other,
                    })
                    .collect();
                MathNode::Text(mapped)
            }
            "mathcal" => MathNode::Styled {
                kind: MathStyleKind::Calligraphic,
                body: Box::new(self.parse_group_or_atom()),
            },
            // Big operators
            "sum" => MathNode::BigOp {
                symbol: "∑".to_string(),
                lower: None,
                upper: None,
                limits_above_below: true,
            },
            "prod" => MathNode::BigOp {
                symbol: "∏".to_string(),
                lower: None,
                upper: None,
                limits_above_below: true,
            },
            "coprod" => MathNode::BigOp {
                symbol: "∐".to_string(),
                lower: None,
                upper: None,
                limits_above_below: true,
            },
            "int" => MathNode::BigOp {
                symbol: "∫".to_string(),
                lower: None,
                upper: None,
                limits_above_below: false,
            },
            "iint" => MathNode::BigOp {
                symbol: "∬".to_string(),
                lower: None,
                upper: None,
                limits_above_below: false,
            },
            "iiint" => MathNode::BigOp {
                symbol: "∭".to_string(),
                lower: None,
                upper: None,
                limits_above_below: false,
            },
            "oint" => MathNode::BigOp {
                symbol: "∮".to_string(),
                lower: None,
                upper: None,
                limits_above_below: false,
            },
            "bigcup" => MathNode::BigOp {
                symbol: "⋃".to_string(),
                lower: None,
                upper: None,
                limits_above_below: true,
            },
            "bigcap" => MathNode::BigOp {
                symbol: "⋂".to_string(),
                lower: None,
                upper: None,
                limits_above_below: true,
            },
            "lim" => MathNode::BigOp {
                symbol: "lim".to_string(),
                lower: None,
                upper: None,
                limits_above_below: true,
            },
            "limsup" => MathNode::BigOp {
                symbol: "lim sup".to_string(),
                lower: None,
                upper: None,
                limits_above_below: true,
            },
            "liminf" => MathNode::BigOp {
                symbol: "lim inf".to_string(),
                lower: None,
                upper: None,
                limits_above_below: true,
            },
            // Spaces
            "," | "thinspace" => MathNode::Space(0.18),
            ":" | "medspace" => MathNode::Space(0.25),
            ";" | "thickspace" => MathNode::Space(0.35),
            " " => MathNode::Space(0.35),
            "!" => MathNode::Space(-0.15),
            "quad" => MathNode::Space(1.0),
            "qquad" => MathNode::Space(2.0),
            // Escaped literals
            "{" | "}" | "%" | "$" | "&" | "#" | "_" => MathNode::Text(cmd.to_string()),
            "|" => MathNode::Text("‖".to_string()),
            "\\" => MathNode::Space(0.35),
            // Standard functions (upright)
            "sin" | "cos" | "tan" | "cot" | "sec" | "csc" | "arcsin" | "arccos" | "arctan"
            | "sinh" | "cosh" | "tanh" | "log" | "ln" | "lg" | "exp" | "sup" | "inf" | "max"
            | "min" | "arg" | "det" | "dim" | "gcd" | "ker" | "deg" | "mod" | "Pr" => {
                MathNode::Text(cmd.to_string())
            }
            "bmod" => MathNode::Operator("mod".to_string()),
            // Symbols
            other => {
                if let Some((sym, is_op)) = lookup_symbol(other) {
                    if is_op {
                        MathNode::Operator(sym.to_string())
                    } else {
                        MathNode::Ident(sym.to_string())
                    }
                } else {
                    MathNode::Text(format!("\\{other}"))
                }
            }
        }
    }
}

fn lookup_symbol(name: &str) -> Option<(&'static str, bool)> {
    // Returns (unicode_str, is_operator)
    let entry = match name {
        // Greek lowercase
        "alpha" => ("α", false),
        "beta" => ("β", false),
        "gamma" => ("γ", false),
        "delta" => ("δ", false),
        "epsilon" => ("ϵ", false),
        "varepsilon" => ("ε", false),
        "zeta" => ("ζ", false),
        "eta" => ("η", false),
        "theta" => ("θ", false),
        "vartheta" => ("ϑ", false),
        "iota" => ("ι", false),
        "kappa" => ("κ", false),
        "lambda" => ("λ", false),
        "mu" => ("μ", false),
        "nu" => ("ν", false),
        "xi" => ("ξ", false),
        "pi" => ("π", false),
        "varpi" => ("ϖ", false),
        "rho" => ("ρ", false),
        "varrho" => ("ϱ", false),
        "sigma" => ("σ", false),
        "varsigma" => ("ς", false),
        "tau" => ("τ", false),
        "upsilon" => ("υ", false),
        "phi" => ("ϕ", false),
        "varphi" => ("φ", false),
        "chi" => ("χ", false),
        "psi" => ("ψ", false),
        "omega" => ("ω", false),
        // Greek uppercase
        "Gamma" => ("Γ", false),
        "Delta" => ("Δ", false),
        "Theta" => ("Θ", false),
        "Lambda" => ("Λ", false),
        "Xi" => ("Ξ", false),
        "Pi" => ("Π", false),
        "Sigma" => ("Σ", false),
        "Upsilon" => ("Υ", false),
        "Phi" => ("Φ", false),
        "Psi" => ("Ψ", false),
        "Omega" => ("Ω", false),
        // Binary & relational operators
        "pm" => ("±", true),
        "mp" => ("∓", true),
        "times" => ("×", true),
        "cdot" => ("·", true),
        "div" => ("÷", true),
        "ast" => ("∗", true),
        "star" => ("⋆", true),
        "circ" => ("∘", true),
        "bullet" => ("•", true),
        "oplus" => ("⊕", true),
        "otimes" => ("⊗", true),
        "le" | "leq" => ("≤", true),
        "ge" | "geq" => ("≥", true),
        "ne" | "neq" => ("≠", true),
        "approx" => ("≈", true),
        "equiv" => ("≡", true),
        "sim" => ("∼", true),
        "simeq" => ("≃", true),
        "cong" => ("≅", true),
        "propto" => ("∝", true),
        "ll" => ("≪", true),
        "gg" => ("≫", true),
        // Set theory & logic
        "in" => ("∈", true),
        "notin" => ("∉", true),
        "ni" => ("∋", true),
        "subset" => ("⊂", true),
        "subseteq" => ("⊆", true),
        "supset" => ("⊃", true),
        "supseteq" => ("⊇", true),
        "cup" => ("∪", true),
        "cap" => ("∩", true),
        "setminus" => ("∖", true),
        "emptyset" | "varnothing" => ("∅", false),
        "forall" => ("∀", false),
        "exists" => ("∃", false),
        "nexists" => ("∄", false),
        "land" | "wedge" => ("∧", true),
        "lor" | "vee" => ("∨", true),
        "neg" | "lnot" => ("¬", false),
        "therefore" => ("∴", true),
        "because" => ("∵", true),
        "top" => ("⊤", false),
        "bot" => ("⊥", false),
        // Arrows
        "to" | "rightarrow" => ("→", true),
        "leftarrow" | "gets" => ("←", true),
        "leftrightarrow" => ("↔", true),
        "Rightarrow" | "implies" => ("⇒", true),
        "Leftarrow" => ("⇐", true),
        "Leftrightarrow" | "iff" => ("⇔", true),
        "mapsto" => ("↦", true),
        "uparrow" => ("↑", true),
        "downarrow" => ("↓", true),
        // Misc math symbols
        "infty" => ("∞", false),
        "partial" => ("∂", false),
        "nabla" => ("∇", false),
        "dots" | "ldots" => ("…", false),
        "cdots" => ("⋯", false),
        "vdots" => ("⋮", false),
        "ddots" => ("⋱", false),
        "angle" => ("∠", false),
        "degree" => ("°", false),
        "prime" => ("′", false),
        "perp" => ("⊥", true),
        "parallel" => ("∥", true),
        "langle" => ("⟨", false),
        "rangle" => ("⟩", false),
        "hbar" => ("ℏ", false),
        "ell" => ("ℓ", false),
        "Re" => ("ℜ", false),
        "Im" => ("ℑ", false),
        "aleph" => ("ℵ", false),
        _ => return None,
    };
    Some(entry)
}

pub fn parse_latex(input: &str) -> MathNode {
    let mut parser = Parser::new(input.trim());
    parser.parse_row(&[])
}

// --- Layout & Rendering ---

#[derive(Clone, Copy)]
struct RenderCtx<'a> {
    font_size: f64,
    display_mode: bool,
    color: Color,
    font_family: &'a str,
    style_override: Option<MathStyleKind>,
}

impl<'a> RenderCtx<'a> {
    fn scaled(&self, factor: f64) -> Self {
        Self {
            font_size: (self.font_size * factor).max(7.0),
            display_mode: false,
            ..*self
        }
    }

    fn with_style(&self, kind: MathStyleKind) -> Self {
        Self {
            style_override: Some(kind),
            ..*self
        }
    }
}

fn build_math_text_layout<T: Text>(
    piet_text: &mut T,
    text: &str,
    font_size: f64,
    italic: bool,
    bold: bool,
    mono: bool,
    color: Color,
    font_family: &str,
) -> Option<T::TextLayout> {
    let family = if mono {
        piet_text
            .font_family("monospace")
            .unwrap_or(FontFamily::MONOSPACE)
    } else {
        piet_text
            .font_family(font_family)
            .unwrap_or(FontFamily::SERIF)
    };
    let mut builder = piet_text
        .new_text_layout(text.to_string())
        .font(family, font_size)
        .text_color(piet::Color::from(color));
    if italic {
        builder = builder.default_attribute(TextAttribute::Style(FontStyle::Italic));
    }
    if bold {
        builder = builder.default_attribute(TextAttribute::Weight(FontWeight::BOLD));
    }
    builder.build().ok()
}

fn text_metrics<T: Text>(
    piet_text: &mut T,
    text: &str,
    font_size: f64,
    italic: bool,
    bold: bool,
    mono: bool,
    color: Color,
    font_family: &str,
) -> MathMetrics {
    if text.is_empty() {
        return MathMetrics {
            width: 0.0,
            ascent: font_size * 0.8,
            descent: font_size * 0.2,
        };
    }
    if let Some(layout) = build_math_text_layout(
        piet_text,
        text,
        font_size,
        italic,
        bold,
        mono,
        color,
        font_family,
    ) {
        let size = layout.size();
        if let Some(lm) = layout.line_metric(0) {
            let ascent = lm.baseline.max(0.0);
            let descent = (lm.height - lm.baseline).max(0.0);
            return MathMetrics {
                width: size.width,
                ascent,
                descent,
            };
        }
        return MathMetrics {
            width: size.width,
            ascent: size.height * 0.8,
            descent: size.height * 0.2,
        };
    }
    MathMetrics {
        width: text.chars().count() as f64 * font_size * 0.55,
        ascent: font_size * 0.8,
        descent: font_size * 0.2,
    }
}

fn draw_text_at(
    cx: &mut impl RenderContext,
    text: &str,
    x: f64,
    baseline_y: f64,
    font_size: f64,
    italic: bool,
    bold: bool,
    mono: bool,
    color: Color,
    font_family: &str,
) {
    if text.is_empty() {
        return;
    }
    if let Some(layout) = build_math_text_layout(
        cx.text(),
        text,
        font_size,
        italic,
        bold,
        mono,
        color,
        font_family,
    ) {
        let baseline = layout
            .line_metric(0)
            .map(|lm| lm.baseline)
            .unwrap_or(font_size * 0.8);
        cx.draw_text(&layout, Point::new(x, baseline_y - baseline));
    }
}

fn style_flags(is_ident: bool, override_style: Option<MathStyleKind>) -> (bool, bool, bool) {
    // Returns (italic, bold, mono)
    match override_style {
        Some(MathStyleKind::Roman | MathStyleKind::Text | MathStyleKind::Sans | MathStyleKind::Blackboard) => {
            (false, false, false)
        }
        Some(MathStyleKind::Bold) => (false, true, false),
        Some(MathStyleKind::Italic | MathStyleKind::Calligraphic) => (true, false, false),
        Some(MathStyleKind::Mono) => (false, false, true),
        None => (is_ident, false, false),
    }
}

fn measure_node<T: Text>(piet_text: &mut T, node: &MathNode, ctx: RenderCtx) -> MathMetrics {
    let fs = ctx.font_size;
    let axis = fs * 0.28;
    match node {
        MathNode::Empty => MathMetrics {
            width: 0.0,
            ascent: fs * 0.75,
            descent: fs * 0.2,
        },
        MathNode::Space(em) => MathMetrics {
            width: (em * fs * 0.5).max(0.0),
            ascent: fs * 0.75,
            descent: fs * 0.2,
        },
        MathNode::Ident(s) => {
            let (italic, bold, mono) = style_flags(true, ctx.style_override);
            text_metrics(piet_text, s, fs, italic, bold, mono, ctx.color, ctx.font_family)
        }
        MathNode::Number(s) | MathNode::Text(s) => {
            let (italic, bold, mono) = style_flags(false, ctx.style_override);
            text_metrics(piet_text, s, fs, italic, bold, mono, ctx.color, ctx.font_family)
        }
        MathNode::Operator(s) => {
            let pad = fs * 0.18;
            let mut m = text_metrics(piet_text, s, fs, false, false, false, ctx.color, ctx.font_family);
            m.width += 2.0 * pad;
            m
        }
        MathNode::Row(nodes) => {
            let mut width = 0.0;
            let mut ascent = 0.0_f64;
            let mut descent = 0.0_f64;
            for n in nodes {
                let m = measure_node(piet_text, n, ctx);
                width += m.width;
                ascent = ascent.max(m.ascent);
                descent = descent.max(m.descent);
            }
            MathMetrics {
                width,
                ascent,
                descent,
            }
        }
        MathNode::Frac { num, den } => {
            let sub_ctx = ctx.scaled(if ctx.display_mode { 0.85 } else { 0.72 });
            let nm = measure_node(piet_text, num, sub_ctx);
            let dm = measure_node(piet_text, den, sub_ctx);
            let gap = fs * 0.14;
            let pad = fs * 0.12;
            let width = nm.width.max(dm.width) + 2.0 * pad;
            let ascent = axis + gap + nm.height();
            let descent = (gap + dm.height() - axis).max(fs * 0.2);
            MathMetrics {
                width,
                ascent,
                descent,
            }
        }
        MathNode::Binom { top, bottom } => {
            let sub_ctx = ctx.scaled(0.75);
            let tm = measure_node(piet_text, top, sub_ctx);
            let bm = measure_node(piet_text, bottom, sub_ctx);
            let gap = fs * 0.16;
            let delim_w = fs * 0.36;
            let inner_w = tm.width.max(bm.width);
            let ascent = axis + gap * 0.5 + tm.height();
            let descent = (gap * 0.5 + bm.height() - axis).max(fs * 0.2);
            MathMetrics {
                width: inner_w + 2.0 * delim_w,
                ascent,
                descent,
            }
        }
        MathNode::Sqrt { index, body } => {
            let bm = measure_node(piet_text, body, ctx);
            let hook_w = fs * 0.52;
            let idx_w = index
                .as_ref()
                .map(|i| measure_node(piet_text, i, ctx.scaled(0.55)).width)
                .unwrap_or(0.0);
            let lead_w = hook_w.max(idx_w + fs * 0.2);
            let top_pad = fs * 0.22;
            MathMetrics {
                width: lead_w + bm.width + fs * 0.12,
                ascent: bm.ascent + top_pad,
                descent: bm.descent,
            }
        }
        MathNode::Scripts { base, sub, sup } => {
            let bm = measure_node(piet_text, base, ctx);
            let sc_ctx = ctx.scaled(0.68);
            let sub_m = sub.as_ref().map(|n| measure_node(piet_text, n, sc_ctx));
            let sup_m = sup.as_ref().map(|n| measure_node(piet_text, n, sc_ctx));
            let script_w = sub_m
                .map(|m| m.width)
                .unwrap_or(0.0)
                .max(sup_m.map(|m| m.width).unwrap_or(0.0));
            let sup_shift = (bm.ascent * 0.55).max(fs * 0.35);
            let sub_shift = fs * 0.25;
            let ascent = bm.ascent.max(sup_m.map(|m| sup_shift + m.ascent).unwrap_or(0.0));
            let descent = bm.descent.max(sub_m.map(|m| sub_shift + m.descent).unwrap_or(0.0));
            MathMetrics {
                width: bm.width + script_w + fs * 0.04,
                ascent,
                descent,
            }
        }
        MathNode::BigOp {
            symbol,
            lower,
            upper,
            limits_above_below,
        } => {
            let is_word = symbol.chars().count() > 1;
            let op_scale = if is_word {
                1.0
            } else if ctx.display_mode {
                1.45
            } else {
                1.2
            };
            let op_m = text_metrics(
                piet_text,
                symbol,
                fs * op_scale,
                false,
                false,
                false,
                ctx.color,
                ctx.font_family,
            );
            let sc_ctx = ctx.scaled(0.66);
            let low_m = lower.as_ref().map(|n| measure_node(piet_text, n, sc_ctx));
            let up_m = upper.as_ref().map(|n| measure_node(piet_text, n, sc_ctx));

            if *limits_above_below && (ctx.display_mode || is_word) {
                let width = op_m
                    .width
                    .max(low_m.map(|m| m.width).unwrap_or(0.0))
                    .max(up_m.map(|m| m.width).unwrap_or(0.0))
                    + fs * 0.16;
                let gap = fs * 0.1;
                let ascent = op_m.ascent + up_m.map(|m| m.height() + gap).unwrap_or(0.0);
                let descent = op_m.descent + low_m.map(|m| m.height() + gap).unwrap_or(0.0);
                MathMetrics {
                    width,
                    ascent,
                    descent,
                }
            } else {
                let script_w = low_m
                    .map(|m| m.width)
                    .unwrap_or(0.0)
                    .max(up_m.map(|m| m.width).unwrap_or(0.0));
                let sup_shift = op_m.ascent * 0.55;
                let sub_shift = op_m.descent + fs * 0.12;
                let ascent = op_m
                    .ascent
                    .max(up_m.map(|m| sup_shift + m.ascent).unwrap_or(0.0));
                let descent = op_m
                    .descent
                    .max(low_m.map(|m| sub_shift + m.descent).unwrap_or(0.0));
                MathMetrics {
                    width: op_m.width + script_w + fs * 0.12,
                    ascent,
                    descent,
                }
            }
        }
        MathNode::Delimited { left, right, body } => {
            let bm = measure_node(piet_text, body, ctx);
            let lw = if left.is_empty() { 0.0 } else { fs * 0.38 };
            let rw = if right.is_empty() { 0.0 } else { fs * 0.38 };
            MathMetrics {
                width: lw + bm.width + rw,
                ascent: bm.ascent + fs * 0.06,
                descent: bm.descent + fs * 0.06,
            }
        }
        MathNode::Accent { kind, body } => {
            let mut bm = measure_node(piet_text, body, ctx);
            match kind {
                AccentKind::Underline => bm.descent += fs * 0.18,
                _ => bm.ascent += fs * 0.25,
            }
            bm
        }
        MathNode::Styled { kind, body } => measure_node(piet_text, body, ctx.with_style(*kind)),
        MathNode::Matrix { delim, rows } => {
            if rows.is_empty() {
                return MathMetrics {
                    width: fs,
                    ascent: fs * 0.8,
                    descent: fs * 0.2,
                };
            }
            let num_cols = rows.iter().map(|r| r.len()).max().unwrap_or(1);
            let mut col_widths = vec![0.0_f64; num_cols];
            let mut row_heights = Vec::with_capacity(rows.len());
            let cell_ctx = ctx.scaled(0.92);
            for row in rows {
                let mut max_asc = fs * 0.65;
                let mut max_desc = fs * 0.22;
                for (c, cell) in row.iter().enumerate() {
                    let cm = measure_node(piet_text, cell, cell_ctx);
                    col_widths[c] = col_widths[c].max(cm.width);
                    max_asc = max_asc.max(cm.ascent);
                    max_desc = max_desc.max(cm.descent);
                }
                row_heights.push((max_asc, max_desc));
            }
            let col_gap = fs * 0.65;
            let row_gap = fs * 0.28;
            let inner_w: f64 =
                col_widths.iter().sum::<f64>() + col_gap * (num_cols.saturating_sub(1) as f64);
            let total_h: f64 = row_heights
                .iter()
                .map(|(a, d)| a + d)
                .sum::<f64>()
                + row_gap * (rows.len().saturating_sub(1) as f64);
            let lw = match delim {
                MatrixDelim::None => 0.0,
                _ => fs * 0.42,
            };
            let rw = match delim {
                MatrixDelim::None | MatrixDelim::Cases => 0.0,
                _ => fs * 0.42,
            };
            MathMetrics {
                width: lw + inner_w + rw,
                ascent: total_h * 0.5 + axis,
                descent: (total_h * 0.5 - axis).max(fs * 0.2),
            }
        }
    }
}

fn draw_scaled_delimiter(
    cx: &mut impl RenderContext,
    delim: &str,
    x: f64,
    top_y: f64,
    bottom_y: f64,
    width: f64,
    color: Color,
    stroke_w: f64,
) {
    if delim.is_empty() {
        return;
    }
    let piet_color = piet::Color::from(color);
    let h = (bottom_y - top_y).max(4.0);
    let mid_y = (top_y + bottom_y) * 0.5;
    let mut path = BezPath::new();

    match delim {
        "(" => {
            path.move_to((x + width * 0.78, top_y));
            path.curve_to(
                (x + width * 0.2, top_y + h * 0.25),
                (x + width * 0.2, bottom_y - h * 0.25),
                (x + width * 0.78, bottom_y),
            );
        }
        ")" => {
            path.move_to((x + width * 0.22, top_y));
            path.curve_to(
                (x + width * 0.8, top_y + h * 0.25),
                (x + width * 0.8, bottom_y - h * 0.25),
                (x + width * 0.22, bottom_y),
            );
        }
        "[" => {
            path.move_to((x + width * 0.78, top_y));
            path.line_to((x + width * 0.28, top_y));
            path.line_to((x + width * 0.28, bottom_y));
            path.line_to((x + width * 0.78, bottom_y));
        }
        "]" => {
            path.move_to((x + width * 0.22, top_y));
            path.line_to((x + width * 0.72, top_y));
            path.line_to((x + width * 0.72, bottom_y));
            path.line_to((x + width * 0.22, bottom_y));
        }
        "{" => {
            path.move_to((x + width * 0.8, top_y));
            path.quad_to((x + width * 0.45, top_y), (x + width * 0.45, top_y + h * 0.22));
            path.line_to((x + width * 0.45, mid_y - h * 0.08));
            path.quad_to((x + width * 0.45, mid_y), (x + width * 0.15, mid_y));
            path.quad_to((x + width * 0.45, mid_y), (x + width * 0.45, mid_y + h * 0.08));
            path.line_to((x + width * 0.45, bottom_y - h * 0.22));
            path.quad_to((x + width * 0.45, bottom_y), (x + width * 0.8, bottom_y));
        }
        "}" => {
            path.move_to((x + width * 0.2, top_y));
            path.quad_to((x + width * 0.55, top_y), (x + width * 0.55, top_y + h * 0.22));
            path.line_to((x + width * 0.55, mid_y - h * 0.08));
            path.quad_to((x + width * 0.55, mid_y), (x + width * 0.85, mid_y));
            path.quad_to((x + width * 0.55, mid_y), (x + width * 0.55, mid_y + h * 0.08));
            path.line_to((x + width * 0.55, bottom_y - h * 0.22));
            path.quad_to((x + width * 0.55, bottom_y), (x + width * 0.2, bottom_y));
        }
        "|" => {
            path.move_to((x + width * 0.5, top_y));
            path.line_to((x + width * 0.5, bottom_y));
        }
        "‖" => {
            path.move_to((x + width * 0.35, top_y));
            path.line_to((x + width * 0.35, bottom_y));
            path.move_to((x + width * 0.65, top_y));
            path.line_to((x + width * 0.65, bottom_y));
        }
        "⟨" => {
            path.move_to((x + width * 0.8, top_y));
            path.line_to((x + width * 0.2, mid_y));
            path.line_to((x + width * 0.8, bottom_y));
        }
        "⟩" => {
            path.move_to((x + width * 0.2, top_y));
            path.line_to((x + width * 0.8, mid_y));
            path.line_to((x + width * 0.2, bottom_y));
        }
        other => {
            draw_text_at(
                cx,
                other,
                x,
                (top_y + bottom_y) * 0.5 + h * 0.3,
                h * 0.75,
                false,
                false,
                false,
                color,
                "serif",
            );
            return;
        }
    }

    cx.stroke(&path, &piet_color, stroke_w);
}

fn draw_node(
    cx: &mut impl RenderContext,
    node: &MathNode,
    x: f64,
    baseline_y: f64,
    ctx: RenderCtx,
) -> MathMetrics {
    let fs = ctx.font_size;
    let axis = fs * 0.28;
    let stroke_w = (fs * 0.06).max(1.0);
    let piet_color = piet::Color::from(ctx.color);

    match node {
        MathNode::Empty | MathNode::Space(_) => measure_node(cx.text(), node, ctx),
        MathNode::Ident(s) => {
            let (italic, bold, mono) = style_flags(true, ctx.style_override);
            draw_text_at(cx, s, x, baseline_y, fs, italic, bold, mono, ctx.color, ctx.font_family);
            measure_node(cx.text(), node, ctx)
        }
        MathNode::Number(s) | MathNode::Text(s) => {
            let (italic, bold, mono) = style_flags(false, ctx.style_override);
            draw_text_at(cx, s, x, baseline_y, fs, italic, bold, mono, ctx.color, ctx.font_family);
            measure_node(cx.text(), node, ctx)
        }
        MathNode::Operator(s) => {
            let pad = fs * 0.18;
            draw_text_at(cx, s, x + pad, baseline_y, fs, false, false, false, ctx.color, ctx.font_family);
            measure_node(cx.text(), node, ctx)
        }
        MathNode::Row(nodes) => {
            let mut cur_x = x;
            let mut ascent = 0.0_f64;
            let mut descent = 0.0_f64;
            for n in nodes {
                let m = draw_node(cx, n, cur_x, baseline_y, ctx);
                cur_x += m.width;
                ascent = ascent.max(m.ascent);
                descent = descent.max(m.descent);
            }
            MathMetrics {
                width: cur_x - x,
                ascent,
                descent,
            }
        }
        MathNode::Frac { num, den } => {
            let sub_ctx = ctx.scaled(if ctx.display_mode { 0.85 } else { 0.72 });
            let nm = measure_node(cx.text(), num, sub_ctx);
            let dm = measure_node(cx.text(), den, sub_ctx);
            let gap = fs * 0.14;
            let pad = fs * 0.12;
            let total_w = nm.width.max(dm.width) + 2.0 * pad;
            let bar_y = baseline_y - axis;

            let num_x = x + (total_w - nm.width) * 0.5;
            let num_baseline = bar_y - gap - nm.descent;
            draw_node(cx, num, num_x, num_baseline, sub_ctx);

            cx.stroke(
                kurbo::Line::new((x + pad * 0.3, bar_y), (x + total_w - pad * 0.3, bar_y)),
                &piet_color,
                stroke_w,
            );

            let den_x = x + (total_w - dm.width) * 0.5;
            let den_baseline = bar_y + gap + dm.ascent;
            draw_node(cx, den, den_x, den_baseline, sub_ctx);

            MathMetrics {
                width: total_w,
                ascent: axis + gap + nm.height(),
                descent: (gap + dm.height() - axis).max(fs * 0.2),
            }
        }
        MathNode::Binom { top, bottom } => {
            let sub_ctx = ctx.scaled(0.75);
            let tm = measure_node(cx.text(), top, sub_ctx);
            let bm = measure_node(cx.text(), bottom, sub_ctx);
            let gap = fs * 0.16;
            let delim_w = fs * 0.36;
            let inner_w = tm.width.max(bm.width);
            let bar_y = baseline_y - axis;
            let ascent = axis + gap * 0.5 + tm.height();
            let descent = (gap * 0.5 + bm.height() - axis).max(fs * 0.2);

            draw_scaled_delimiter(
                cx,
                "(",
                x,
                baseline_y - ascent,
                baseline_y + descent,
                delim_w,
                ctx.color,
                stroke_w,
            );
            draw_node(
                cx,
                top,
                x + delim_w + (inner_w - tm.width) * 0.5,
                bar_y - gap * 0.5 - tm.descent,
                sub_ctx,
            );
            draw_node(
                cx,
                bottom,
                x + delim_w + (inner_w - bm.width) * 0.5,
                bar_y + gap * 0.5 + bm.ascent,
                sub_ctx,
            );
            draw_scaled_delimiter(
                cx,
                ")",
                x + delim_w + inner_w,
                baseline_y - ascent,
                baseline_y + descent,
                delim_w,
                ctx.color,
                stroke_w,
            );

            MathMetrics {
                width: inner_w + 2.0 * delim_w,
                ascent,
                descent,
            }
        }
        MathNode::Sqrt { index, body } => {
            let bm = measure_node(cx.text(), body, ctx);
            let hook_w = fs * 0.52;
            let idx_ctx = ctx.scaled(0.55);
            let idx_m = index.as_ref().map(|i| measure_node(cx.text(), i, idx_ctx));
            let idx_w = idx_m.map(|m| m.width).unwrap_or(0.0);
            let lead_w = hook_w.max(idx_w + fs * 0.2);
            let top_pad = fs * 0.22;
            let top_y = baseline_y - bm.ascent - top_pad * 0.7;
            let bot_y = baseline_y + bm.descent * 0.85;
            let body_x = x + lead_w;
            let total_w = lead_w + bm.width + fs * 0.12;

            if let (Some(idx_node), Some(im)) = (index, idx_m) {
                let idx_y = (top_y + bot_y) * 0.5 - im.descent * 0.5;
                draw_node(cx, idx_node, x, idx_y, idx_ctx);
            }

            let mut path = BezPath::new();
            path.move_to((body_x - hook_w * 0.9, (top_y + bot_y) * 0.52));
            path.line_to((body_x - hook_w * 0.65, (top_y + bot_y) * 0.45));
            path.line_to((body_x - hook_w * 0.35, bot_y));
            path.line_to((body_x - hook_w * 0.05, top_y));
            path.line_to((x + total_w, top_y));
            cx.stroke(&path, &piet_color, stroke_w);

            draw_node(cx, body, body_x, baseline_y, ctx);

            MathMetrics {
                width: total_w,
                ascent: bm.ascent + top_pad,
                descent: bm.descent,
            }
        }
        MathNode::Scripts { base, sub, sup } => {
            let bm = draw_node(cx, base, x, baseline_y, ctx);
            let sc_ctx = ctx.scaled(0.68);
            let sub_m = sub.as_ref().map(|n| measure_node(cx.text(), n, sc_ctx));
            let sup_m = sup.as_ref().map(|n| measure_node(cx.text(), n, sc_ctx));
            let sup_shift = (bm.ascent * 0.55).max(fs * 0.35);
            let sub_shift = fs * 0.25;

            if let Some(sup_node) = sup {
                draw_node(cx, sup_node, x + bm.width, baseline_y - sup_shift, sc_ctx);
            }
            if let Some(sub_node) = sub {
                draw_node(cx, sub_node, x + bm.width, baseline_y + sub_shift, sc_ctx);
            }

            let script_w = sub_m
                .map(|m| m.width)
                .unwrap_or(0.0)
                .max(sup_m.map(|m| m.width).unwrap_or(0.0));
            let ascent = bm.ascent.max(sup_m.map(|m| sup_shift + m.ascent).unwrap_or(0.0));
            let descent = bm.descent.max(sub_m.map(|m| sub_shift + m.descent).unwrap_or(0.0));
            MathMetrics {
                width: bm.width + script_w + fs * 0.04,
                ascent,
                descent,
            }
        }
        MathNode::BigOp {
            symbol,
            lower,
            upper,
            limits_above_below,
        } => {
            let is_word = symbol.chars().count() > 1;
            let op_scale = if is_word {
                1.0
            } else if ctx.display_mode {
                1.45
            } else {
                1.2
            };
            let op_fs = fs * op_scale;
            let op_m = text_metrics(
                cx.text(),
                symbol,
                op_fs,
                false,
                false,
                false,
                ctx.color,
                ctx.font_family,
            );
            let sc_ctx = ctx.scaled(0.66);
            let low_m = lower.as_ref().map(|n| measure_node(cx.text(), n, sc_ctx));
            let up_m = upper.as_ref().map(|n| measure_node(cx.text(), n, sc_ctx));

            if *limits_above_below && (ctx.display_mode || is_word) {
                let width = op_m
                    .width
                    .max(low_m.map(|m| m.width).unwrap_or(0.0))
                    .max(up_m.map(|m| m.width).unwrap_or(0.0))
                    + fs * 0.16;
                let gap = fs * 0.1;
                draw_text_at(
                    cx,
                    symbol,
                    x + (width - op_m.width) * 0.5,
                    baseline_y,
                    op_fs,
                    false,
                    false,
                    false,
                    ctx.color,
                    ctx.font_family,
                );
                if let (Some(up_node), Some(um)) = (upper, up_m) {
                    let up_base = baseline_y - op_m.ascent - gap - um.descent;
                    draw_node(cx, up_node, x + (width - um.width) * 0.5, up_base, sc_ctx);
                }
                if let (Some(low_node), Some(lm)) = (lower, low_m) {
                    let low_base = baseline_y + op_m.descent + gap + lm.ascent;
                    draw_node(cx, low_node, x + (width - lm.width) * 0.5, low_base, sc_ctx);
                }
                let ascent = op_m.ascent + up_m.map(|m| m.height() + gap).unwrap_or(0.0);
                let descent = op_m.descent + low_m.map(|m| m.height() + gap).unwrap_or(0.0);
                MathMetrics {
                    width,
                    ascent,
                    descent,
                }
            } else {
                draw_text_at(
                    cx,
                    symbol,
                    x,
                    baseline_y,
                    op_fs,
                    false,
                    false,
                    false,
                    ctx.color,
                    ctx.font_family,
                );
                let sup_shift = op_m.ascent * 0.55;
                let sub_shift = op_m.descent + fs * 0.12;
                if let Some(up_node) = upper {
                    draw_node(cx, up_node, x + op_m.width, baseline_y - sup_shift, sc_ctx);
                }
                if let Some(low_node) = lower {
                    draw_node(cx, low_node, x + op_m.width, baseline_y + sub_shift, sc_ctx);
                }
                let script_w = low_m
                    .map(|m| m.width)
                    .unwrap_or(0.0)
                    .max(up_m.map(|m| m.width).unwrap_or(0.0));
                let ascent = op_m
                    .ascent
                    .max(up_m.map(|m| sup_shift + m.ascent).unwrap_or(0.0));
                let descent = op_m
                    .descent
                    .max(low_m.map(|m| sub_shift + m.descent).unwrap_or(0.0));
                MathMetrics {
                    width: op_m.width + script_w + fs * 0.12,
                    ascent,
                    descent,
                }
            }
        }
        MathNode::Delimited { left, right, body } => {
            let bm = measure_node(cx.text(), body, ctx);
            let lw = if left.is_empty() { 0.0 } else { fs * 0.38 };
            let rw = if right.is_empty() { 0.0 } else { fs * 0.38 };
            let top_y = baseline_y - bm.ascent - fs * 0.04;
            let bot_y = baseline_y + bm.descent + fs * 0.04;

            draw_scaled_delimiter(cx, left, x, top_y, bot_y, lw, ctx.color, stroke_w);
            draw_node(cx, body, x + lw, baseline_y, ctx);
            draw_scaled_delimiter(cx, right, x + lw + bm.width, top_y, bot_y, rw, ctx.color, stroke_w);

            MathMetrics {
                width: lw + bm.width + rw,
                ascent: bm.ascent + fs * 0.06,
                descent: bm.descent + fs * 0.06,
            }
        }
        MathNode::Accent { kind, body } => {
            let bm = draw_node(cx, body, x, baseline_y, ctx);
            let top_y = baseline_y - bm.ascent - fs * 0.06;
            let mid_x = x + bm.width * 0.5;
            match kind {
                AccentKind::Vec => {
                    let mut p = BezPath::new();
                    p.move_to((x + fs * 0.04, top_y));
                    p.line_to((x + bm.width - fs * 0.04, top_y));
                    p.move_to((x + bm.width - fs * 0.16, top_y - fs * 0.09));
                    p.line_to((x + bm.width - fs * 0.04, top_y));
                    p.line_to((x + bm.width - fs * 0.16, top_y + fs * 0.09));
                    cx.stroke(&p, &piet_color, stroke_w * 0.9);
                }
                AccentKind::Hat => {
                    let mut p = BezPath::new();
                    let hw = (bm.width * 0.42).max(fs * 0.2);
                    p.move_to((mid_x - hw, top_y + fs * 0.06));
                    p.line_to((mid_x, top_y - fs * 0.08));
                    p.line_to((mid_x + hw, top_y + fs * 0.06));
                    cx.stroke(&p, &piet_color, stroke_w * 0.9);
                }
                AccentKind::Bar => {
                    cx.stroke(
                        kurbo::Line::new((x, top_y), (x + bm.width, top_y)),
                        &piet_color,
                        stroke_w * 0.9,
                    );
                }
                AccentKind::Underline => {
                    let bot_y = baseline_y + bm.descent * 0.7;
                    cx.stroke(
                        kurbo::Line::new((x, bot_y), (x + bm.width, bot_y)),
                        &piet_color,
                        stroke_w * 0.9,
                    );
                }
                AccentKind::Dot => {
                    cx.fill(kurbo::Circle::new((mid_x, top_y), stroke_w * 0.9), &piet_color);
                }
                AccentKind::Ddot => {
                    let off = fs * 0.12;
                    cx.fill(kurbo::Circle::new((mid_x - off, top_y), stroke_w * 0.8), &piet_color);
                    cx.fill(kurbo::Circle::new((mid_x + off, top_y), stroke_w * 0.8), &piet_color);
                }
                AccentKind::Tilde => {
                    let mut p = BezPath::new();
                    let hw = (bm.width * 0.42).max(fs * 0.22);
                    p.move_to((mid_x - hw, top_y + fs * 0.02));
                    p.curve_to(
                        (mid_x - hw * 0.33, top_y - fs * 0.12),
                        (mid_x + hw * 0.33, top_y + fs * 0.12),
                        (mid_x + hw, top_y - fs * 0.02),
                    );
                    cx.stroke(&p, &piet_color, stroke_w * 0.9);
                }
            }
            let mut out = bm;
            match kind {
                AccentKind::Underline => out.descent += fs * 0.18,
                _ => out.ascent += fs * 0.25,
            }
            out
        }
        MathNode::Styled { kind, body } => draw_node(cx, body, x, baseline_y, ctx.with_style(*kind)),
        MathNode::Matrix { delim, rows } => {
            if rows.is_empty() {
                return measure_node(cx.text(), node, ctx);
            }
            let num_cols = rows.iter().map(|r| r.len()).max().unwrap_or(1);
            let mut col_widths = vec![0.0_f64; num_cols];
            let mut row_heights = Vec::with_capacity(rows.len());
            let cell_ctx = ctx.scaled(0.92);
            for row in rows {
                let mut max_asc = fs * 0.65;
                let mut max_desc = fs * 0.22;
                for (c, cell) in row.iter().enumerate() {
                    let cm = measure_node(cx.text(), cell, cell_ctx);
                    col_widths[c] = col_widths[c].max(cm.width);
                    max_asc = max_asc.max(cm.ascent);
                    max_desc = max_desc.max(cm.descent);
                }
                row_heights.push((max_asc, max_desc));
            }
            let col_gap = fs * 0.65;
            let row_gap = fs * 0.28;
            let inner_w: f64 =
                col_widths.iter().sum::<f64>() + col_gap * (num_cols.saturating_sub(1) as f64);
            let total_h: f64 = row_heights
                .iter()
                .map(|(a, d)| a + d)
                .sum::<f64>()
                + row_gap * (rows.len().saturating_sub(1) as f64);

            let lw = match delim {
                MatrixDelim::None => 0.0,
                _ => fs * 0.42,
            };
            let rw = match delim {
                MatrixDelim::None | MatrixDelim::Cases => 0.0,
                _ => fs * 0.42,
            };
            let ascent = total_h * 0.5 + axis;
            let descent = (total_h * 0.5 - axis).max(fs * 0.2);
            let top_y = baseline_y - ascent;
            let bot_y = baseline_y + descent;

            let (ld, rd) = match delim {
                MatrixDelim::None => ("", ""),
                MatrixDelim::Paren => ("(", ")"),
                MatrixDelim::Bracket => ("[", "]"),
                MatrixDelim::Brace => ("{", "}"),
                MatrixDelim::Pipe => ("|", "|"),
                MatrixDelim::Cases => ("{", ""),
            };
            draw_scaled_delimiter(cx, ld, x, top_y, bot_y, lw, ctx.color, stroke_w);
            draw_scaled_delimiter(cx, rd, x + lw + inner_w, top_y, bot_y, rw, ctx.color, stroke_w);

            let mut cur_y = top_y;
            for (r, row) in rows.iter().enumerate() {
                let (r_asc, r_desc) = row_heights[r];
                let row_baseline = cur_y + r_asc;
                let mut cur_x = x + lw;
                for (c, cell) in row.iter().enumerate() {
                    let cm = measure_node(cx.text(), cell, cell_ctx);
                    let cell_x = if matches!(delim, MatrixDelim::Cases | MatrixDelim::None) {
                        cur_x
                    } else {
                        cur_x + (col_widths[c] - cm.width) * 0.5
                    };
                    draw_node(cx, cell, cell_x, row_baseline, cell_ctx);
                    cur_x += col_widths[c] + col_gap;
                }
                cur_y += r_asc + r_desc + row_gap;
            }

            MathMetrics {
                width: lw + inner_w + rw,
                ascent,
                descent,
            }
        }
    }
}

pub fn measure_latex<T: Text>(
    piet_text: &mut T,
    input: &str,
    font_size: f64,
    display_mode: bool,
    font_family: &str,
) -> MathMetrics {
    let ast = parse_latex(input);
    let ctx = RenderCtx {
        font_size,
        display_mode,
        color: Color::BLACK,
        font_family,
        style_override: None,
    };
    measure_node(piet_text, &ast, ctx)
}

pub fn draw_latex(
    cx: &mut impl RenderContext,
    input: &str,
    x: f64,
    baseline_y: f64,
    font_size: f64,
    display_mode: bool,
    color: Color,
    font_family: &str,
) -> MathMetrics {
    let ast = parse_latex(input);
    let ctx = RenderCtx {
        font_size,
        display_mode,
        color,
        font_family,
        style_override: None,
    };
    draw_node(cx, &ast, x, baseline_y, ctx)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_frac_and_sqrt() {
        let node = parse_latex(r"\frac{a + b}{\sqrt[3]{x^2 + 1}}");
        match node {
            MathNode::Frac { num, den } => {
                assert!(matches!(*num, MathNode::Row(_)));
                assert!(matches!(*den, MathNode::Sqrt { index: Some(_), .. }));
            }
            other => panic!("Expected Frac, got {other:?}"),
        }
    }

    #[test]
    fn parse_bigop_sum_and_greek() {
        let node = parse_latex(r"\sum_{i=1}^{n} \alpha_i");
        match node {
            MathNode::Row(items) => {
                assert!(matches!(
                    &items[0],
                    MathNode::BigOp {
                        lower: Some(_),
                        upper: Some(_),
                        limits_above_below: true,
                        ..
                    }
                ));
                assert!(matches!(&items[1], MathNode::Scripts { .. }));
            }
            other => panic!("Expected Row, got {other:?}"),
        }
    }

    #[test]
    fn parse_cases_and_blackboard() {
        let node = parse_latex(r"f(x) = \begin{cases} x^2 & x \in \mathbb{R} \\ 0 & x < 0 \end{cases}");
        let mut text = piet_cairo::CairoText::new();
        let metrics = measure_node(
            &mut text,
            &node,
            RenderCtx {
                font_size: 24.0,
                display_mode: true,
                color: Color::BLACK,
                font_family: "serif",
                style_override: None,
            },
        );
        assert!(metrics.width > 50.0);
        assert!(metrics.height() > 20.0);
    }

    #[test]
    fn malformed_latex_never_panics() {
        let mut text = piet_cairo::CairoText::new();
        for bad in [
            r"\frac{a",
            r"\sqrt[",
            r"\left( x",
            r"\begin{pmatrix} 1 & 2",
            r"x^",
            r"\unknowncmd{abc}",
            "",
        ] {
            let m = measure_latex(&mut text, bad, 20.0, false, "serif");
            assert!(m.width >= 0.0);
            assert!(m.height() > 0.0);
        }
    }
}
