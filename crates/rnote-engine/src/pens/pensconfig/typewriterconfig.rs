// Imports
use crate::strokes::textstroke::TextStyle;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(default, rename = "typewriter_config")]
pub struct TypewriterConfig {
    #[serde(rename = "text_style")]
    pub text_style: TextStyle,
    #[serde(rename = "text_width")]
    text_width: f64,
}

impl Default for TypewriterConfig {
    fn default() -> Self {
        let mut text_style = TextStyle::default();
        text_style.markdown_enabled = true;
        Self {
            text_style,
            text_width: Self::TEXT_WIDTH_DEFAULT,
        }
    }
}

impl TypewriterConfig {
    pub const TEXT_WIDTH_DEFAULT: f64 = 600.;

    pub fn text_width(&self) -> f64 {
        self.text_width
    }

    pub fn set_text_width(&mut self, text_width: f64) {
        self.text_width = text_width.max(0.);
    }
}
