use crate::{DeserializeResult, SerializeResult};
use alloc::{
    borrow::ToOwned,
    boxed::Box,
    collections::BTreeMap,
    fmt, format,
    string::{String, ToString},
    vec::Vec,
};
use serde::de::{Error, IntoDeserializer, MapAccess, Visitor};
use serde::ser::SerializeMap;
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;

#[derive(Clone, Debug, PartialEq)]
pub enum Chat<'a> {
    Text(TextComponent<'a>),
    Translation(TranslationComponent<'a>),
    Keybind(KeybindComponent<'a>),
    Score(ScoreComponent<'a>),
}

impl<'a> Chat<'a> {
    pub fn base(&self) -> &BaseComponent<'a> {
        use Chat::*;

        match self {
            Text(body) => &body.base,
            Translation(body) => &body.base,
            Keybind(body) => &body.base,
            Score(body) => &body.base,
        }
    }

    pub fn siblings(&self) -> &[Chat] {
        &self.base().extra
    }

    pub fn from_text(text: &str) -> Chat<'_> {
        Chat::Text(TextComponent {
            base: BaseComponent::default(),
            text: Cow::Borrowed(text),
        })
    }

    pub fn from_traditional(orig: &str, translate_colorcodes: bool) -> Chat<'_> {
        TraditionalParser::new(orig, translate_colorcodes).parse()
    }

    pub fn to_traditional(&self) -> Option<String> {
        use Chat::*;

        match self {
            Text(body) => Some(body.to_traditional()),
            _ => None,
        }
    }
}

struct TraditionalParser<'a> {
    source: Vec<char>,
    at: usize,
    translate_colorcodes: bool,

    // state
    text: String,
    color: Option<ColorCode>,
    bold: bool,
    italic: bool,
    underlined: bool,
    strikethrough: bool,
    obfuscated: bool,

    // all the parts we've already seen
    done: Vec<TextComponent<'a>>,
}

impl<'a> TraditionalParser<'a> {
    fn new(source: &str, translate_colorcodes: bool) -> Self {
        Self {
            source: source.chars().collect(),
            at: 0,
            translate_colorcodes,

            text: String::new(),
            color: None,
            bold: false,
            italic: false,
            underlined: false,
            strikethrough: false,
            obfuscated: false,

            done: Vec::new(),
        }
    }

    fn parse(mut self) -> Chat<'a> {
        loop {
            if let Some(formatter) = self.consume_formatter() {
                self.handle_formatter(formatter)
            } else if let Some(next) = self.consume_char() {
                self.push_next(next)
            } else {
                return self.finalize();
            }
        }
    }

    fn handle_formatter(&mut self, formatter: Formatter) {
        use Formatter::*;

        if self.has_text() {
            self.finish_current();
        }

        match formatter {
            Color(color) => {
                self.finish_current();
                self.color = Some(color);
            }
            Obfuscated => self.obfuscated = true,
            Bold => self.bold = true,
            Strikethrough => self.strikethrough = true,
            Underline => self.underlined = true,
            Italic => self.italic = true,
            _ => {}
        }
    }

    fn push_next(&mut self, next: char) {
        self.text.push(next);
    }

    fn finish_current(&mut self) {
        if self.has_text() {
            let current = TextComponent {
                text: Cow::Owned(std::mem::take(&mut self.text)),
                base: BaseComponent {
                    color: self.color.clone(),
                    bold: self.bold,
                    italic: self.italic,
                    underlined: self.underlined,
                    strikethrough: self.strikethrough,
                    obfuscated: self.obfuscated,
                    hover_event: None,
                    click_event: None,
                    insertion: None,
                    extra: Vec::default(),
                },
            };
            self.done.push(current);
        }

        self.reset_style();
    }

    fn reset_style(&mut self) {
        self.bold = false;
        self.italic = false;
        self.underlined = false;
        self.strikethrough = false;
        self.obfuscated = false;
        self.color = None;
    }

    fn has_text(&self) -> bool {
        return !self.text.is_empty();
    }

    fn is_on_formatter(&self) -> bool {
        self.source
            .get(self.at)
            .map(move |c| {
                let c = *c;
                c == SECTION_SYMBOL || (self.translate_colorcodes && c == '&')
            })
            .unwrap_or(false)
    }

    fn consume_char(&mut self) -> Option<char> {
        if let Some(c) = self.source.get(self.at) {
            self.at += 1;
            Some(*c)
        } else {
            None
        }
    }

    fn consume_formatter(&mut self) -> Option<Formatter> {
        if self.is_on_formatter() {
            self.consume_char()?;
            let c = self.consume_char()?;
            let out = Formatter::from_code(&c);
            if out.is_none() {
                self.at -= 1;
            }

            out
        } else {
            None
        }
    }

    fn finalize(mut self) -> Chat<'a> {
        self.finish_current();
        self.simplify();
        let n_components = self.done.len();
        if n_components == 1 {
            return Chat::Text(self.done.remove(0));
        }

        let mut top_level = TextComponent {
            text: Default::default(),
            base: BaseComponent::default(),
        };

        if n_components > 0 {
            top_level.base.extra.extend(
                self.done
                    .into_iter()
                    .map(move |component| Chat::Text(component)),
            );
        }

        Chat::Text(top_level)
    }

    fn simplify(&mut self) {
        let mut updated = Vec::with_capacity(self.done.len());
        let mut last: Option<TextComponent<'a>> = None;
        while !self.done.is_empty() {
            let cur = self.done.remove(0);
            if let Some(mut la) = last.take() {
                if la.base.has_same_style_as(&cur.base) {
                    la.text.to_mut().extend(cur.text.chars());
                    last = Some(la);
                    continue;
                } else {
                    updated.push(la);
                }
            }

            last = Some(cur);
        }

        if let Some(l) = last.take() {
            updated.push(l);
        }

        self.done = updated;
    }
}

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct BaseComponent<'a> {
    #[serde(skip_serializing_if = "should_skip_flag_field")]
    pub bold: bool,
    #[serde(skip_serializing_if = "should_skip_flag_field")]
    pub italic: bool,
    #[serde(skip_serializing_if = "should_skip_flag_field")]
    pub underlined: bool,
    #[serde(skip_serializing_if = "should_skip_flag_field")]
    pub strikethrough: bool,
    #[serde(skip_serializing_if = "should_skip_flag_field")]
    pub obfuscated: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub color: Option<ColorCode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub insertion: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub click_event: Option<ChatClickEvent<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hover_event: Option<ChatHoverEvent<'a>>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub extra: Vec<Chat<'a>>,
}

fn should_skip_flag_field(flag: &bool) -> bool {
    !*flag
}

impl<'a> BaseComponent<'a> {
    fn has_same_style_as(&self, other: &Self) -> bool {
        other.bold == self.bold
            && other.italic == self.italic
            && other.underlined == self.underlined
            && other.strikethrough == self.strikethrough
            && other.obfuscated == self.obfuscated
            && other.color.eq(&self.color)
    }
}

impl<'a> Into<BaseComponent<'a>> for JsonComponentBase<'a> {
    fn into(self) -> BaseComponent<'a> {
        BaseComponent {
            bold: self.bold.unwrap_or(false),
            italic: self.italic.unwrap_or(false),
            underlined: self.underlined.unwrap_or(false),
            strikethrough: self.strikethrough.unwrap_or(false),
            obfuscated: self.obfuscated.unwrap_or(false),
            color: self.color,
            insertion: self.insertion,
            click_event: self.click_event,
            hover_event: self.hover_event,
            extra: self.extra,
        }
    }
}

#[derive(Deserialize)]
struct JsonComponentBase<'a> {
    pub bold: Option<bool>,
    pub italic: Option<bool>,
    pub underlined: Option<bool>,
    pub strikethrough: Option<bool>,
    pub obfuscated: Option<bool>,
    pub color: Option<ColorCode>,
    pub insertion: Option<&'a str>,
    #[serde(rename = "clickEvent")]
    pub click_event: Option<ChatClickEvent<'a>>,
    #[serde(rename = "hoverEvent")]
    pub hover_event: Option<ChatHoverEvent<'a>>,
    #[serde(default = "Vec::default")]
    pub extra: Vec<Chat<'a>>,

    #[serde(flatten)]
    _additional: BTreeMap<String, serde_json::Value>,
}

impl<'a> Default for BaseComponent<'a> {
    fn default() -> Self {
        Self {
            bold: false,
            italic: false,
            underlined: false,
            strikethrough: false,
            obfuscated: false,
            color: None,
            insertion: None,
            click_event: None,
            hover_event: None,
            extra: Vec::default(),
        }
    }
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq)]
pub struct TextComponent<'a> {
    pub text: Cow<'a, str>,

    #[serde(flatten)]
    #[serde(skip_deserializing)]
    pub base: BaseComponent<'a>,
}

impl<'a> TextComponent<'a> {
    pub fn to_traditional(&self) -> String {
        let b = &self.base;
        let text = &self.text;

        let (mut buf, has_formatters) = if !text.is_empty() {
            let formatters = self.traditional_formatters(false);
            let has_formatters = formatters.is_some();
            let mut buf = formatters.unwrap_or_else(|| String::new());
            buf.extend(text.chars());
            (buf, has_formatters)
        } else {
            (String::default(), false)
        };

        let mut last_had_formatters = has_formatters;
        for extra in b.extra.iter() {
            if let Chat::Text(child) = extra {
                match child.traditional_formatters(last_had_formatters) {
                    Some(child_fmts) => {
                        last_had_formatters = true;
                        buf.extend(child_fmts.chars())
                    }
                    None => {
                        last_had_formatters = false;
                    }
                }

                buf.extend(child.text.chars());
            }
        }

        buf
    }

    fn traditional_formatters(&self, prev_colored: bool) -> Option<String> {
        let b = &self.base;
        let mut buf = String::default();

        if let Some(c) = b.color {
            buf.push(SECTION_SYMBOL);
            buf.push(c.code());
        }

        let mut apply_formatter = |b: bool, formatter: Formatter| {
            if b {
                buf.push(SECTION_SYMBOL);
                buf.push(formatter.code());
            }
        };

        apply_formatter(b.bold, Formatter::Bold);
        apply_formatter(b.italic, Formatter::Italic);
        apply_formatter(b.strikethrough, Formatter::Strikethrough);
        apply_formatter(b.underlined, Formatter::Underline);
        apply_formatter(b.obfuscated, Formatter::Obfuscated);

        if buf.is_empty() {
            if prev_colored {
                buf.push(SECTION_SYMBOL);
                buf.push('r');
                Some(buf)
            } else {
                None
            }
        } else {
            Some(buf)
        }
    }
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq)]
pub struct TranslationComponent<'a> {
    pub translate: &'a str,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub with: Vec<Chat<'a>>,

    #[serde(flatten)]
    #[serde(skip_deserializing)]
    pub base: BaseComponent<'a>,
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq)]
pub struct KeybindComponent<'a> {
    pub keybind: &'a str,

    #[serde(flatten)]
    #[serde(skip_deserializing)]
    pub base: BaseComponent<'a>,
}
#[derive(Deserialize, Serialize, Clone, Debug, PartialEq)]
pub struct ScoreComponent<'a> {
    #[serde(borrow)]
    pub score: ScoreComponentObjective<'a>,

    #[serde(flatten)]
    #[serde(skip_deserializing)]
    pub base: BaseComponent<'a>,
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq)]
pub struct ScoreComponentObjective<'a> {
    pub name: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub objective: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub value: Option<&'a str>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ChatClickEvent<'a> {
    OpenUrl(&'a str),
    RunCommand(&'a str),
    SuggestCommand(&'a str),
    ChangePage(i32),
}

impl<'a> Serialize for ChatClickEvent<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        let mut m = serializer.serialize_map(Some(2))?;

        use ChatClickEvent::*;

        m.serialize_entry(
            "action",
            match self {
                OpenUrl(_) => "open_url",
                RunCommand(_) => "run_command",
                SuggestCommand(_) => "suggest_command",
                ChangePage(_) => "change_page",
            },
        )?;

        m.serialize_key("value")?;

        match self {
            OpenUrl(body) => m.serialize_value(body),
            RunCommand(body) => m.serialize_value(body),
            SuggestCommand(body) => m.serialize_value(body),
            ChangePage(body) => m.serialize_value(body),
        }?;

        m.end()
    }
}

impl<'de: 'a, 'a> Deserialize<'de> for ChatClickEvent<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where
        D: Deserializer<'de>,
    {
        struct V;

        impl<'de> Visitor<'de> for V {
            type Value = ChatClickEvent<'de>;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "an event object for ChatClickEvent")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, <A as MapAccess<'de>>::Error>
            where
                A: MapAccess<'de>,
            {
                let (action, value) = read_event(&mut map)?;

                use ChatClickEvent::*;
                match action {
                    "open_url" => match value.as_str() {
                        Some(url) => Ok(OpenUrl(url)),
                        None => Err(A::Error::custom(format!(
                            "open_url requires string body, got {}",
                            value
                        ))),
                    },
                    "run_command" => match value.as_str() {
                        Some(cmd) => Ok(RunCommand(cmd)),
                        None => Err(A::Error::custom(format!(
                            "run_command requires string body, got {}",
                            value
                        ))),
                    },
                    "suggest_command" => match value.as_str() {
                        Some(cmd) => Ok(SuggestCommand(cmd)),
                        None => Err(A::Error::custom(format!(
                            "suggest_command requires string body, got {}",
                            value
                        ))),
                    },
                    "change_page" => match value.as_i64() {
                        Some(v) => Ok(ChangePage(v as i32)),
                        None => Err(A::Error::custom(format!(
                            "change_page requires integer body, got {}",
                            value
                        ))),
                    },
                    other => Err(A::Error::custom(format!(
                        "invalid click action kind {}",
                        other
                    ))),
                }
            }
        }

        deserializer.deserialize_map(V)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ChatHoverEvent<'a> {
    ShowText(Box<Chat<'a>>),
    ShowItem(Value),
    ShowEntity(Value),
}

impl<'a> Serialize for ChatHoverEvent<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        let mut m = serializer.serialize_map(Some(2))?;

        use ChatHoverEvent::*;

        m.serialize_entry(
            "action",
            match self {
                ShowText(_) => "show_text",
                ShowItem(_) => "show_item",
                ShowEntity(_) => "show_entity",
            },
        )?;

        m.serialize_key("value")?;

        match self {
            ShowText(body) => m.serialize_value(body),
            ShowItem(body) => m.serialize_value(body),
            ShowEntity(body) => m.serialize_value(body),
        }?;

        m.end()
    }
}

impl<'de: 'a, 'a> Deserialize<'de> for ChatHoverEvent<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where
        D: Deserializer<'de>,
    {
        struct V;

        impl<'de> Visitor<'de> for V {
            type Value = ChatHoverEvent<'de>;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "an event object for ChatClickEvent")
            }

            //noinspection ALL
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, <A as MapAccess<'de>>::Error>
            where
                A: MapAccess<'de>,
            {
                let (action, value) = read_event(&mut map)?;

                use ChatHoverEvent::*;
                match action {
                    "show_text" => Ok(ShowText(Box::new(
                        Chat::deserialize(value.into_deserializer()).map_err(move |err| {
                            A::Error::custom(format!("error deserializing text to show {:?}", err))
                        })?,
                    ))),
                    "show_item" => Ok(ShowItem(value)),
                    "show_entity" => Ok(ShowEntity(value)),
                    other => Err(A::Error::custom(format!(
                        "invalid hover action kind {}",
                        other
                    ))),
                }
            }
        }

        deserializer.deserialize_map(V)
    }
}

pub const SECTION_SYMBOL: char = '§';

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
pub enum ColorCode {
    Black,
    DarkBlue,
    DarkGreen,
    DarkAqua,
    DarkRed,
    DarkPurple,
    Gold,
    Gray,
    DarkGray,
    Blue,
    Green,
    Aqua,
    Red,
    LightPurple,
    Yellow,
    White,
}

impl ColorCode {
    pub fn from_code(i: &char) -> Option<Self> {
        match i {
            '0' => Some(ColorCode::Black),
            '1' => Some(ColorCode::DarkBlue),
            '2' => Some(ColorCode::DarkGreen),
            '3' => Some(ColorCode::DarkAqua),
            '4' => Some(ColorCode::DarkRed),
            '5' => Some(ColorCode::DarkPurple),
            '6' => Some(ColorCode::Gold),
            '7' => Some(ColorCode::Gray),
            '8' => Some(ColorCode::DarkGray),
            '9' => Some(ColorCode::Blue),
            'a' => Some(ColorCode::Green),
            'b' => Some(ColorCode::Aqua),
            'c' => Some(ColorCode::Red),
            'd' => Some(ColorCode::LightPurple),
            'e' => Some(ColorCode::Yellow),
            'f' => Some(ColorCode::White),
            _ => None,
        }
    }

    pub fn code(&self) -> char {
        match self {
            ColorCode::Black => '0',
            ColorCode::DarkBlue => '1',
            ColorCode::DarkGreen => '2',
            ColorCode::DarkAqua => '3',
            ColorCode::DarkRed => '4',
            ColorCode::DarkPurple => '5',
            ColorCode::Gold => '6',
            ColorCode::Gray => '7',
            ColorCode::DarkGray => '8',
            ColorCode::Blue => '9',
            ColorCode::Green => 'a',
            ColorCode::Aqua => 'b',
            ColorCode::Red => 'c',
            ColorCode::LightPurple => 'd',
            ColorCode::Yellow => 'e',
            ColorCode::White => 'f',
        }
    }

    pub fn from_name(name: &str) -> Option<Self> {
        match name.to_ascii_lowercase().as_str() {
            "black" => Some(ColorCode::Black),
            "dark_blue" => Some(ColorCode::DarkBlue),
            "dark_green" => Some(ColorCode::DarkGreen),
            "dark_aqua" => Some(ColorCode::DarkAqua),
            "dark_red" => Some(ColorCode::DarkRed),
            "dark_purple" => Some(ColorCode::DarkPurple),
            "gold" => Some(ColorCode::Gold),
            "gray" => Some(ColorCode::Gray),
            "dark_gray" => Some(ColorCode::DarkGray),
            "blue" => Some(ColorCode::Blue),
            "green" => Some(ColorCode::Green),
            "aqua" => Some(ColorCode::Aqua),
            "red" => Some(ColorCode::Red),
            "light_purple" => Some(ColorCode::LightPurple),
            "yellow" => Some(ColorCode::Yellow),
            "white" => Some(ColorCode::White),
            _ => None,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            ColorCode::Black => "black",
            ColorCode::DarkBlue => "dark_blue",
            ColorCode::DarkGreen => "dark_green",
            ColorCode::DarkAqua => "dark_aqua",
            ColorCode::DarkRed => "dark_red",
            ColorCode::DarkPurple => "dark_purple",
            ColorCode::Gold => "gold",
            ColorCode::Gray => "gray",
            ColorCode::DarkGray => "dark_gray",
            ColorCode::Blue => "blue",
            ColorCode::Green => "green",
            ColorCode::Aqua => "aqua",
            ColorCode::Red => "red",
            ColorCode::LightPurple => "light_purple",
            ColorCode::Yellow => "yellow",
            ColorCode::White => "white",
        }
    }
}

impl fmt::Display for ColorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", SECTION_SYMBOL, self.code())
    }
}

impl Serialize for ColorCode {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.name())
    }
}

impl<'de> Deserialize<'de> for ColorCode {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where
        D: Deserializer<'de>,
    {
        struct V;

        impl<'de> Visitor<'de> for V {
            type Value = ColorCode;

            fn expecting(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(fmt, "a string representing a color code")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                if let Some(code) = ColorCode::from_name(v) {
                    Ok(code)
                } else {
                    Err(E::custom(format!("invalid color code name {}", v)))
                }
            }
        }

        deserializer.deserialize_str(V)
    }
}

#[derive(PartialOrd, PartialEq, Debug, Copy, Clone)]
pub enum Formatter {
    Color(ColorCode),
    Obfuscated,
    Bold,
    Strikethrough,
    Underline,
    Italic,
    Reset,
}

impl Formatter {
    pub fn from_code(i: &char) -> Option<Self> {
        match i.to_ascii_lowercase() {
            'k' => Some(Formatter::Obfuscated),
            'l' => Some(Formatter::Bold),
            'm' => Some(Formatter::Strikethrough),
            'n' => Some(Formatter::Underline),
            'o' => Some(Formatter::Italic),
            'r' => Some(Formatter::Reset),
            _ => ColorCode::from_code(i).map(Formatter::Color),
        }
    }

    pub fn code(&self) -> char {
        match self {
            Formatter::Color(c) => c.code(),
            Formatter::Obfuscated => 'k',
            Formatter::Bold => 'l',
            Formatter::Strikethrough => 'm',
            Formatter::Underline => 'n',
            Formatter::Italic => 'o',
            Formatter::Reset => 'r',
        }
    }

    pub fn from_name(name: &str) -> Option<Self> {
        match name.to_ascii_lowercase().as_str() {
            "obfuscated" => Some(Formatter::Obfuscated),
            "bold" => Some(Formatter::Bold),
            "strikethrough" => Some(Formatter::Strikethrough),
            "underline" => Some(Formatter::Underline),
            "italic" => Some(Formatter::Italic),
            "reset" => Some(Formatter::Reset),
            _ => ColorCode::from_name(name).map(Formatter::Color),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Formatter::Obfuscated => "obfuscated",
            Formatter::Bold => "bold",
            Formatter::Strikethrough => "strikethrough",
            Formatter::Underline => "underline",
            Formatter::Italic => "italic",
            Formatter::Reset => "reset",
            Formatter::Color(c) => c.name(),
        }
    }
}

impl fmt::Display for Formatter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", SECTION_SYMBOL, self.code())
    }
}

impl<'de: 'a, 'a> Deserialize<'de> for Chat<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where
        D: Deserializer<'de>,
    {
        struct V;

        impl<'de> Visitor<'de> for V {
            type Value = Chat<'de>;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(
                    formatter,
                    "any primitive or a JSON object specifying the component"
                )
            }

            fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                self.visit_string(value.to_string())
            }

            fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                self.visit_string(value.to_string())
            }

            fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                self.visit_string(value.to_string())
            }

            fn visit_str<E>(self, value: &'de str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Chat::Text(TextComponent {
                    base: BaseComponent::default(),
                    text: Cow::Borrowed(value),
                }))
            }

            fn visit_map<M>(self, map: M) -> Result<Self::Value, M::Error>
            where
                M: de::MapAccess<'de>,
            {
                let mut base: JsonComponentBase =
                    de::Deserialize::deserialize(de::value::MapAccessDeserializer::new(map))?;
                let additional = &mut base._additional;

                // string component
                if let Some(raw_text) = additional.remove("text") {
                    return if let Some(text) = raw_text.as_str() {
                        additional.clear();
                        Ok(Chat::Text(TextComponent {
                            text: Cow::Borrowed(text),
                            base: base.into(),
                        }))
                    } else {
                        Err(M::Error::custom(format!(
                            "have text but it's not a string - {:?}",
                            raw_text
                        )))
                    };
                }

                // translate
                if let Some(raw_translate) = additional.remove("translate") {
                    return if let Some(translate) = raw_translate.as_str() {
                        // need "with"
                        if let Some(raw_with) = additional.remove("with") {
                            if let Some(withs) = raw_with.as_array() {
                                let mut withs_out = Vec::with_capacity(withs.len());
                                for with in withs {
                                    withs_out.push(with.deserialize_any(V)
                                        .map_err(move |err| M::Error::custom(
                                            format!("unable to parse one of the translation with entries :: {}", err)))?);
                                }
                                Ok(Chat::Translation(TranslationComponent {
                                    base: base.into(),
                                    translate,
                                    with: withs_out,
                                }))
                            } else {
                                Err(M::Error::custom(format!(
                                    "have with but it's not an array - {:?}",
                                    raw_with
                                )))
                            }
                        } else {
                            Ok(Chat::Translation(TranslationComponent {
                                base: base.into(),
                                translate,
                                with: Vec::default(),
                            }))
                        }
                    } else {
                        Err(M::Error::custom(format!(
                            "have translate but it's not a string - {:?}",
                            raw_translate
                        )))
                    };
                }

                // keybind
                if let Some(raw_keybind) = additional.remove("keybind") {
                    return if let Some(keybind) = raw_keybind.as_str() {
                        Ok(Chat::Keybind(KeybindComponent {
                            keybind,
                            base: base.into(),
                        }))
                    } else {
                        Err(M::Error::custom(format!(
                            "have keybind but it's not a string! {:?}",
                            raw_keybind
                        )))
                    };
                }

                // score
                if let Some(raw_score) = additional.remove("score") {
                    let score = ScoreComponentObjective::deserialize(raw_score.into_deserializer())
                        .map_err(move |err| M::Error::custom(
                            format!("failed to deserialize scoreboard objective for score chat component :: {:?}", err)))?;

                    return Ok(Chat::Score(ScoreComponent {
                        score,
                        base: base.into(),
                    }));
                }

                // selector (SKIP)

                Err(M::Error::custom(
                    "not able to parse chat component, not a valid chat component kind",
                ))
            }
        }

        deserializer.deserialize_any(V)
    }
}

impl<'a> Serialize for Chat<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        use Chat::*;

        match self {
            Text(body) => body.serialize(serializer),
            Translation(body) => body.serialize(serializer),
            Keybind(body) => body.serialize(serializer),
            Score(body) => body.serialize(serializer),
        }
    }
}

impl<'a> super::Serialize for Chat<'a> {
    fn mc_serialize<S: super::Serializer>(&self, to: &mut S) -> SerializeResult {
        serde_json::to_string(self)
            .map_err(move |err| {
                super::SerializeErr::FailedJsonEncode(format!(
                    "error while encoding chat :: {:?} -> {:?}",
                    self, err
                ))
            })?
            .mc_serialize(to)
    }
}

impl<'a> super::Deserialize for Chat<'a> {
    fn mc_deserialize(data: &'a [u8]) -> DeserializeResult<'a, Self> {
        //let s: DeserializeResult<'a, &'a str> = super::Deserialize::mc_deserialize(data);
        super::Deserialize::mc_deserialize(data)?.try_map(|raw: &str| {
            serde_json::from_str(raw).map_err(move |err| {
                super::DeserializeErr::FailedJsonDeserialize(format!(
                    "failed to deserialize chat from JSON '{}' :: {:?}",
                    raw, err
                ))
            })
        })
    }
}

#[cfg(all(test, feature = "std"))]
use super::protocol::TestRandom;
use std::borrow::Cow;

#[cfg(all(test, feature = "std"))]
impl TestRandom for Chat {
    fn test_gen_random() -> Self {
        let str = String::test_gen_random();
        Chat::from_text(str.as_str())
    }
}

fn read_event<'de, A>(access: &mut A) -> Result<(&'de str, Value), <A as MapAccess<'de>>::Error>
where
    A: MapAccess<'de>,
{
    let mut action: Option<&str> = None;
    let mut value: Option<Value> = None;
    while action.is_none() || value.is_none() {
        if let Some(key) = access.next_key()? {
            match key {
                "action" => {
                    action = access.next_value()?;
                    if action.is_none() {
                        return Err(A::Error::custom("none for value key=action"));
                    }
                }
                "value" => {
                    value = access.next_value()?;
                    if value.is_none() {
                        return Err(A::Error::custom("none for value key=value"));
                    }
                }
                other => {
                    return Err(A::Error::custom(format!(
                        "unexpected key in event {}",
                        other
                    )));
                }
            }
        } else {
            return Err(A::Error::custom(format!("event needs action and value")));
        }
    }

    Ok((action.expect("checked"), value.expect("checked")))
}

#[cfg(test)]
pub mod tests {

    use super::*;

    #[test]
    fn test_from_traditional_simple() {
        let out = Chat::from_traditional("&cthis &cis red, and &rthis is &e&lyellow", true);
        assert_eq!(
            out,
            Chat::Text(TextComponent {
                text: Default::default(),
                base: {
                    let mut b = BaseComponent::default();
                    b.extra = alloc::vec!(
                        Chat::Text(TextComponent {
                            text: Cow::Borrowed("this is red, and "),
                            base: {
                                let mut b = BaseComponent::default();
                                b.color = Some(ColorCode::Red);
                                b
                            },
                        }),
                        Chat::Text(TextComponent {
                            text: Cow::Borrowed("this is "),
                            base: BaseComponent::default(),
                        }),
                        Chat::Text(TextComponent {
                            text: Cow::Borrowed("yellow"),
                            base: {
                                let mut b = BaseComponent::default();
                                b.color = Some(ColorCode::Yellow);
                                b.bold = true;
                                b
                            }
                        })
                    );
                    b
                }
            })
        );

        let traditional = out.to_traditional().expect("is text");
        assert_eq!(
            traditional.as_str(),
            "§cthis is red, and §rthis is §e§lyellow"
        );
        #[cfg(feature = "std")]
        println!(
            "{}",
            serde_json::to_string_pretty(&out).expect("should serialize fine")
        );
    }
}
