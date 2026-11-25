// Imports
use super::RnCanvas;
use gtk4::{Native, gdk, glib, graphene, prelude::*, subclass::prelude::ObjectSubclassIsExt};
use p2d::math::Vector2;
use rnote_compose::penevent::{KeyboardKey, ModifierKey, PenEvent, PenState, ShortcutKey};
use rnote_compose::penpath::Element;
use rnote_engine::WidgetFlags;
use rnote_engine::ext::EventPropagationExt;
use rnote_engine::ext::GraphenePointExt;
use rnote_engine::pens::PenMode;
use rnote_engine::pens::penholder::BacklogPolicy;
use std::collections::HashSet;
use std::time::{Duration, Instant};
use tracing::trace;

/// Handles a pointer controller event.
///
/// Returns:
/// - the propagation of the event, which is `glib::Propagation::Stop` when the event
///   should be inhibited from propagating
/// - the new pen state
/// - whether a stylus input session is active
pub(crate) fn handle_pointer_controller_event(
    canvas: &RnCanvas,
    event: &gdk::Event,
    mut pen_state: PenState,
    mut stylus_active: bool,
) -> (glib::Propagation, PenState, bool) {
    let now = Instant::now();
    let mut widget_flags = WidgetFlags::default();
    let touch_drawing = canvas.touch_drawing();
    let gdk_event_type = event.event_type();
    let gdk_modifiers = event.modifier_state();
    let _gdk_device = event.device().unwrap();
    let backlog_policy = canvas.engine_ref().penholder.backlog_policy();
    let is_stylus = event_is_stylus(event);

    //std::thread::sleep(std::time::Duration::from_millis(100));
    //super::input::debug_gdk_event(event);

    if reject_pointer_input(event, touch_drawing) {
        return (glib::Propagation::Proceed, pen_state, stylus_active);
    }

    // GTK can deliver mouse motion events while a stylus is hovering or drawing.
    // They must not reuse the stylus state, otherwise the pen position jumps to
    // the last mouse position and generates an unwanted pen event.
    if is_stylus {
        stylus_active = gdk_event_type != gdk::EventType::ProximityOut;
    } else if stylus_active {
        // A deliberate mouse click hands control back to the mouse.
        match gdk_event_type {
            gdk::EventType::ButtonPress | gdk::EventType::ButtonRelease => {
                stylus_active = false;
            }
            _ => return (glib::Propagation::Proceed, pen_state, stylus_active),
        }
    }

    let mut handle_pen_event = false;
    let mut propagation = glib::Propagation::Proceed;

    match gdk_event_type {
        gdk::EventType::MotionNotify => {
            trace!(
                "canvas event MotionNotify - gdk_modifiers: {gdk_modifiers:?}, is_stylus: {is_stylus}"
            );
            handle_pen_event = true;

            if is_stylus {
                // like in gtk4 'gesturestylus.c:120' stylus proximity is detected this way,
                // in case ProximityIn & ProximityOut is not reported.
                if gdk_modifiers.contains(gdk::ModifierType::BUTTON1_MASK) {
                    pen_state = PenState::Down;
                } else {
                    pen_state = PenState::Proximity;
                }
            }
        }
        gdk::EventType::ButtonPress => {
            let button_event = event.downcast_ref::<gdk::ButtonEvent>().unwrap();
            let gdk_button = button_event.button();
            let mut handle_shortcut_key = false;

            trace!("canvas event ButtonPress - gdk_button: {gdk_button}, is_stylus: {is_stylus}");

            if is_stylus {
                if gdk_button == gdk::BUTTON_PRIMARY
                    || gdk_button == gdk::BUTTON_SECONDARY
                    || gdk_button == gdk::BUTTON_MIDDLE
                {
                    handle_pen_event = true;
                    handle_shortcut_key = true;
                }
            } else {
                #[allow(clippy::collapsible_else_if)]
                if gdk_button == gdk::BUTTON_PRIMARY || gdk_button == gdk::BUTTON_SECONDARY {
                    handle_pen_event = true;
                    handle_shortcut_key = true;
                    pen_state = PenState::Down;
                }
            }

            if handle_shortcut_key {
                let shortcut_key = retrieve_button_shortcut_key(gdk_button, is_stylus);

                if let Some(shortcut_key) = shortcut_key {
                    let (ep, wf) = canvas
                        .engine_mut()
                        .handle_pressed_shortcut_key(shortcut_key, now);
                    widget_flags |= wf;
                    propagation = ep.into_glib();
                }
            }
        }
        gdk::EventType::ButtonRelease => {
            let button_event = event.downcast_ref::<gdk::ButtonEvent>().unwrap();
            let gdk_button = button_event.button();

            trace!("canvas event ButtonRelease - gdk_button: {gdk_button}, is_stylus: {is_stylus}");

            if is_stylus {
                if gdk_button == gdk::BUTTON_PRIMARY
                    || gdk_button == gdk::BUTTON_SECONDARY
                    || gdk_button == gdk::BUTTON_MIDDLE
                {
                    handle_pen_event = true;
                }

                // again, this is the method to detect proximity on stylus.
                if gdk_button == gdk::BUTTON_PRIMARY {
                    pen_state = PenState::Up;
                } else {
                    // Workaround for https://github.com/flxzt/rnote/issues/785
                    // On window only one button release event is sent when the
                    // pen leaves the screen, and if the button is pressed this
                    // is not a gdk::BUTTON_PRIMARY
                    #[allow(clippy::collapsible_else_if)]
                    if cfg!(target_os = "windows") {
                        pen_state = PenState::Up;
                    } else {
                        pen_state = PenState::Proximity;
                    }
                }
            } else {
                #[allow(clippy::collapsible_else_if)]
                if gdk_button == gdk::BUTTON_PRIMARY || gdk_button == gdk::BUTTON_SECONDARY {
                    pen_state = PenState::Up;
                    handle_pen_event = true;
                }
            };
        }
        gdk::EventType::ProximityIn => {
            pen_state = PenState::Proximity;
            handle_pen_event = true;
        }
        gdk::EventType::ProximityOut => {
            pen_state = PenState::Up;
            handle_pen_event = true;
        }
        // We early-returned when detecting touch input and touch-drawing is not enabled,
        // so it is fine to always handle it here.
        gdk::EventType::TouchBegin => {
            pen_state = PenState::Down;
            handle_pen_event = true;
        }
        gdk::EventType::TouchUpdate => {
            pen_state = PenState::Down;
            handle_pen_event = true;
        }
        gdk::EventType::TouchEnd => {
            pen_state = PenState::Up;
            handle_pen_event = true;
        }
        gdk::EventType::TouchCancel => {
            pen_state = PenState::Up;
            handle_pen_event = true;
        }
        _ => {}
    };

    if handle_pen_event {
        let Some(elements) = retrieve_pointer_elements(canvas, now, event, backlog_policy) else {
            return (glib::Propagation::Proceed, pen_state, stylus_active);
        };
        let modifier_keys = retrieve_modifier_keys(event.modifier_state());
        let pen_mode = retrieve_pen_mode(event);

        for (element, event_time) in elements {
            trace!(?element, ?pen_state, ?modifier_keys, ?pen_mode, event_time_delta=?now.duration_since(event_time), msg="handle pen event element");

            // Workaround for https://github.com/flxzt/rnote/issues/785
            // only one event is sent when the pen approaches the screen
            // on Windows whereas rnote expects 2 (switch to proximity
            // then down). This forces the pen to be down when on the
            // screen
            #[cfg(target_os = "windows")]
            {
                if element.pressure > 0.0 && is_stylus {
                    pen_state = PenState::Down;
                }
            }

            match pen_state {
                PenState::Up => {
                    canvas.enable_drawing_cursor(false);

                    let (ep, wf) = canvas.engine_mut().handle_pen_event(
                        PenEvent::Up {
                            element,
                            modifier_keys: modifier_keys.clone(),
                        },
                        pen_mode,
                        event_time,
                    );
                    widget_flags |= wf;
                    propagation = ep.into_glib();
                }
                PenState::Proximity => {
                    canvas.enable_drawing_cursor(false);

                    let (ep, wf) = canvas.engine_mut().handle_pen_event(
                        PenEvent::Proximity {
                            element,
                            modifier_keys: modifier_keys.clone(),
                        },
                        pen_mode,
                        event_time,
                    );
                    widget_flags |= wf;
                    propagation = ep.into_glib();
                }
                PenState::Down => {
                    canvas.grab_focus();
                    canvas.enable_drawing_cursor(true);

                    let (ep, wf) = canvas.engine_mut().handle_pen_event(
                        PenEvent::Down {
                            element,
                            modifier_keys: modifier_keys.clone(),
                        },
                        pen_mode,
                        event_time,
                    );
                    widget_flags |= wf;
                    propagation = ep.into_glib();
                }
            }
        }
    }

    canvas.emit_handle_widget_flags(widget_flags);
    (propagation, pen_state, stylus_active)
}

pub(crate) fn handle_key_controller_key_pressed(
    canvas: &RnCanvas,
    gdk_key: gdk::Key,
    gdk_modifiers: gdk::ModifierType,
) -> glib::Propagation {
    trace!("canvas event key pressed - gdk_key: {gdk_key:?}, gdk_modifiers: {gdk_modifiers:?}");
    canvas.grab_focus();

    if canvas.imp().text_input_enabled.get()
        && !gdk_modifiers.intersects(gdk::ModifierType::CONTROL_MASK | gdk::ModifierType::ALT_MASK)
    {
        match process_dead_key(canvas.imp().dead_key_composer.get(), gdk_key) {
            DeadKeyAction::ConsumedPending(dk) => {
                canvas.imp().dead_key_composer.set(Some(dk));
                return glib::Propagation::Stop;
            }
            DeadKeyAction::CommitText(text) => {
                canvas.imp().dead_key_composer.set(None);
                handle_imcontext_text_commit(canvas, &text);
                return glib::Propagation::Stop;
            }
            DeadKeyAction::PassThrough { clear_pending } => {
                if clear_pending {
                    canvas.imp().dead_key_composer.set(None);
                }
            }
        }
    } else {
        canvas.imp().dead_key_composer.set(None);
    }

    let now = Instant::now();
    let keyboard_key = retrieve_keyboard_key(gdk_key);
    let modifier_keys = retrieve_modifier_keys(gdk_modifiers);
    let shortcut_key = retrieve_keyboard_shortcut_key(gdk_key, gdk_modifiers);

    let (propagation, widget_flags) = if let Some(shortcut_key) = shortcut_key {
        canvas
            .engine_mut()
            .handle_pressed_shortcut_key(shortcut_key, now)
    } else {
        canvas.engine_mut().handle_pen_event(
            PenEvent::KeyPressed {
                keyboard_key,
                modifier_keys,
            },
            None,
            now,
        )
    };

    canvas.emit_handle_widget_flags(widget_flags);
    propagation.into_glib()
}

pub(crate) fn handle_key_controller_key_released(
    _canvas: &RnCanvas,
    gdk_key: gdk::Key,
    gdk_modifiers: gdk::ModifierType,
) {
    trace!("canvas event key released - gdk_key: {gdk_key:?}, gdk_modifiers: {gdk_modifiers:?}");
}

pub(crate) fn handle_imcontext_text_commit(canvas: &RnCanvas, text: &str) {
    canvas.imp().dead_key_composer.set(None);
    let now = Instant::now();

    let (_ep, widget_flags) = canvas.engine_mut().handle_pen_event(
        PenEvent::Text {
            text: text.to_string(),
        },
        None,
        now,
    );
    canvas.emit_handle_widget_flags(widget_flags);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DeadKey {
    Acute,
    Grave,
    Circumflex,
    Tilde,
    Diaeresis,
    Cedilla,
    Ring,
    Caron,
    Macron,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum DeadKeyAction {
    PassThrough { clear_pending: bool },
    ConsumedPending(DeadKey),
    CommitText(String),
}

impl DeadKey {
    pub(crate) fn from_gdk_key(key: gdk::Key) -> Option<Self> {
        match key {
            gdk::Key::dead_acute => Some(Self::Acute),
            gdk::Key::dead_grave => Some(Self::Grave),
            gdk::Key::dead_circumflex => Some(Self::Circumflex),
            gdk::Key::dead_tilde => Some(Self::Tilde),
            gdk::Key::dead_diaeresis => Some(Self::Diaeresis),
            gdk::Key::dead_cedilla => Some(Self::Cedilla),
            gdk::Key::dead_abovering => Some(Self::Ring),
            gdk::Key::dead_caron => Some(Self::Caron),
            gdk::Key::dead_macron => Some(Self::Macron),
            _ => None,
        }
    }

    pub(crate) fn standalone_char(self) -> char {
        match self {
            Self::Acute => '´',
            Self::Grave => '`',
            Self::Circumflex => '^',
            Self::Tilde => '~',
            Self::Diaeresis => '¨',
            Self::Cedilla => '¸',
            Self::Ring => '°',
            Self::Caron => 'ˇ',
            Self::Macron => '¯',
        }
    }

    pub(crate) fn compose(self, ch: char) -> Option<char> {
        if ch == ' ' {
            return Some(self.standalone_char());
        }
        match (self, ch) {
            (Self::Acute, 'a') => Some('á'),
            (Self::Acute, 'e') => Some('é'),
            (Self::Acute, 'i') => Some('í'),
            (Self::Acute, 'o') => Some('ó'),
            (Self::Acute, 'u') => Some('ú'),
            (Self::Acute, 'y') => Some('ý'),
            (Self::Acute, 'A') => Some('Á'),
            (Self::Acute, 'E') => Some('É'),
            (Self::Acute, 'I') => Some('Í'),
            (Self::Acute, 'O') => Some('Ó'),
            (Self::Acute, 'U') => Some('Ú'),
            (Self::Acute, 'Y') => Some('Ý'),
            (Self::Acute, 'c') => Some('ć'),
            (Self::Acute, 'C') => Some('Ć'),
            (Self::Acute, 'n') => Some('ń'),
            (Self::Acute, 'N') => Some('Ń'),
            (Self::Acute, 's') => Some('ś'),
            (Self::Acute, 'S') => Some('Ś'),
            (Self::Acute, 'z') => Some('ź'),
            (Self::Acute, 'Z') => Some('Ź'),

            (Self::Grave, 'a') => Some('à'),
            (Self::Grave, 'e') => Some('è'),
            (Self::Grave, 'i') => Some('ì'),
            (Self::Grave, 'o') => Some('ò'),
            (Self::Grave, 'u') => Some('ù'),
            (Self::Grave, 'A') => Some('À'),
            (Self::Grave, 'E') => Some('È'),
            (Self::Grave, 'I') => Some('Ì'),
            (Self::Grave, 'O') => Some('Ò'),
            (Self::Grave, 'U') => Some('Ù'),

            (Self::Circumflex, 'a') => Some('â'),
            (Self::Circumflex, 'e') => Some('ê'),
            (Self::Circumflex, 'i') => Some('î'),
            (Self::Circumflex, 'o') => Some('ô'),
            (Self::Circumflex, 'u') => Some('û'),
            (Self::Circumflex, 'A') => Some('Â'),
            (Self::Circumflex, 'E') => Some('Ê'),
            (Self::Circumflex, 'I') => Some('Î'),
            (Self::Circumflex, 'O') => Some('Ô'),
            (Self::Circumflex, 'U') => Some('Û'),

            (Self::Tilde, 'a') => Some('ã'),
            (Self::Tilde, 'n') => Some('ñ'),
            (Self::Tilde, 'o') => Some('õ'),
            (Self::Tilde, 'A') => Some('Ã'),
            (Self::Tilde, 'N') => Some('Ñ'),
            (Self::Tilde, 'O') => Some('Õ'),

            (Self::Diaeresis, 'a') => Some('ä'),
            (Self::Diaeresis, 'e') => Some('ë'),
            (Self::Diaeresis, 'i') => Some('ï'),
            (Self::Diaeresis, 'o') => Some('ö'),
            (Self::Diaeresis, 'u') => Some('ü'),
            (Self::Diaeresis, 'y') => Some('ÿ'),
            (Self::Diaeresis, 'A') => Some('Ä'),
            (Self::Diaeresis, 'E') => Some('Ë'),
            (Self::Diaeresis, 'I') => Some('Ï'),
            (Self::Diaeresis, 'O') => Some('Ö'),
            (Self::Diaeresis, 'U') => Some('Ü'),
            (Self::Diaeresis, 'Y') => Some('Ÿ'),

            (Self::Cedilla, 'c') => Some('ç'),
            (Self::Cedilla, 'C') => Some('Ç'),

            (Self::Ring, 'a') => Some('å'),
            (Self::Ring, 'A') => Some('Å'),

            (Self::Caron, 'c') => Some('č'),
            (Self::Caron, 's') => Some('š'),
            (Self::Caron, 'z') => Some('ž'),
            (Self::Caron, 'r') => Some('ř'),
            (Self::Caron, 'C') => Some('Č'),
            (Self::Caron, 'S') => Some('Š'),
            (Self::Caron, 'Z') => Some('Ž'),
            (Self::Caron, 'R') => Some('Ř'),

            (Self::Macron, 'a') => Some('ā'),
            (Self::Macron, 'e') => Some('ē'),
            (Self::Macron, 'i') => Some('ī'),
            (Self::Macron, 'o') => Some('ō'),
            (Self::Macron, 'u') => Some('ū'),
            (Self::Macron, 'A') => Some('Ā'),
            (Self::Macron, 'E') => Some('Ē'),
            (Self::Macron, 'I') => Some('Ī'),
            (Self::Macron, 'O') => Some('Ō'),
            (Self::Macron, 'U') => Some('Ū'),

            _ => None,
        }
    }
}

fn is_modifier_only_key(key: gdk::Key) -> bool {
    matches!(
        key,
        gdk::Key::Shift_L
            | gdk::Key::Shift_R
            | gdk::Key::Control_L
            | gdk::Key::Control_R
            | gdk::Key::Alt_L
            | gdk::Key::Alt_R
            | gdk::Key::Super_L
            | gdk::Key::Super_R
            | gdk::Key::ISO_Level3_Shift
            | gdk::Key::ISO_Level5_Shift
            | gdk::Key::Caps_Lock
    )
}

pub(crate) fn process_dead_key(pending: Option<DeadKey>, gdk_key: gdk::Key) -> DeadKeyAction {
    match pending {
        None => {
            if let Some(dk) = DeadKey::from_gdk_key(gdk_key) {
                DeadKeyAction::ConsumedPending(dk)
            } else {
                DeadKeyAction::PassThrough {
                    clear_pending: false,
                }
            }
        }
        Some(dk) => {
            if is_modifier_only_key(gdk_key) {
                return DeadKeyAction::PassThrough {
                    clear_pending: false,
                };
            }
            if DeadKey::from_gdk_key(gdk_key) == Some(dk) {
                return DeadKeyAction::CommitText(dk.standalone_char().to_string());
            }
            if gdk_key == gdk::Key::Escape {
                return DeadKeyAction::PassThrough {
                    clear_pending: true,
                };
            }
            if let Some(ch) = gdk_key.to_unicode()
                && !ch.is_control()
            {
                if let Some(composed) = dk.compose(ch) {
                    return DeadKeyAction::CommitText(composed.to_string());
                } else {
                    return DeadKeyAction::CommitText(format!("{}{}", dk.standalone_char(), ch));
                }
            }
            DeadKeyAction::PassThrough {
                clear_pending: true,
            }
        }
    }
}

#[allow(unused)]
fn trace_gdk_event(event: &gdk::Event) {
    let pos = event
        .position()
        .map(|(x, y)| format!("x: {x:.1}, y: {y:.1}"));
    trace!(
        msg="Gdk event",
        pos=?pos,
        device=?event.device(),
        modifier=?event.modifier_state(),
        event_type=?event.event_type(),
        tool_type=?event.device_tool().map(|t| t.tool_type()),
        input_source=?event.device().map(|d| d.source())
    );
}

/// Returns true if input should be rejected
pub(crate) fn reject_pointer_input(event: &gdk::Event, touch_drawing: bool) -> bool {
    if touch_drawing {
        if event.device().unwrap().num_touches() > 1 {
            return true;
        }
    } else {
        let event_type = event.event_type();
        if event.is_pointer_emulated()
            || event_type == gdk::EventType::TouchBegin
            || event_type == gdk::EventType::TouchUpdate
            || event_type == gdk::EventType::TouchEnd
            || event_type == gdk::EventType::TouchCancel
        {
            return true;
        }
    }
    false
}

fn event_is_stylus(event: &gdk::Event) -> bool {
    // As in gtk4 'gtkgesturestylus.c:106' we detect if the pointer is a stylus when it has a device tool
    event.device_tool().is_some()
}

fn retrieve_pointer_elements(
    canvas: &RnCanvas,
    now: Instant,
    event: &gdk::Event,
    backlog_policy: BacklogPolicy,
) -> Option<Vec<(Element, Instant)>> {
    // Retrieve the transform directly from the event, just like in `gtkgesturestylus.c`'s `get_backlog()`
    let event_native = Native::for_surface(&event.surface()?)?;
    let (surface_trans_x, surface_trans_y) = event_native.surface_transform();
    // retrieving the pressure only works when the event has a device tool (== is a stylus),
    // else we get SIGSEGV when trying to access (TODO: report this to gtk-rs)
    let is_stylus = event_is_stylus(event);
    let event_time = event.time();

    let mut elements = Vec::with_capacity(1);

    // Transforms the pos given in surface coordinate space to the canvas document coordinate space
    let transform_pos = |pos: Vector2| -> Vector2 {
        event_native
            .compute_point(
                canvas,
                &graphene::Point::from_p2d_vec(
                    pos - Vector2::new(surface_trans_x, surface_trans_y),
                ),
            )
            .map(|p| {
                canvas
                    .engine_ref()
                    .camera
                    .transform()
                    .inverse()
                    .transform_point2(p.to_p2d_vec())
            })
            .unwrap()
    };

    if event.event_type() == gdk::EventType::MotionNotify
        && backlog_policy != BacklogPolicy::Disable
    {
        let mut prev_delta = Duration::ZERO;

        let mut entries = vec![];
        for entry in event.history().into_iter().rev() {
            let available_axes = entry.flags();
            if !(available_axes.contains(gdk::AxisFlags::X)
                && available_axes.contains(gdk::AxisFlags::Y))
            {
                continue;
            }

            let entry_delta = Duration::from_millis(event_time.saturating_sub(entry.time()) as u64);
            let Some(entry_time) = now.checked_sub(entry_delta) else {
                continue;
            };

            if let BacklogPolicy::Limit(delta_limit) = backlog_policy {
                // We go back in time, so `entry_delta` will increase
                //
                // If the backlog input rate is higher than the limit, filter it out
                if entry_delta.saturating_sub(prev_delta) < delta_limit {
                    continue;
                }
            }
            prev_delta = entry_delta;

            let axes = entry.axes();
            let pos = transform_pos(Vector2::new(
                axes[crate::utils::axis_use_idx(gdk::AxisUse::X)],
                axes[crate::utils::axis_use_idx(gdk::AxisUse::Y)],
            ));
            let pressure = if is_stylus {
                axes[crate::utils::axis_use_idx(gdk::AxisUse::Pressure)]
            } else {
                Element::PRESSURE_DEFAULT
            };

            entries.push((Element::new(pos, pressure), entry_time));
        }

        elements.extend(entries.into_iter().rev());
    }

    let pos = event
        .position()
        .map(|(x, y)| transform_pos(Vector2::new(x, y)))?;

    let pressure = if is_stylus {
        event.axis(gdk::AxisUse::Pressure).unwrap()
    } else {
        Element::PRESSURE_DEFAULT
    };

    elements.push((Element::new(pos, pressure), now));

    Some(elements)
}

pub(crate) fn retrieve_button_shortcut_key(
    gdk_button: u32,
    is_stylus: bool,
) -> Option<ShortcutKey> {
    match (is_stylus, gdk_button) {
        (_, gdk::BUTTON_PRIMARY) => None,
        (false, gdk::BUTTON_SECONDARY) => Some(ShortcutKey::MouseSecondaryButton),
        (true, gdk::BUTTON_SECONDARY) => Some(ShortcutKey::StylusPrimaryButton),
        (true, gdk::BUTTON_MIDDLE) => Some(ShortcutKey::StylusSecondaryButton),
        (_, _) => None,
    }
}

pub(crate) fn retrieve_modifier_keys(modifier: gdk::ModifierType) -> HashSet<ModifierKey> {
    let mut keys = HashSet::new();

    if modifier.contains(gdk::ModifierType::SHIFT_MASK) {
        keys.insert(ModifierKey::KeyboardShift);
    }
    if modifier.contains(gdk::ModifierType::CONTROL_MASK) {
        keys.insert(ModifierKey::KeyboardCtrl);
    }
    if modifier.contains(gdk::ModifierType::ALT_MASK) {
        keys.insert(ModifierKey::KeyboardAlt);
    }

    keys
}

fn retrieve_pen_mode(event: &gdk::Event) -> Option<PenMode> {
    let device_tool = event.device_tool()?;
    match device_tool.tool_type() {
        gdk::DeviceToolType::Pen => Some(PenMode::Pen),
        gdk::DeviceToolType::Eraser => Some(PenMode::Eraser),
        _ => None,
    }
}

pub(crate) fn retrieve_keyboard_shortcut_key(
    gdk_key: gdk::Key,
    modifier: gdk::ModifierType,
) -> Option<ShortcutKey> {
    match gdk_key {
        gdk::Key::space if modifier.contains(gdk::ModifierType::CONTROL_MASK) => {
            Some(ShortcutKey::KeyboardCtrlSpace)
        }
        _ => None,
    }
}

pub(crate) fn retrieve_keyboard_key(gdk_key: gdk::Key) -> KeyboardKey {
    if let Some(keychar) = gdk_key.to_unicode() {
        KeyboardKey::Unicode(keychar).filter_convert_unicode_control_chars()
    } else {
        // Mapping based on https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
        match gdk_key {
            gdk::Key::BackSpace => KeyboardKey::BackSpace,
            gdk::Key::Tab => KeyboardKey::HorizontalTab,
            gdk::Key::KP_Tab => KeyboardKey::HorizontalTab,
            gdk::Key::ISO_Left_Tab => KeyboardKey::HorizontalTab,
            gdk::Key::Linefeed => KeyboardKey::Linefeed,
            gdk::Key::Return => KeyboardKey::CarriageReturn,
            gdk::Key::KP_Enter => KeyboardKey::CarriageReturn,
            gdk::Key::ISO_Enter => KeyboardKey::CarriageReturn,
            gdk::Key::_3270_Enter => KeyboardKey::CarriageReturn,
            gdk::Key::Escape => KeyboardKey::Escape,
            gdk::Key::Delete => KeyboardKey::Delete,
            gdk::Key::KP_Delete => KeyboardKey::Delete,
            gdk::Key::Down => KeyboardKey::NavDown,
            gdk::Key::KP_Down => KeyboardKey::NavDown,
            gdk::Key::Up => KeyboardKey::NavUp,
            gdk::Key::KP_Up => KeyboardKey::NavUp,
            gdk::Key::Left => KeyboardKey::NavLeft,
            gdk::Key::KP_Left => KeyboardKey::NavLeft,
            gdk::Key::Right => KeyboardKey::NavRight,
            gdk::Key::KP_Right => KeyboardKey::NavRight,
            gdk::Key::Shift_L => KeyboardKey::ShiftLeft,
            gdk::Key::Shift_R => KeyboardKey::ShiftRight,
            gdk::Key::Control_L => KeyboardKey::CtrlLeft,
            gdk::Key::Control_R => KeyboardKey::CtrlRight,
            gdk::Key::Home => KeyboardKey::Home,
            gdk::Key::KP_Home => KeyboardKey::Home,
            gdk::Key::End => KeyboardKey::End,
            gdk::Key::KP_End => KeyboardKey::End,
            _ => KeyboardKey::Unsupported,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dead_key_acute_e() {
        let a1 = process_dead_key(None, gdk::Key::dead_acute);
        assert_eq!(a1, DeadKeyAction::ConsumedPending(DeadKey::Acute));

        let a2 = process_dead_key(Some(DeadKey::Acute), gdk::Key::e);
        assert_eq!(a2, DeadKeyAction::CommitText("é".to_string()));
    }

    #[test]
    fn dead_key_grave_e() {
        let a1 = process_dead_key(None, gdk::Key::dead_grave);
        assert_eq!(a1, DeadKeyAction::ConsumedPending(DeadKey::Grave));

        let a2 = process_dead_key(Some(DeadKey::Grave), gdk::Key::e);
        assert_eq!(a2, DeadKeyAction::CommitText("è".to_string()));
    }

    #[test]
    fn dead_key_circumflex_standalone_and_latex() {
        // ^ + space -> ^
        assert_eq!(
            process_dead_key(Some(DeadKey::Circumflex), gdk::Key::space),
            DeadKeyAction::CommitText("^".to_string())
        );
        // ^ + ^ -> ^
        assert_eq!(
            process_dead_key(Some(DeadKey::Circumflex), gdk::Key::dead_circumflex),
            DeadKeyAction::CommitText("^".to_string())
        );
        // ^ + 2 -> ^2 (for LaTeX x^2)
        assert_eq!(
            process_dead_key(Some(DeadKey::Circumflex), gdk::Key::_2),
            DeadKeyAction::CommitText("^2".to_string())
        );
        // ^ + { -> ^{ (for LaTeX x^{n})
        assert_eq!(
            process_dead_key(Some(DeadKey::Circumflex), gdk::Key::braceleft),
            DeadKeyAction::CommitText("^{".to_string())
        );
    }

    #[test]
    fn dead_key_tilde_and_diaeresis() {
        assert_eq!(
            process_dead_key(Some(DeadKey::Tilde), gdk::Key::n),
            DeadKeyAction::CommitText("ñ".to_string())
        );
        assert_eq!(
            process_dead_key(Some(DeadKey::Diaeresis), gdk::Key::u),
            DeadKeyAction::CommitText("ü".to_string())
        );
        assert_eq!(
            process_dead_key(Some(DeadKey::Circumflex), gdk::Key::Shift_L),
            DeadKeyAction::PassThrough {
                clear_pending: false
            }
        );
    }
}
