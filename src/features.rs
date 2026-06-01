use std::{
    io::{self, Write},
    os::fd::{AsFd, AsRawFd},
    time::{Duration, Instant},
};

use crate::{
    Terminal,
    event::{self, ClipboardContent, Events, Polled},
    rgb_supported_from_env,
    vt::{
        BufferWrite, ClipboardSelection, QUERY_PRIMARY_DEVICE_ATTRIBUTES, QueryClipboard,
        QueryTerminfoCapability,
    },
};

/// Default timeout for active terminal feature probes.
///
/// Active probes should be short because they are typically run during startup
/// or on first use. A timeout means the terminal either does not support the
/// feature or the user has disabled the response for security reasons.
pub const DEFAULT_TERMINAL_FEATURE_QUERY_TIMEOUT: Duration = Duration::from_millis(100);

bitflags::bitflags! {
    /// Terminal features detected by extui.
    ///
    /// Features that can be inferred without terminal I/O are available through
    /// [`TerminalFeatures::from_env`]. Features that require a terminal
    /// response can be queried with [`Terminal::detect_features`].
    #[derive(Debug, Default, PartialOrd, PartialEq, Eq, Clone, Copy, Hash)]
    pub struct TerminalFeatures: u8 {
        /// The environment advertises 24-bit RGB color support.
        const TRUECOLOR = 0b0000_0001;
        /// OSC 52 clipboard writes are advertised by the terminal.
        ///
        /// This follows Neovim's detection path: query DA1 first and accept a
        /// `52` parameter, then fall back to XTGETTCAP `Ms` and require an OSC
        /// 52 sequence.
        const OSC52_CLIPBOARD = 0b0000_0010;
        /// OSC 52 clipboard reads are supported and currently permitted.
        ///
        /// This requires an active OSC 52 query. A missing response is treated
        /// as unsupported because many terminals disable clipboard reads by
        /// default.
        const OSC52_CLIPBOARD_READ = 0b0000_0100;
    }
}

impl TerminalFeatures {
    /// Detects features that can be inferred from environment variables.
    ///
    /// This performs no terminal I/O and is safe to call frequently.
    pub fn from_env() -> Self {
        let mut features = TerminalFeatures::empty();
        if rgb_supported_from_env() {
            features |= TerminalFeatures::TRUECOLOR;
        }
        features
    }
}

impl Terminal {
    /// Detects the requested terminal features.
    ///
    /// Environment-only features are resolved immediately. Features requiring a
    /// terminal response are queried synchronously using `input` and `events`,
    /// which keeps any unrelated input buffered for the caller's normal event
    /// loop.
    ///
    /// # Errors
    ///
    /// Returns an error if writing the probe, polling `input`, or reading from
    /// `input` fails.
    pub fn detect_features<Input>(
        &mut self,
        input: &Input,
        events: &mut Events,
        requested: TerminalFeatures,
        timeout: Duration,
    ) -> io::Result<TerminalFeatures>
    where
        Input: AsFd + AsRawFd,
    {
        let mut supported = TerminalFeatures::empty();
        let env = TerminalFeatures::from_env();
        supported |= env & requested;

        if requested.contains(TerminalFeatures::OSC52_CLIPBOARD)
            && self.osc52_clipboard_supported(input, events, timeout)?
        {
            supported |= TerminalFeatures::OSC52_CLIPBOARD;
        }

        if requested.contains(TerminalFeatures::OSC52_CLIPBOARD_READ)
            && self.osc52_clipboard_read_supported(input, events, timeout)?
        {
            supported |= TerminalFeatures::OSC52_CLIPBOARD_READ;
        }

        Ok(supported)
    }

    /// Queries whether OSC 52 clipboard writes are advertised by the terminal.
    ///
    /// This mirrors Neovim's TUI detection path. It sends DA1 (`ESC[c`) and
    /// treats parameter `52` in the response as OSC 52 support. If DA1 responds
    /// without `52`, it falls back to XTGETTCAP `Ms` and accepts the capability
    /// only when the reported sequence starts with OSC 52.
    ///
    /// This detects support for writing clipboard data. It does not prove a
    /// particular write succeeded because OSC 52 clipboard writes do not have a
    /// standard acknowledgement.
    ///
    /// # Errors
    ///
    /// Returns an error if writing a query, polling `input`, or reading from
    /// `input` fails.
    pub fn osc52_clipboard_supported<Input>(
        &mut self,
        input: &Input,
        events: &mut Events,
        timeout: Duration,
    ) -> io::Result<bool>
    where
        Input: AsFd + AsRawFd,
    {
        self.write_all(QUERY_PRIMARY_DEVICE_ATTRIBUTES)?;

        let Some(attributes) = wait_for_response(input, events, timeout, |events| {
            events.take_primary_device_attributes()
        })?
        else {
            return Ok(false);
        };

        if attributes.params.contains(&52) {
            return Ok(true);
        }

        if std::env::var("TERM_PROGRAM").is_ok_and(|value| value == "Apple_Terminal") {
            return Ok(false);
        }

        let mut query = Vec::with_capacity(8);
        QueryTerminfoCapability("Ms").write_to_buffer(&mut query);
        self.write_all(&query)?;

        let Some(response) = wait_for_response(input, events, timeout, |events| {
            events.take_terminfo_response("Ms")
        })?
        else {
            return Ok(false);
        };

        Ok(response
            .value
            .as_deref()
            .is_some_and(|value| response.found && value.starts_with("\x1b]52")))
    }

    /// Queries whether OSC 52 clipboard reads are supported and permitted.
    ///
    /// This sends `OSC 52 ; c ; ?` and waits up to `timeout` for the terminal
    /// to answer with clipboard contents. Terminals commonly block this query
    /// by default, so `Ok(false)` means either unsupported or disabled by user
    /// policy.
    ///
    /// Any bytes read while waiting remain in `events` unless they are the OSC
    /// 52 clipboard response consumed by this probe.
    ///
    /// # Errors
    ///
    /// Returns an error if writing the probe, polling `input`, or reading from
    /// `input` fails.
    pub fn osc52_clipboard_read_supported<Input>(
        &mut self,
        input: &Input,
        events: &mut Events,
        timeout: Duration,
    ) -> io::Result<bool>
    where
        Input: AsFd + AsRawFd,
    {
        self.read_clipboard(input, events, ClipboardSelection::Clipboard, timeout)
            .map(|response| response.is_some())
    }

    /// Reads clipboard text using OSC 52, if the terminal permits it.
    ///
    /// This sends an OSC 52 query for `selection` and waits up to `timeout`
    /// for the terminal to answer. `Ok(None)` means no response arrived before
    /// the timeout, which covers unsupported terminals and security policies
    /// that block clipboard reads.
    ///
    /// Unrelated input read while waiting remains buffered in `events`.
    ///
    /// # Errors
    ///
    /// Returns an error if writing the query, polling `input`, or reading from
    /// `input` fails.
    pub fn read_clipboard<Input>(
        &mut self,
        input: &Input,
        events: &mut Events,
        selection: ClipboardSelection,
        timeout: Duration,
    ) -> io::Result<Option<ClipboardContent>>
    where
        Input: AsFd + AsRawFd,
    {
        self.write_clipboard_query(selection)?;

        wait_for_response(input, events, timeout, |events| {
            events.take_clipboard_response(selection)
        })
    }

    fn write_clipboard_query(&mut self, selection: ClipboardSelection) -> io::Result<()> {
        let mut query = Vec::with_capacity(9);
        QueryClipboard(selection).write_to_buffer(&mut query);
        self.write_all(&query)
    }
}

fn wait_for_response<Input, T>(
    input: &Input,
    events: &mut Events,
    timeout: Duration,
    mut take: impl FnMut(&mut Events) -> Option<T>,
) -> io::Result<Option<T>>
where
    Input: AsFd + AsRawFd,
{
    let start = Instant::now();
    loop {
        if let Some(response) = take(events) {
            return Ok(Some(response));
        }

        let Some(remaining) = timeout.checked_sub(start.elapsed()) else {
            return Ok(None);
        };

        match event::poll(input, Some(remaining))? {
            Polled::ReadReady => events.read_from(input)?,
            Polled::TimedOut => return Ok(None),
            Polled::Woken => {}
        }
    }
}
