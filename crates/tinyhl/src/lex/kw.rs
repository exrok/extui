//! Keyword lookup over a static, sorted byte-slice table.
//!
//! The [`kw_table!`] macro declares a `static` table and enforces sorted
//! order at compile time. [`kw_lookup`] runs a binary search over the table.

/// One entry in a keyword table, pairing the keyword bytes with a local
/// kind from [`crate::kind`].
pub type Entry = (&'static [u8], u16);

/// Returns the local kind associated with `needle`, or [`None`] if it is
/// not in `table`.
///
/// `table` must be sorted by the byte slices. The macro [`kw_table!`]
/// produces a sorted table.
pub fn kw_lookup(table: &[Entry], needle: &[u8]) -> Option<u16> {
    table
        .binary_search_by(|(k, _)| (*k).cmp(needle))
        .ok()
        .map(|i| table[i].1)
}

/// Declares a `static` keyword table and asserts at compile time that its
/// entries are sorted.
///
/// # Examples
///
/// ```
/// use tinyhl::lex::kw;
///
/// kw::kw_table! {
///     static MY_KWS = [
///         (b"false", 1),
///         (b"null",  2),
///         (b"true",  3),
///     ];
/// }
/// assert_eq!(kw::kw_lookup(MY_KWS, b"null"), Some(2));
/// assert_eq!(kw::kw_lookup(MY_KWS, b"nope"), None);
/// ```
#[macro_export]
macro_rules! kw_table {
    (static $name:ident = [ $( ($bytes:expr, $kind:expr) ),+ $(,)? ];) => {
        pub static $name: &[$crate::lex::kw::Entry] = &[
            $( ($bytes, $kind) ),+
        ];
        const _: () = {
            let entries: &[$crate::lex::kw::Entry] = $name;
            let mut i = 1;
            while i < entries.len() {
                let a = entries[i - 1].0;
                let b = entries[i].0;
                let mut j = 0;
                let min = if a.len() < b.len() { a.len() } else { b.len() };
                let ok = loop {
                    if j == min {
                        break a.len() < b.len();
                    }
                    if a[j] < b[j] {
                        break true;
                    }
                    if a[j] > b[j] {
                        break false;
                    }
                    j += 1;
                };
                assert!(ok, "keyword table is not strictly sorted");
                i += 1;
            }
        };
    };
}

pub use kw_table;

#[cfg(test)]
mod tests {
    use super::*;

    kw_table! {
        static TEST_KWS = [
            (b"false", 10),
            (b"null", 20),
            (b"true", 30),
        ];
    }

    #[test]
    fn finds_keywords() {
        assert_eq!(kw_lookup(TEST_KWS, b"false"), Some(10));
        assert_eq!(kw_lookup(TEST_KWS, b"null"), Some(20));
        assert_eq!(kw_lookup(TEST_KWS, b"true"), Some(30));
    }

    #[test]
    fn returns_none_for_misses() {
        assert_eq!(kw_lookup(TEST_KWS, b""), None);
        assert_eq!(kw_lookup(TEST_KWS, b"maybe"), None);
        assert_eq!(kw_lookup(TEST_KWS, b"True"), None);
        assert_eq!(kw_lookup(TEST_KWS, b"nulll"), None);
    }
}
