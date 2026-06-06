//! Paged access to source text.
//!
//! Every entry point takes source through [`Source`], a `dyn`-safe trait
//! that yields one page at a time. This lets callers back the source with
//! ropes, flash-paged buffers, memory-mapped files, or a plain `&str`
//! without materializing a single contiguous copy.

/// Paged byte access to source text.
///
/// Implementors must return at least one byte whenever the requested offset
/// lies within `0..self.len()`, and an empty slice once `offset >= self.len()`.
///
/// Pages should be large enough that per-call dispatch is amortized.
/// Returning one byte per call is legal but slow. The blanket impls for
/// `&str` and `&[u8]` return the whole buffer in a single page.
///
/// # Examples
///
/// ```
/// use tinyhl::Source;
///
/// let text = "hello";
/// let src: &dyn Source = &text;
/// assert_eq!(src.len(), 5);
/// let (base, page) = src.page(0);
/// assert_eq!(base, 0);
/// assert_eq!(page, b"hello");
/// ```
pub trait Source {
    /// Returns the total number of bytes in the source.
    fn len(&self) -> u32;

    /// Returns `true` if the source contains no bytes.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the page containing `offset` as `(page_base, page)`, where
    /// `page_base <= offset < page_base + page.len()`.
    ///
    /// At or past end-of-source the returned slice is empty and `page_base`
    /// equals [`Source::len`].
    fn page(&self, offset: u32) -> (u32, &[u8]);

    /// Returns the page beginning at `offset` when a cursor advances exactly
    /// past the previous page. Implementors with cheap sequential paging can
    /// override this to avoid a fresh random lookup.
    #[inline]
    fn next_page(&self, offset: u32) -> (u32, &[u8]) {
        self.page(offset)
    }
}

impl Source for &str {
    #[inline]
    fn len(&self) -> u32 {
        str::len(self) as u32
    }

    #[inline]
    fn page(&self, offset: u32) -> (u32, &[u8]) {
        let bytes = self.as_bytes();
        let end = bytes.len() as u32;
        if offset >= end {
            (end, &[])
        } else {
            (0, bytes)
        }
    }
}

impl Source for &[u8] {
    #[inline]
    fn len(&self) -> u32 {
        <[u8]>::len(self) as u32
    }

    #[inline]
    fn page(&self, offset: u32) -> (u32, &[u8]) {
        let end = <[u8]>::len(self) as u32;
        if offset >= end { (end, &[]) } else { (0, self) }
    }
}

/// Cursor-friendly wrapper over a [`Source`] that caches the current page.
pub struct SourceView<'s> {
    src: &'s dyn Source,
    page_base: u32,
    page_end: u32,
    page: &'s [u8],
    src_len: u32,
}

impl<'s> SourceView<'s> {
    pub(crate) fn new(src: &'s dyn Source, at: u32) -> Self {
        let src_len = src.len();
        let (page_base, page) = src.page(at);
        let page_end = page_base + page.len() as u32;
        Self {
            src,
            page_base,
            page_end,
            page,
            src_len,
        }
    }

    #[inline]
    #[allow(dead_code)]
    pub(crate) fn len(&self) -> u32 {
        self.src_len
    }

    #[inline]
    #[allow(dead_code)]
    pub(crate) fn is_empty(&self) -> bool {
        self.src_len == 0
    }

    #[inline]
    #[allow(dead_code)]
    pub(crate) fn page_base(&self) -> u32 {
        self.page_base
    }

    #[inline]
    #[allow(dead_code)]
    pub(crate) fn page(&self) -> &[u8] {
        self.page
    }

    #[inline]
    pub(crate) fn in_window(&self, offset: u32) -> bool {
        offset >= self.page_base && offset < self.page_end
    }

    #[inline]
    pub(crate) fn refresh(&mut self, offset: u32) {
        if !self.in_window(offset) {
            let (base, page) = if offset == self.page_end {
                self.src.next_page(offset)
            } else {
                self.src.page(offset)
            };
            self.page_base = base;
            self.page_end = base + page.len() as u32;
            self.page = page;
        }
    }

    #[inline]
    pub(crate) fn set_visible_len(&mut self, new_len: u32) -> u32 {
        let old = self.src_len;
        self.src_len = new_len;
        old
    }

    #[inline]
    pub(crate) fn byte_at(&mut self, offset: u32) -> Option<u8> {
        if offset >= self.src_len {
            return None;
        }
        if !self.in_window(offset) {
            self.refresh(offset);
        }
        let rel = (offset - self.page_base) as usize;
        Some(self.page[rel])
    }

    #[inline]
    pub(crate) fn window_at(&mut self, offset: u32) -> (u32, &[u8]) {
        if offset >= self.src_len {
            return (self.src_len, &[]);
        }
        if !self.in_window(offset) {
            self.refresh(offset);
        }
        let rel = (offset - self.page_base) as usize;
        let visible = (self.src_len - offset) as usize;
        let avail = (self.page_end - offset) as usize;
        let take = visible.min(avail);
        (offset, &self.page[rel..rel + take])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn str_source_one_page() {
        let s = "hello";
        let src: &dyn Source = &s;
        assert_eq!(src.len(), 5);
        let (base, page) = src.page(0);
        assert_eq!(base, 0);
        assert_eq!(page, b"hello");
        let (base, page) = src.page(5);
        assert_eq!(base, 5);
        assert!(page.is_empty());
    }

    #[test]
    fn view_reads_bytes() {
        let s = "abcdef";
        let src: &dyn Source = &s;
        let mut v = SourceView::new(src, 0);
        assert_eq!(v.byte_at(0), Some(b'a'));
        assert_eq!(v.byte_at(5), Some(b'f'));
        assert_eq!(v.byte_at(6), None);
    }

    #[test]
    fn view_window_slices_from_offset() {
        let s = "hello world";
        let src: &dyn Source = &s;
        let mut v = SourceView::new(src, 0);
        let (base, slice) = v.window_at(6);
        assert_eq!(base, 6);
        assert_eq!(slice, b"world");
    }

    struct Paged<'a> {
        pages: &'a [&'a [u8]],
        total: u32,
    }

    impl<'a> Paged<'a> {
        fn new(pages: &'a [&'a [u8]]) -> Self {
            let mut total = 0u32;
            for p in pages {
                total += p.len() as u32;
            }
            Self { pages, total }
        }
    }

    impl Source for Paged<'_> {
        fn len(&self) -> u32 {
            self.total
        }

        fn page(&self, offset: u32) -> (u32, &[u8]) {
            let mut base = 0u32;
            for p in self.pages {
                let end = base + p.len() as u32;
                if offset < end {
                    return (base, p);
                }
                base = end;
            }
            (self.total, &[])
        }
    }

    #[test]
    fn view_refreshes_across_pages() {
        let p = Paged::new(&[b"abc", b"def", b"ghi"]);
        let src: &dyn Source = &p;
        let mut v = SourceView::new(src, 0);
        assert_eq!(v.byte_at(0), Some(b'a'));
        assert_eq!(v.byte_at(2), Some(b'c'));
        assert_eq!(v.byte_at(3), Some(b'd'));
        assert_eq!(v.byte_at(5), Some(b'f'));
        assert_eq!(v.byte_at(8), Some(b'i'));
        assert_eq!(v.byte_at(9), None);
    }
}
