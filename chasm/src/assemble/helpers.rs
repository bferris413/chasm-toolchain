pub fn normalize_to_ascii_lower<'a>(s: &'a str, buf: &'a mut [u8; 8]) -> &'a str {
    let bytes = s.as_bytes();
    let len = bytes.len().min(8);
    let buf = &mut buf[..len];

    for (i, &b) in bytes.iter().take(len).enumerate() {
        buf[i] = b.to_ascii_lowercase();
    }

    str::from_utf8(buf).expect("valid ascii")
}