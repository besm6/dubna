//
// Write GOST-10859 string to stdout.
// Convert to local encoding UTF-8.
//
void gost_write(unsigned char *line, unsigned n);

//
// Convert character in GOST-10859 encoding to Unicode.
//
unsigned gost_to_unicode(unsigned char ch);

//
// Write Unicode symbol to stdout in UTF-8 encoding.
//
void utf8_putc(unsigned ch);
