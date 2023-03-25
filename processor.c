//
// Enable/disable tracing.
//
void ElSvsSetTrace(struct ElSvsProcessor *cpu, const char *trace_mode, const char *filename)
{
    if (cpu->log_output != stdout) {
        // Close previous log file.
        fclose(cpu->log_output);
        cpu->log_output = stdout;
    }

    // Disable all trace options.
    cpu->trace_instructions = false;
    cpu->trace_extracodes = false;
    cpu->trace_fetch = false;
    cpu->trace_memory = false;
    cpu->trace_exceptions = false;
    cpu->trace_registers = false;

    if (trace_mode && trace_mode[0]) {
        // Parse the mode string and enable all requested trace flags.
        int i;
        for (i = 0; trace_mode[i]; i++) {
            switch (trace_mode[i]) {
            case 'i': cpu->trace_instructions = true; break;
            case 'e': cpu->trace_extracodes = true; break;
            case 'f': cpu->trace_fetch = true; break;
            case 'm': cpu->trace_memory = true; break;
            case 'x': cpu->trace_exceptions = true; break;
            case 'r': cpu->trace_registers = true; break;
            default:
                fprintf(stderr, "Wrong trace option: %c\n", trace_mode[i]);
                exit(1);
            }
        }

        if (filename && filename[0]) {
            // Open new log file.
            cpu->log_output = fopen(filename, "a");
            if (!cpu->log_output) {
                perror(filename);
                exit(1);
            }
            setlinebuf(cpu->log_output);
        }
    }
}

//
// Write Unicode symbol to file.
// Convert to UTF-8 encoding:
// 00000000.0xxxxxxx -> 0xxxxxxx
// 00000xxx.xxyyyyyy -> 110xxxxx, 10yyyyyy
// xxxxyyyy.yyzzzzzz -> 1110xxxx, 10yyyyyy, 10zzzzzz
//
void utf8_putc(unsigned ch, FILE *fout)
{
    if (ch < 0x80) {
        putc(ch, fout);
        return;
    }
    if (ch < 0x800) {
        putc(ch >> 6 | 0xc0, fout);
        putc((ch & 0x3f) | 0x80, fout);
        return;
    }
    putc(ch >> 12 | 0xe0, fout);
    putc(((ch >> 6) & 0x3f) | 0x80, fout);
    putc((ch & 0x3f) | 0x80, fout);
}
