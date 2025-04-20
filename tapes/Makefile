C_FILES = monsys9.c librar12.c librar37.c bemsh739.c b7.c

all:    $(C_FILES)

clean:
	rm -f $(C_FILES)

monsys9.c: monsys.9.patched
	xxd -i $< > $@

librar12.c: librar.12
	xxd -i $< > $@

librar37.c: librar.37
	xxd -i $< > $@

bemsh739.c: bemsh.739
	xxd -i $< > $@

b7.c: b.7
	xxd -i $< > $@
