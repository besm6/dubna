#
# make
# make all      -- build everything
#
# make test     -- run unit tests
#
# make install  -- install RP/M binaries to /usr/local
#
# make clean    -- remove build files
#

all:    build
	$(MAKE) -Cbuild $@

test:
	$(MAKE) -Cbuild unit_tests
	ctest --test-dir build/tests

install: build
	$(MAKE) -Cbuild $@

clean:
	rm -rf build tests/build

build:
	mkdir $@
	cmake -B$@ -DCMAKE_BUILD_TYPE=Debug
