#
# make
# make all      -- build everything
#
# make test     -- run unit tests
#
# make install  -- install binaries to /usr/local
#
# make clean    -- remove build files
#
# To reconfigure for Debug build:
#   make clean; make debug; make
#

all:    build
	$(MAKE) -Cbuild $@

test:   build
	$(MAKE) -Cbuild unit_tests
	ctest --test-dir build/tests

test-all: TEST_ALL="-DTEST_ALL:BOOL=ON"
test-all: build
	@if grep TEST_ALL:BOOL=ON build/CMakeCache.txt; \
        then \
            $(MAKE) test; \
        else \
            rm -rf build; \
            $(MAKE) $@; \
        fi

install: build
	$(MAKE) -Cbuild $@

clean:
	rm -rf build

build:
	mkdir $@
	cmake -B$@ -DCMAKE_BUILD_TYPE=RelWithDebInfo $(TEST_ALL)

debug:
	mkdir build
	cmake -Bbuild -DCMAKE_BUILD_TYPE=Debug
