#
# Download GoogleTest
#
include(FetchContent)
FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG release-1.12.1
)
FetchContent_GetProperties(googletest)
if(NOT googletest_POPULATED)
    FetchContent_Populate(googletest)
    add_subdirectory(${googletest_SOURCE_DIR} ${googletest_BINARY_DIR} EXCLUDE_FROM_ALL)
endif()
include(GoogleTest)
enable_testing()

#
# Common includes and libraries for all tests.
#
include_directories(BEFORE ..)
link_libraries(simulator gtest gtest_main)

#
# Check CPU instructions.
#
add_executable(unit_tests EXCLUDE_FROM_ALL
    cli_test.cpp
    session_test.cpp
    machine_test.cpp
    cpu_test.cpp
    alu_test.cpp
    util.cpp
)
gtest_discover_tests(unit_tests EXTRA_ARGS --gtest_repeat=1 PROPERTIES TIMEOUT 120)
