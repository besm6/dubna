#
# Download GoogleTest
#
include(FetchContent)
FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG v1.15.2
    EXCLUDE_FROM_ALL
)
FetchContent_MakeAvailable(googletest)
include(GoogleTest)
enable_testing()

#
# Common includes and libraries for all tests.
#
include_directories(BEFORE ..)
link_libraries(simulator gtest_main)
add_definitions(-DTEST_DIR="${CMAKE_CURRENT_SOURCE_DIR}")
add_definitions(-DBUILD_DIR="${CMAKE_BINARY_DIR}")

set(TEST_SOURCES
    cosy_test.cpp
    alu_test.cpp
    cpu_test.cpp
    machine_test.cpp
    cli_test.cpp
    session_test.cpp
    e64_test.cpp
    elem_func_test.cpp
    arch_test.cpp
    util.cpp
)
# Enable CERN tests when variable TEST_ALL is defined.
if(TEST_ALL)
    list(APPEND TEST_SOURCES
        cernlib_test.cpp
    )
endif()

#
# Check CPU instructions.
#
add_executable(unit_tests EXCLUDE_FROM_ALL ${TEST_SOURCES})
add_dependencies(unit_tests ${PROJECT_NAME})
gtest_discover_tests(unit_tests EXTRA_ARGS --gtest_repeat=1 PROPERTIES TIMEOUT 120)
