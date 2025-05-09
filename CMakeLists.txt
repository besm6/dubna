# Set minimum required version of CMake
cmake_minimum_required(VERSION 3.12)

# Set name of project (as PROJECT_NAME) and C/C++ standards
project(dubna VERSION 0.1 DESCRIPTION "Dubna Simulator")
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)
add_compile_options(-Wall -Werror -Wshadow)

#
# Enable cppcheck by default
#
find_program(Cppcheck NAMES cppcheck)
if(NOT (Cppcheck MATCHES "NOTFOUND"))
    message(STATUS "Found Cppcheck: ${Cppcheck}")
    set(CMAKE_CXX_CPPCHECK "${Cppcheck}"
        "--std=c++17"
        "--enable=style"
        "--check-level=exhaustive"
        "--error-exitcode=1" # Fail on any issues
        "--inline-suppr" # Enable inline control like // cppcheck-suppress "id"
        "--quiet" # No progress report messages
        "--suppress=badBitmaskCheck" # Allow redundant zero operands
        "--suppress=*:*/_deps/*" # Ignore issues in Googletest
        "--library=${CMAKE_SOURCE_DIR}/tests/googletest.xml" # Parse TEST() macro properly
    )
endif()

# Build library
add_library(simulator STATIC
    tapes/bemsh739.c
    tapes/librar12.c
    tapes/librar37.c
    tapes/monsys9.c
    tapes/b7.c
    session.cpp
    memory.cpp
    machine.cpp
    processor.cpp
    arithmetic.cpp
    besm6_arch.cpp
    assembler.cpp
    extracode.cpp
    trace.cpp
    drum.cpp
    disk.cpp
    e50.cpp
    e57.cpp
    e64.cpp
    encoding.cpp
    plotter.cpp
    puncher.cpp
    cosy.cpp
)

# Build executable file
add_executable(${PROJECT_NAME} main.cpp)
target_link_libraries(${PROJECT_NAME} simulator)

# Get git commit hash and revision count
execute_process(
    COMMAND git log -1 --format=%h
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_STRIP_TRAILING_WHITESPACE
    OUTPUT_VARIABLE GIT_HASH
)
execute_process(
    COMMAND git rev-list HEAD --count
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_STRIP_TRAILING_WHITESPACE
    OUTPUT_VARIABLE GIT_REVCOUNT
)
set_source_files_properties(session.cpp PROPERTIES COMPILE_FLAGS
    -DVERSION_STRING=\\"${CMAKE_PROJECT_VERSION}.${GIT_REVCOUNT}-${GIT_HASH}\\"
)

install(TARGETS
    ${PROJECT_NAME}
    DESTINATION bin
)

add_subdirectory(tests)
