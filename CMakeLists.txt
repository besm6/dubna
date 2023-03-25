# Set minimum required version of CMake
cmake_minimum_required(VERSION 3.12)

# Set name of project (as PROJECT_NAME) and C/C++ standards
project(dubna VERSION 0.1 DESCRIPTION "Dubna Simulator")
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)
add_compile_options(-Wall -Werror -Wmissing-prototypes -Wshadow)

# Build library
add_library(simulator STATIC
    session.cpp
    memory.cpp
    machine.cpp
    processor.cpp
    arithmetic.cpp
    besm6_arch.cpp
    extracode.cpp
    trace.cpp
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
    -DVERSION_STRING=\\"${DUBNA_VERSION}.${GIT_REVCOUNT}-${GIT_HASH}\\"
)

install(TARGETS
    ${PROJECT_NAME}
    DESTINATION bin
)

add_subdirectory(tests)
