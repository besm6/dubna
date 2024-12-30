#
# Find the GTest library
#
gtest = dependency('gtest', main: true, version: '>= 1.15.2')

#
# Build all tests as a single executable
#
unit_tests = executable('unit_tests',
    [
        'cosy_test.cpp',
        'alu_test.cpp',
        'cpu_test.cpp',
        'machine_test.cpp',
        'cli_test.cpp',
        'session_test.cpp',
        'e64_test.cpp',
        'elem_func_test.cpp',
        'arch_test.cpp',
        'util.cpp',
#       'cernlib_test.cpp', # TODO: enable CERN tests when variable TEST_ALL is defined
    ],
    dependencies: gtest,
    include_directories: '..',
    link_with: simulator,
    cpp_args: [
        '-Wshadow',
        '-DTEST_DIR="' + meson.current_source_dir() + '"',
        '-DBUILD_DIR="' + meson.build_root() + '"',
    ],
    build_by_default: false
)
test('tests', unit_tests)
