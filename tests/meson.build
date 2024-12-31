#
# Find the GTest library
#
gtest = subproject('gtest')

#
# Source files of the tests
#
test_sources = [
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
]

# Enable CERN tests when variable TEST_ALL is defined
if (get_option('test-all').allowed())
    test_sources += ['cernlib_test.cpp']
endif

#
# Build all tests as a single executable
#
unit_tests = executable('unit_tests', test_sources,
    dependencies: [
        gtest.get_variable('gtest_dep'),
        gtest.get_variable('gtest_main_dep'),
    ],
    include_directories: '..',
    link_with: simulator,
    cpp_args: [
        '-Werror',
        '-Wshadow',
        '-DTEST_DIR="' + meson.current_source_dir() + '"',
        '-DBUILD_DIR="' + meson.project_build_root() + '"',
    ],
    build_by_default: false
)
test('tests', unit_tests, protocol: 'gtest', timeout: 0)
