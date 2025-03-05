#
# Set project name, version and C++ level
#
project('dubna', ['cpp', 'c'],
    version: '0.1',
    meson_version: '>=1.1',
    default_options: [
        'buildtype=debugoptimized',
        'cpp_std=c++17',
        'warning_level=3',
    ]
)

#
# Get git commit hash and revision count
#
hash = run_command('git', 'log', '-1', '--format=%h', check: true).stdout().strip()
revcount = run_command('git', 'rev-list', 'HEAD', '--count', check: true).stdout().strip()

#
# Build library
#
simulator = static_library('simulator',
    [
        'tapes/bemsh.739.c',
        'tapes/librar.12.c',
        'tapes/librar.37.c',
        'tapes/monsys.9.c',
        'session.cpp',
        'memory.cpp',
        'machine.cpp',
        'processor.cpp',
        'arithmetic.cpp',
        'besm6_arch.cpp',
        'assembler.cpp',
        'extracode.cpp',
        'trace.cpp',
        'drum.cpp',
        'disk.cpp',
        'e50.cpp',
        'e57.cpp',
        'e64.cpp',
        'encoding.cpp',
        'plotter.cpp',
        'puncher.cpp',
        'cosy.cpp',
    ],
    cpp_args: [
        '-Werror',
        '-Wshadow',
        '-DVERSION_STRING="' + meson.project_version() + '.' + revcount + '-' + hash + '"'
    ]
)

#
# Build executable file
#
executable('dubna', 'main.cpp',
    cpp_args: [ '-Wshadow' ],
    link_with: simulator,
    install: true,
)

#
# Run cppcheck
#
run_target('cppcheck', command: [
        'cppcheck',
        '--project=' + join_paths(meson.project_build_root(), 'compile_commands.json'),
        '--std=c++17',
        '--enable=style',
        '--check-level=exhaustive',
        '--error-exitcode=1', # Fail on any issues
        '--inline-suppr', # Enable inline control like // cppcheck-suppress 'id'
        '--quiet', # No progress report messages
        '--suppress=badBitmaskCheck', # Allow redundant zero operands
        '--suppress=*:*/gtest/*', # Ignore issues in Googletest
        '--library=' + join_paths(meson.project_source_root(), 'tests/googletest.xml'), # Parse TEST() macro properly
    ]
)

# Tests are defined in 'tests/meson.build' file
if (get_option('tests').allowed())
    subdir('tests')
endif
