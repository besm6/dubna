#
# Set project name, version and C++ level
#
project('dubna', 'cpp',
    version: '0.1',
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

add_global_arguments(
    '-DVERSION_STRING="' + meson.project_version() + '.' + revcount + '-' + hash + '"',
    language: 'cpp',
)

#
# Build library
#
simulator = static_library('simulator',
    [
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
# Install disk images
#
fs = import('fs')
install_data([
        'tapes/9',
        'tapes/12',
        'tapes/37',
        'tapes/739',
    ],
    install_dir: fs.expanduser('~/.besm6'),
    install_mode: 'r--r--r--',
)

# Tests are defined in 'tests/meson.build' file.
#subdir('tests')
