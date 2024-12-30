#
# Set project name, version and C++ level
#
project('dubna', 'cpp',
    version: '0.1',
    default_options: [
        'buildtype=debugoptimized',
        'cpp_std=c++17',
        'warning_level=3',
        'default_library=static',
        'prefer_static=true',
    ]
)

#
# Get git commit hash and revision count
#
hash = run_command('git', 'log', '-1', '--format=%h', check: true).stdout().strip()
revcount = run_command('git', 'rev-list', 'HEAD', '--count', check: true).stdout().strip()

#
# Build executable file
#
executable('dubna', [
        'main.cpp',
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
    install: true,
    cpp_args: [
        '-Wshadow',
        '-DVERSION_STRING="' + meson.project_version() + '.' + revcount + '-' + hash + '"'
    ]
)

# Tests are defined in 'tests/meson.build' file.
#subdir('tests')
