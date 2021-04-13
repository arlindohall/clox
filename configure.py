#!/usr/bin/env python3

"""
configure.py

Create a build.ninja file used to build the project.

Reads the `.c` files from the clox directory and uses clang
to build object files for each, then uses a special rule for
build/clox to build the executable.

This file is based on:

- https://ninja-build.org/manual.html#_writing_your_own_ninja_files
- https://jvns.ca/blog/2020/10/26/ninja--a-simple-way-to-do-builds/
"""

import os

def output(filename):
    return f'build/{filename}.o'

with open('build.ninja', 'w') as build_ninja:
    build_ninja.write("""
rule compile
    command = mkdir -p build && clang -c $in -o $out
    description = use clang to build infile=$in object file=$out

rule link
    command = clang $in -o $out
    description = link object files=$in to final executable=$out
""")

    files = [
        file[:-2] for file in os.listdir('clox') if file.endswith('.c')
    ]
    for filename in files:
        build_ninja.write(f"""
build build/{filename}.o: compile clox/{filename}.c
""")

    build_ninja.write(f"""
build build/clox: link {' '.join([output(f) for f in files])}
""")