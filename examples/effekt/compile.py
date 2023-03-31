#!/usr/bin/env python3
import argparse
import os
import subprocess
import sys
import shutil


def main(args):
    if args.compile_effekt:
        if args.names:
            filenames = [f"{name}.effekt" for name in args.names]
        else:
            filenames = [file for file in os.listdir('.') if file.endswith('.effekt')]

        for file in filenames:
            name, _ = os.path.splitext(file)
            command = f'effekt -c {name}.effekt --backend ml -o out'

            print(f'Running command: {command}')
            result = subprocess.run(command, shell=True)

            if result.returncode == 0:
                src = 'out/main.mlb'
                dest = f'out/{name}.mlb'
                if os.path.exists(src):
                    shutil.move(src, dest)
                else:
                    print(f'Could not find file {src}')
                    sys.exit(1)

    if args.compile_ml:
        for name in args.names:
            command = f'mlton -output out/{name}.exe out/{name}.mlb'
            print(f'Running command: {command}')
            result = subprocess.run(command, shell=True)

            if result.returncode != 0:
                print(f'Error encountered while running command: {command}')
                sys.exit(1)

    if not args.compile_effekt and not args.compile_ml:
        parser.print_help()

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--compile-effekt', action='store_true', help='Compile effekt files')
    parser.add_argument('--compile-ml', action='store_true', help='Print TODO')
    parser.add_argument('-n', '--names', nargs='*', help='List of names to run the command on (without extension)')
    args = parser.parse_args()
    main(args)
