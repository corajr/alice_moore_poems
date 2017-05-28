#!/usr/bin/env python

import re
import sys

poem_title_re = re.compile(r'\\PoemTitle\[([^]]+)\]{([^:]+):}')

def extract_character(poem_title_line):
    result = poem_title_re.match(poem_title_line)
    if result is None:
        return None, None
    result = result.groups()
    title_line = '\\PoemTitle[' + result[0] + ']{}'
    return title_line, result[1]

def main():
    for fname in sys.argv[1:]:
        with open(fname, 'rU') as f:
            lines = f.readlines()
        new_title_line, character = extract_character(lines[0])
        if new_title_line is not None:
            lines[0] = new_title_line
            lines[4] = '\\Character{' + character + '} ' + lines[4]
            with open(fname, 'w') as f:
                f.writelines(lines)

if __name__ == '__main__':
    main()
