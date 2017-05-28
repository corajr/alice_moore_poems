#!/usr/bin/env python

import os
import re
import sys

poem_title_re = re.compile(r'\\PoemTitle\[([^]]+)\]{}')

def extract_incipit(poem_title_line):
    result = poem_title_re.match(poem_title_line)
    if result is None:
        return None
    return result.group(1)

def main():
    for fname in sys.argv[1:]:
        ref = os.path.basename(fname)[:-4]
        with open(fname, 'rU') as f:
            poem_title_line = f.readline()
        incipit = extract_incipit(poem_title_line)
        if incipit is not None:
            subsection = '\\subsection{' + incipit + ' (p. \pageref{ch:' + ref + '})}'
            print(subsection)
            print('')

if __name__ == '__main__':
    main()
