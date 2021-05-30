"""
Extract the changes from last release
"""

import sys

if __name__ == '__main__':
    filename = sys.argv[1]

    with open(filename, mode='r') as f:
        
        # Look for the first second-level title
        for line in f:
            if line.startswith('##'):
                break
        
        print(line, end='')
        for line in f:
            if not line.startswith('##'):
                print(line, end='')
            else:
                # Exit gracefully
                sys.exit(0)
    # There was a problem: Exit with error
    sys.exit(-1)