import os
from argparse import ArgumentParser


if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument('input', type=str)
    parser.add_argument('--output', type=str, default='tmp/hands-split')
    args = parser.parse_args()

    file_txt = open(args.input, 'r').read()
    hands = file_txt.split('\n\n\n\n')

    for i, x in enumerate(hands):
        filename = os.path.join(args.output, '%i.txt' % i)
        with open(filename, 'w') as f:
            f.write(x.encode('ascii', errors='ignore').decode())
