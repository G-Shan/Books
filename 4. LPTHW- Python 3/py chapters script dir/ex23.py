
# To run this code, type "python ex23.py utf-8 strict"

import sys

script, input_encoding, error = sys.argv

def main(language_file, encoding, errors):
    line = language_file.readline()

    if line:
        print_line(line, encoding, errors)
        return main(language_file, encoding, errors)



def print_line(line, encoding, errors):
    next_lang = line.strip()
    raw_bytes = next_lang.encode(encoding, errors= errors)
    cooked_string = raw_bytes.decode(encoding, errors=errors)

    print(raw_bytes, "<===>", cooked_string)

languages = open("languages.txt", encoding="utf-8")

main(languages, input_encoding, error)

"""
Interpretation of the output
The ex23.py script is taking bytes written inside the b'' (byte string) and converting them to the UTF-
8 (or other) encoding you specified. On the left is the numbers for each byte of the utf-8 (shown in
hexadecimal), and the right has the character output as actual utf-8. The way to think of this is the left
side of <===> is the Python numerical bytes, or the ”raw” bytes Python uses to store the string. You
specify this with b'' to tell Python this is ”bytes”. These raw bytes are then displayed ”cooked” on the
right so you can see the real characters in your terminal.
"""
