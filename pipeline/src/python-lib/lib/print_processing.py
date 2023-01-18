import io
from contextlib import redirect_stdout
import re

if 'PRESUPPOSITION_MARKER_STRING_REFERENT' not in locals():
    PRESUPPOSITION_MARKER_STRING_REFERENT = "K"
if 'PRESUPPOSITION_MARKER_STRING_CONDITION' not in locals():
    PRESUPPOSITION_MARKER_STRING_CONDITION = "L"

def find_substring_locations(str_needle, str_haystack):
    # Lazy implementation of finding substring locations in a string
    locations = set()
    for i in range(0,len(str_haystack)-len(str_needle)):
        same = True
        subText = str_haystack[i:i+len(str_needle)]
        for j in range(len(str_needle)):
            if subText[j] != str_needle[j]:
                same = False
                break
        if same:
            locations.add(i)
    return locations


def write_stderr(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def get_stdout(expr):
    f = io.StringIO()
    with redirect_stdout(f):
        exec(expr, globals())
    out = f.getvalue()
    return out

def make_more_pretty(strr):
    global PRESUPPOSITION_MARKER_STRING_REFERENT
    strr = strr.splitlines();
    for i in range(0, len(strr)):
        strr[i] = strr[i].replace("|_","└─").replace(" _", "┌─").replace("_ ","─┐").replace("_|","─┘").replace("|-","├─").replace("-|","─┤").replace("|","│").replace("_","─").replace("-","─").replace("─>","->").replace("/\\"," λ").replace("\ ","  ")
    for i in range(0, len(strr)):
        if i<len(strr)-2:
            indices = find_substring_locations('┌──┐ ', strr[i]) & find_substring_locations('   │ ', strr[i+1])
            for indice in indices:
                strr[i] = strr[i][0:indice] + '    ¬' + strr[i][indice+5:]
                strr[i+1] = strr[i+1][0:indice] + '     ' + strr[i+1][indice+5:]
            
    strr = "\n".join(strr).replace(PRESUPPOSITION_MARKER_STRING_REFERENT, "*" + ' '*(len(PRESUPPOSITION_MARKER_STRING_REFERENT)-1)).replace("`~","* ")
    return strr
