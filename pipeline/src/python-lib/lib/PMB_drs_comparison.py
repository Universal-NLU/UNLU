# This file includes code that takes two PMB DRSes as input and returns a 
# a comparison score between them. It uses https://github.com/RikVN/DRS_parsing 
# tool as the base library. It wraps it by including the main counter.py 
# code that does the actual computation.

import os
import sys
import io
from contextlib import redirect_stdout, redirect_stderr
import subprocess

PATH_TO_DRS_PARSING = "DRS_parsing/evaluation"
if DRT_PYTHON!="":
    PATH_TO_DRS_PARSING = os.path.abspath(DRT_PYTHON + "/" + PATH_TO_DRS_PARSING)

PATH_TO_UNLU_YAML = os.path.abspath(DRT_PYTHON + "/../../config/unlu-signature.yaml")

# Include the code in the base library file that we have
sys.path.append(PATH_TO_DRS_PARSING)

PMB_COMPARISON_FILENAMES=["first.tmp","second.tmp", "result.tmp"]

# If write access to a RAM based shared memory space exists, use there
if os.access('/dev/shm/', os.W_OK | os.R_OK):
    PMB_COMPARISON_FILENAMES=["/dev/shm/first.tmp", "/dev/shm/second.tmp", "/dev/shm/result.tmp"]


def write_to_file(f_name, content):
    try:
        with open(f_name, 'w') as f:
            f.write(content)
            f.close()
        return True
    except Exception as E:
        return False

def pmb_drs_compare(drs1, drs2, include_wordnet=False, return_out=False):
    global PMB_COMPARISON_FILENAMES
    global PATH_TO_DRS_PARSING
    global PATH_TO_UNLU_YAML
    global firstt
    out = None
    error =None
    the_output=""
    if write_to_file(PMB_COMPARISON_FILENAMES[0], drs1.strip()+"\n") and write_to_file(PMB_COMPARISON_FILENAMES[1], drs2.strip()+"\n"):
        argv = [PATH_TO_DRS_PARSING + "/counter.py", "-f1", PMB_COMPARISON_FILENAMES[0], "-f2", PMB_COMPARISON_FILENAMES[1], "-r", "20", "-s", "conc", "-runs", "1", "-dse", "-g", PATH_TO_UNLU_YAML, "-ms_file", PMB_COMPARISON_FILENAMES[2] ]

        if include_wordnet:
            del argv[11]
        proc = subprocess.Popen(sys.executable + " " + " ".join(argv) ,stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True)
        out=  proc.communicate()[0].decode()
        the_output=out
        eprint(out)
    precision =""
    recall = ""
    f1=""
    error = None
    if not out == None:
        if(out.find("Error:") != -1):
            out = out.split("\n")
            output = ""
            for item in out:
                if item.find("Error:") != -1:
                    output += item

            return output
        try:
            with open(PMB_COMPARISON_FILENAMES[2], "r") as file:
                data = file.read().replace('\n', '')
                data = float(data)
                f1 = data
            out=out.split("\n")
            for item in out:
                item = item.split(":")
                if item[0].strip() == "Precision":
                    precision = float(item[1][1:])
                elif item[0].strip() == "Recall":
                    recall = float(item[1][1:])

        except Exception as e:
            error = str(e) 
            return error

        return (f1,recall,precision, the_output) if return_out else (f1,recall,precision)

    if error != None:
        return error

    return(None, None, None, "") if return_out else (None, None, None)

