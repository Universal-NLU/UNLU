import sys
import os
import json

config_file = sys.argv[1]
language_code = sys.argv[2].replace("-X","")
file1=sys.argv[3]
file2=sys.argv[4]
file3=sys.argv[5]

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def get_file_content(file):
    return open(file,"r").read()

def compile_files_into_one(files, prefix=""):
    files = [prefix + f for f in files if f!=""]
    content = ""
    try:
        content="\n".join([ get_file_content(f) for f in files])
    except:
        eprint("Could not compile files: " + " ".join(files))
        exit(1)
    return content

def write_content_to_file(c,file):
    f=open(file,"w")
    f.write(c)
    f.close()

eprint("Processing config file:", config_file)
eprint("language code:", language_code)

if not (os.path.isfile(config_file)):
    eprint("Could not find the config file. Exiting")
    exit (1)


config=None
try:
    with open(config_file, "r") as f:
        config = json.load(f)
except:
    eprint("Could not load the config file. Exiting")
    exit(1)

if config == None or type(config)!=dict or "languages" not in config or "prefix" not in config:
    eprint("Could not load the config file. Exiting")
    exit(1)

if language_code not in config["languages"]:
    eprint("Language "+language_code+" could not be found in the config file. Exiting")
    exit(1)
try:
    file1_content = compile_files_into_one( config["languages"][language_code]["rules"], prefix=config["prefix"])
    file2_content = compile_files_into_one( config["languages"][language_code]["chop_rules"], prefix=config["prefix"])
    file3_content = compile_files_into_one( config["languages"][language_code]["templates"], prefix=config["prefix"])

    write_content_to_file(file1_content, file1)
    write_content_to_file(file2_content, file2)
    write_content_to_file(file3_content, file3)
    print("-t"+ config["languages"][language_code]["stanza_lang_code"] + " -x"+config["languages"][language_code]["stanza_processors"])

except Exception as a:
    eprint("Could not read the files and write them in the compiled files. Exiting")
    eprint(str(a))
    exit(1)

exit(0)
