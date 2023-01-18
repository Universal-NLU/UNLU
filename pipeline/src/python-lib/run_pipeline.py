# This is the main pipeline that calls ud2drs and gsw
# Two parameters must be given to run:
# input_file
# heads_dat_file
# 
# Also the UD2DRS_EXE, PYTHON_EXE, DRT_PYTHON, GSW_JAR, and JAVA_EXE
# global variables must be set accordingly

import os
import argparse
from pathlib import Path
import sys
import subprocess
import re
import json
import hashlib
import traceback
import re
import copy
import multiprocessing
import time
from random import seed
from random import random

# Write anything given as paramter to STDERR
def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)
UNLU_HOME = os.path.abspath(os.path.abspath(os.path.dirname( __file__ ))+"/../..") 

os.environ['NLTK_DATA'] = os.path.abspath(os.path.join(os.path.dirname(__file__), "./nltk_data"))

DRT_PYTHON = UNLU_HOME + "/src/python-lib"
GSW_TIMEOUT = "300s"
UD2DRS_TIMEOUT = "300s"
TOTAL_TIMEOUT_PER_SENTENCE=650
UD2DRS_EXE="ud2drs-exe"
PYTHON_EXE=sys.executable
GSW_JAR= UNLU_HOME +"/bin/glueSemWorkbench2.jar"
JAVA_EXE="/usr/bin/java"
DRT_PYTHON= UNLU_HOME + "/src/python-lib"
SEARCH_GOAL=""
SENTENCE_NOW=""

seed(42)

# Include the code in the basic DRT-Python library file that we have
sys.path.insert(0, DRT_PYTHON + '/nltk')
import nltk

# inlude pretty printing functions
with open (DRT_PYTHON + "/lib/print_processing.py", "r") as myfile:
    exec(myfile.read())

# include the code to convert NLTK DRSes to clause notation
with open (DRT_PYTHON + "/lib/NLTK_drs_to_PMB_drs.py", "r") as myfile:
    exec(myfile.read(), globals())

from srl.verbnet import *
from srl.srl import * 


# This function returns DRS of a proof using NLTK
def get_drs_of_proof(proof, print_drs_operations=False):
    global myexpr
    try:
        myexpr = nltk.sem.drt.DrtExpression.fromstring(r"{}".format(proof))
        if(print_drs_operations):
            write_stderr("DRS: Loaded " + str(myexpr))
        output = get_stdout('myexpr = myexpr.simplify()')#.eliminate_equality()')
        if(print_drs_operations):
            write_stderr(output)
        return (make_more_pretty(get_stdout('myexpr.pretty_print()')) + "\n")
    except :
        return ("% DRS: " + traceback.format_exc().replace("\n","\n% DRS: "))


# This function returns DRS of a proof using NLTK. It gets an NLTK drs as input
def get_drs_of_proof_nltk_drs(pr,print_drs_operations=False):
    global myexpr
    myexpr=pr
    try:
        return (make_more_pretty(get_stdout('myexpr.pretty_print()')) + "\n")
    except : 
        return ("% DRS: " + traceback.format_exc().replace("\n","\n% DRS: "))



def get_drs_object_of_proof(proof):
    global myexpr
    try:
        myexpr = nltk.sem.drt.DrtExpression.fromstring(r"{}".format(proof))
        output = get_stdout('myexpr = myexpr.simplify()')#.eliminate_equality()')
        return myexpr
    except :
        return ("% DRS: " + traceback.format_exc().replace("\n","\n% DRS: "))


# This funtion returns the DRS of a proof in PMB clause notation
# In case of the proof is a Lambda-DRS it does not produce PMB clause notation. 
# Instead it prints the NLTK DRS
def get_clause_notation_of_proof(proof):
    global myexpr
    try:
        myexpr = nltk.sem.drt.DrtExpression.fromstring(r"{}".format(proof))
        output = get_stdout('myexpr = myexpr.simplify()')#.eliminate_equality()')
        if type(myexpr) == nltk.sem.drt.DrtLambdaExpression:
            return (make_more_pretty(get_stdout('myexpr.pretty_print()')) + "\n")
        return (nltk_drs_to_pmb_drs(myexpr,False) + "\n")
    except :
        return ("% DRS: " + traceback.format_exc().replace("\n","\n% DRS: "))

# Run a command accepting stdin_str as an input to its STDIN
def run_cmd_stdin(cmd, stdin_str):
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.PIPE )
    out, err = p.communicate(input=stdin_str.encode())
    return (out,err, p.returncode)

# Get proofs when an input obtained from the output of UD2DRS is provided 
def parse_premises(inp):
    # Just get the first goal and its premises from ud2drs output
    splitted = inp.split("\n*goal: ")
    # The following for is not meant for looping but just to get the required element
    for piece in splitted:
        if piece =="":
            continue

        # Split into lines
        lines = piece.split("\n")

        # Parse the goal and dummy semantic side corresoponding the goal
        dummy_semantics = ""
        semicolumn_index = lines[0].find(":")
        if semicolumn_index != -1:
            my_goal = lines[0][semicolumn_index+1:].strip()
            dummy_semantics = lines[0][:semicolumn_index].strip()
        else:
            my_goal = lines[0]

        # Parse the premises
        my_premises = []
        for i in range(1,len(lines)):
            line=lines[i]
            if line=="":
                continue
            elif line[0]==' ':
                pass
            else:
                my_premises.append(line)

        # Parse the piece of input corresponding to the subproofs
        new_pieces = piece.split("\n  *goal: ");
        del new_pieces[0]
        new_pieces = ["\n  *goal: " + i for i in new_pieces]

        # Remove the two spaces, since this will be given as parameter to the recursive call
        new_pieces = [i.replace("\n  ","\n") for i in new_pieces]

        # Parse the sub goals and sub-premises of this goal by recursive calling
        for piece in new_pieces:
            my_premises.append(parse_premises(piece))

        # Return everything parsed
        return {"goal":my_goal,"goal_semantics":dummy_semantics,"premises": my_premises}


# Parse reading
def parse_reading(reading):
    index_end_line = reading.index("\n")
    reading_id = int(reading[:index_end_line].replace(" ",""))
    reading = reading[index_end_line:]
    return {"reading_id":reading_id,"goal": parse_premises(reading)}

# Parse only one sentence
def parse_sentence(sentence):
    sentence = sentence.split("** reading")
    sentence_id=sentence[0].replace(" (sent_id=","").replace(")","").replace("\n","").replace(" ","")
    if sentence_id=="":
        return ""
    sentence_id=str(sentence_id)

    return {"sentence_id":sentence_id, "readings": [parse_reading(i) for i in sentence[1:]]}

# Parse all sentences given the lines
def parse_sentences(lines):
    sentences = lines.split("*** sentence")
    return [parse_sentence(i) for i in sentences[1:] ]

# Read the file given as filename into memory
def readFile(file):
    f=open(file,"r")
    r=f.read()
    f.close()
    return r

# Run GSW on the premises given as an array
def gsw(premises, explainFail =False):
    global GSW_JAR
    global JAVA_EXE
    global SEARCH_GOAL
    global GSW_TIMEOUT

    stdin_str="\n".join(premises)

    # Run GSW according to request for an explanation
    # If requested Hepple's algorithm is run to produce an explanation
    if explainFail:
        p = subprocess.Popen(["timeout", GSW_TIMEOUT, JAVA_EXE, "-jar", GSW_JAR,"-readStdIn", "-writeStdOut", "-onlyMeaningSide", "-outputStyle", "3", "-pr", "0", "-assureGlueParsing", "-explainFail", SEARCH_GOAL], stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.PIPE )
    else:
        # Run Lev's algorithm # Disabled Lev's algorithm temporarily until Lev's algorithm works fine.
        p = subprocess.Popen(["timeout", GSW_TIMEOUT, JAVA_EXE, "-jar", GSW_JAR, "-readStdIn", "-writeStdOut", "-onlyMeaningSide", "-outputStyle", "3", "-pr", "0", "-assureGlueParsing", SEARCH_GOAL], stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.PIPE )
    out, err = p.communicate(input=stdin_str.encode())

    # Return STDOUT and STDERR
    return (out.decode(),err.decode())

# This function processes stderr of Glue Semantic Workbench.
# It gets rid of ^[[0m, and ^[[37m that are observed when viewing using vi.
# It also suppresses the lines with 'Combining ... and ... '
def process_gsw_stderr(errstr):
    errstr = errstr.encode()
    errstr = errstr.replace(b'\x1b\x5b\x30\x6d', b'')
    errstr = errstr.replace(b'\x1b\x5b\x33\x33\x6d', b'')
    errstr = errstr.replace(b'\x1b\x5b\x33\x37\x6d', b'')
    errstr = errstr.decode()
    errstr = errstr.split('\n')
    errstr = [x for x in errstr if not x.startswith('Combining ') and not x.startswith('to: ')]
    errstr = '\n'.join(errstr)
    return errstr

# This function returns a list of combination of list elements
# Taken from https://stackoverflow.com/questions/34681797/how-to-generate-all-combinations-of-ranges-of-numbers
def variads(lst, lstsofar, lastState):
    offset = len(lstsofar)
    outerlen = len(lst)
    innerLst = lst[offset]
    printit = False
    if offset == (outerlen - 1):
        printit = True
    for item in innerLst:
        if printit:
            
            lastState.append( lstsofar + [item])
        else:
            variads(lst, lstsofar + [item], lastState)
    return

# This function returns the proofs when the object obtained from parsing ud2drs is provided
def get_proofs(node, explain_fail=False):
    # Set this nodes's goal
    my_goal = node["goal"]

    # Get this node's premises into an array, so initialize it first
    my_premises = []

    # Initialize replacements array for any potential subproof and goal replacements
    sub_proofs={}

    # Iterate over premises of this goal
    for premise in node["premises"]:

        # If the type is dict, this means it is a sub-goal and it will have a sub-proof
        if type(premise)==dict:

            # Get proofs
            local_sub_proofs=get_proofs(premise, explain_fail)

            # Identify the sub-goal
            sub_proof_goal = premise["goal"]

            # Get a hash of it for later replacing the semantic side back
            sub_proof_hash = hashlib.sha256(sub_proof_goal.encode() + str(random()).encode()).hexdigest()

            if sub_proof_hash in sub_proofs:
                sub_proofs[sub_proof_hash].extend( local_sub_proofs)
            else:
                sub_proofs[sub_proof_hash]=local_sub_proofs

            # Add this replacement information to the replacements array.
            my_premises.append(sub_proof_hash + " : " + sub_proof_goal)

        else:
            # Add premises to my_premises as usual if this is not a sub-goal 
            my_premises.append(premise)

    # Add an artificial atomic goal to the premises. This is the goal that we are looking for.
    my_premises.append("MYGOAL: (" + my_goal + " -o v(999999))")

    # Get this node's proof
    my_proofs = gsw(my_premises)


    # Compute combinations of replacements
    to_combine=[]
    for sub_proof_hash in sub_proofs:
        to_combine.append([(sub_proof_hash,sub_proofs[sub_proof_hash][i]) for i in range(len(sub_proofs[sub_proof_hash]))])
    replacement_combinations=[]
    if(len(to_combine)>0):
        variads(to_combine,[],replacement_combinations)

    if len(replacement_combinations)>0:
        # Here we log the proof by replacing the semantic sides according to each combination of the replacements 
        # that were computed before
        for cfg in replacement_combinations:
            current_premises = copy.deepcopy(my_premises)
            current_stderr = copy.deepcopy(process_gsw_stderr(str(my_proofs[1])))
            eprint("The following input premises and GSW log are produced by replacing the semantic side of some premises.")
            for (sub_proof_hash, sub_proof) in cfg:
                # Do the neccessary replacements on input premises and stderr message for this configuration
                for i in range(len(current_premises)):
                    current_premises[i] = current_premises[i].replace(sub_proof_hash, sub_proof)
                current_stderr = current_stderr.replace(sub_proof_hash, sub_proof)

            # stderr out premises
            for premise in current_premises:
                eprint(premise)

            # stdderr out log
            eprint(current_stderr)

    else:
        # If there is nothing to replace just do the normal logging
        for premise in my_premises :
            eprint(premise)
        err_msg=str(my_proofs[1])
        eprint(process_gsw_stderr(err_msg))


    # Parse STDOUT of GSW. Throw away the semantic information introduced by adding the artificial
    # atomic goal
    my_proofs=[ proof for proof in my_proofs[0].split("\n") if not proof.startswith("%") ]
    my_proofs = [proof[7:-1] for proof in my_proofs if proof.startswith("MYGOAL(")  ]


    # Prepare the set of proofs to be returned.
    my_proofs_to_return = [] 
    
    for cfg in replacement_combinations:
        current_my_proofs = copy.deepcopy(my_proofs)
        current_my_proofs_before = copy.deepcopy(my_proofs)
        for (sub_proof_hash, sub_proof) in cfg:
            for i in range(len(current_my_proofs)):
                current_my_proofs[i] = current_my_proofs[i].replace(sub_proof_hash, sub_proof)
        for i in range(len(current_my_proofs)):
             # The effect of the following line will be explored later if needed. Currently we add all proofs regardless of if they
             # are updated with any replacements
#            if current_my_proofs[i] != current_my_proofs_before[i]:
                my_proofs_to_return.append(current_my_proofs[i])

    if len(replacement_combinations)==0:
        for proof in my_proofs:
            my_proofs_to_return.append(proof)


    # If there is no proof, return the dummy semantics
    if len(my_proofs_to_return)==0:
        my_proofs_to_return.append(node["goal_semantics"])
        if explain_fail:
                # Run GSW for this input
                my_proofs = gsw(my_premises, True)

                # Write to STDERR
                eprint("Running GSW for the failing proof of the following premises to get an explanation:")
                if len(replacement_combinations)>0:
                    for cfg in replacement_combinations:
                        current_output = copy.deepcopy(str(my_proofs[0]))
                        current_premises = copy.deepcopy(my_premises)
                        eprint("The following log is produced by replacing the semantic side of some premises.")
                        for (sub_proof_hash, sub_proof) in cfg:
                            for i in range(len(current_premises)):
                                current_premises[i] = current_premises[i].replace(sub_proof_hash, sub_proof)

                            current_output = current_output.replace(sub_proof_hash, sub_proof)

                        for pr in current_premises:
                            eprint(pr)

                        eprint(current_output)
                else:
                    for pr in my_premises:
                        eprint(pr)
                    eprint(my_proofs[0])
                #eprint(my_proofs[1])

    # Do return
    return my_proofs_to_return

def number_of_conditions_without_dummy(clause_notation):
    return len([i for i in [j.strip() for j in clause_notation.split("\n")] if not i=="" and i.find("REF") == -1 and i.find("DUMMY") == -1])

def number_of_confitions_without_dummy_ntlk(drs_object):
    num = 0
    drs_vars=vars(drs_object)

    if "conds" in drs_vars:
        for cond in drs_vars["conds"]:
            type_cnd = type(cond)
            cnd_vars = vars(cond)
            if str(cnd_vars).find("DUMMY") == -1:
                num +=1 
            if type_cnd == nltk.sem.drt.DrtProposition:
                num += number_of_confitions_without_dummy_ntlk(cnd_vars["drs"])
            elif type_cnd == nltk.sem.drt.DrtOrExpression:
                num += number_of_confitions_without_dummy_ntlk(cnd_vars["first"])
                num += number_of_confitions_without_dummy_ntlk(cnd_vars["second"])
            elif type_cnd == nltk.sem.drt.DrtNegatedExpression:
                if type(cnd_vars["term"]) == nltk.sem.drt.DrtEqualityExpression:
                    cnd_vars_2 = vars(cnd_vars["term"])
                    if type(cnd_vars_2["first"]) == nltk.sem.drt.DRS:
                        num += number_of_confitions_without_dummy_ntlk(cnd_vars_2["first"])
                    if type(cnd_vars_2["second"]) == nltk.sem.drt.DRS:
                        num += number_of_confitions_without_dummy_ntlk(cnd_vars_2["second"])
                else:
                    if type(cnd_vars["term"]) == nltk.sem.drt.DRS:
                        num += number_of_confitions_without_dummy_ntlk(cnd_vars["term"])
            elif type_cnd == nltk.sem.drt.DRS:
               if "consequent" in cnd_vars and type(cnd_vars["consequent"])==nltk.sem.drt.DRS:
                   num += number_of_confitions_without_dummy_ntlk(cnd_vars["consequent"])
               num += number_of_confitions_without_dummy_ntlk(cond)
            else:
                if "function" in cnd_vars:
                    if type(cnd_vars["function"]) == nltk.sem.drt.DRS:
                        num += number_of_confitions_without_dummy_ntlk(cnd_vars["function"])
                if "argument" in cnd_vars:
                    if type(cnd_vars["argument"]) == nltk.sem.drt.DRS:
                        num += number_of_confitions_without_dummy_ntlk(cnd_vars["argument"])
    return num




# Returns conllu of a given sentence
def get_conll(sentence, nlp, CoNLL, temp_name="tmp_tmp.conllu"):
    doc = nlp(sentence)
    output=""
    CoNLL.write_doc2conll(doc, temp_name)
    f=open(temp_name,"r")
    output=f.read()
    f.close()
    os.remove(temp_name)
    return output


# Parse arguments
parser = argparse.ArgumentParser(description='Run UNLU pipeline')
parser.add_argument('input_file',  help="input file")
parser.add_argument('rules_file',  help="rules file")
parser.add_argument('chop_rules', help="chopping rules file")
parser.add_argument('templates', help="templates file")
parser.add_argument('-e','--explaination', action='store_true', help='Explain failing cases')
parser.add_argument('-i','--clause-notation-i', action='store_true', help='Output clause notation with -i option in UD2DRS')
parser.add_argument('-c','--clause-notation', action='store_true', help='Output clause notation without -i in UD2DRS')
parser.add_argument('-o','--output-for-comparison', action='store_true', help= 'Output for comparison')
parser.add_argument('-g','--prove-goal', action='store_true', help='Consider a subproof with the longest index covereage as the proof for its proof process')
parser.add_argument('-l','--least-dummy', action='store_true', help='Choose the reading.proof without any DUMMY conditions if any, if not chose the reading.proof with the most number of conditions that are not DUMMY')
parser.add_argument('-s','--sentence', const="en",nargs="?", choices=['en', 'it', 'nl','de','fr'], help='Consider the input file as a list of sentences where each line has one sentence. If this option is not provided, then the input file is considered a conll file. This option uses Stanza to get conll of a sentence. Stanza utilizes CPUs to load the model and produce conll. The language options are en, it, nl, de. The usage is -sit for Italian and -snl for Dutch etc. The default is "en" when only used as -s.')
parser.add_argument('-t','--stanza-lang-code', action="store", default="",type=str, required=False, help='This option is used by the caller script. You do not need to use this option.')
parser.add_argument('-x','--stanza-config',action="store", default="", type=str, required=False, help='This option is used by the caller script. You do not need to use this option.')
parser.add_argument('-r','--semantic-role-labeling', action='store_true', help='Do semantic role labeling.')
parsed_arguments = parser.parse_args()
file_name = parsed_arguments.input_file

# Check files' existence
path = Path(parsed_arguments.input_file)
if not path.is_file():
    raise IOError('File "' + parsed_arguments.input_file + '" could not be opened.')
path = Path(parsed_arguments.rules_file)
if not path.is_file():
    raise IOError('File "' + parsed_arguments.rules_file + '" could not be opened.')
path = Path(parsed_arguments.chop_rules)
if not path.is_file():
    raise IOError('File "' + parsed_arguments.chop_rules + '" could not be opened.')
path = Path(parsed_arguments.templates)
if not path.is_file():
    raise IOError('File "' + parsed_arguments.templates + '" could not be opened.')



if parsed_arguments.prove_goal:
    SEARCH_GOAL = " -proveGoal MYGOAL "

# Get input into memory:
file = open(parsed_arguments.input_file,mode='r')
input_str = file.read()
file.close()

# Check if the file must be considered as a sentence file.
# If so, use Stanza to create conll of sentences and create
# a new input_str to be used by the rest of the script.
if parsed_arguments.sentence:
    new_str=""
    sentence_id=1
    sentences = input_str.split("\n")
    import stanza
    # Override by stanza_config and stanza_lang_code
    if parsed_arguments.stanza_config!="" and parsed_arguments.stanza_lang_code!="":
        nlp=stanza.Pipeline(parsed_arguments.stanza_lang_code, processors=parsed_arguments.stanza_config,use_gpu=False)
    elif parsed_arguments.sentence=="en":
        nlp = stanza.Pipeline('en', processors='tokenize,mwt,pos,lemma,depparse',use_gpu=False)
    elif parsed_arguments.sentence=="it":
        nlp = stanza.Pipeline('it', processors='tokenize,mwt,pos,lemma,depparse',use_gpu=False)
    elif parsed_arguments.sentence=="de":
        nlp = stanza.Pipeline('de', processors='tokenize,mwt,pos,lemma,depparse',use_gpu=False)
    elif parsed_arguments.sentence=="nl":
        nlp = stanza.Pipeline('nl', processors='tokenize,pos,lemma,depparse',use_gpu=False)
    elif parsed_arguments.sentence=="fr":
        nlp = stanza.Pipeline('fr', processors='tokenize,mwt,pos,lemma,depparse',use_gpu=False)

    for sentence in sentences:
        sentence = sentence.strip()
        if sentence =="":
            continue
        conll_of_sentence = get_conll(sentence.strip(), nlp, stanza.utils.conll.CoNLL)
        new_str += "# sent_id = " + str(sentence_id) + "\n# text = " + sentence + "\n" + conll_of_sentence.split("\n",2)[2]
        sentence_id +=1
    input_str=new_str
input_str=input_str.strip() + "\n"
if parsed_arguments.sentence:
    eprint("The following input will be given to UD2DRS from the Stanza parse:")
    eprint(input_str)

# run ud2Drs
if parsed_arguments.clause_notation_i:
    (out,err,rcode) = run_cmd_stdin(["timeout", UD2DRS_TIMEOUT, UD2DRS_EXE, parsed_arguments.rules_file, parsed_arguments.chop_rules, parsed_arguments.templates, "-i", "-n"],input_str)
else:
    (out,err,rcode) = run_cmd_stdin(["timeout", UD2DRS_TIMEOUT, UD2DRS_EXE, parsed_arguments.rules_file, parsed_arguments.chop_rules, parsed_arguments.templates, "-n"],input_str)
err_str=err.decode()
eprint(err_str)
if rcode != 0:
    exit(rcode)

def process_single_sentence():
    global SENTENCE_NOW
    sentence=SENTENCE_NOW
    if not parsed_arguments.output_for_comparison : print("% Sentence " + str(sentence["sentence_id"]))
    chosen_proof=""
    chosen_proof_drs_object=None
    num_of_conditions_without_dummy=-1
    chosen_reading=0
    chosen_proof_id=0
    for reading in sentence["readings"]:
        if not parsed_arguments.output_for_comparison  and not parsed_arguments.least_dummy: print("% Reading " + str(reading["reading_id"]))
        eprint("Sentence " + str(sentence["sentence_id"]) + " reading " + str(reading["reading_id"]))
        proofs=get_proofs(reading["goal"], parsed_arguments.explaination)
        proof_id = 0
        break_readings_also = False
        for proof in proofs:
            drs_object_of_proof = get_drs_object_of_proof(proof)
            if parsed_arguments.least_dummy:
                if proof.count("DUMMY") == 0:
                    chosen_proof = proof
                    chosen_proof_drs_object=drs_object_of_proof 
                    chosen_reading = reading["reading_id"]
                    chosen_proof_id = proof_id

                    # Here we break because we found what we wanted
                    break_readings_also = True
                    break
                else:
                    num_conds = number_of_confitions_without_dummy_ntlk(drs_object_of_proof)
                    if num_conds> num_of_conditions_without_dummy:
                        num_of_conditions_without_dummy = num_conds
                        chosen_proof = proof
                        chosen_reading = reading["reading_id"]
                        chosen_proof_id = proof_id
                        chosen_proof_drs_object=drs_object_of_proof
            elif parsed_arguments.output_for_comparison:
                if parsed_arguments.semantic_role_labeling:
                    drs_object_of_proof = srl(drs_object_of_proof)[0]
                    proof=str(drs_object_of_proof)
                print (str(sentence["sentence_id"])+"\t"+str(reading["reading_id"])+"."+str(proof_id)+"\t" + proof)
            elif parsed_arguments.clause_notation or parsed_arguments.clause_notation_i:
                if parsed_arguments.semantic_role_labeling:
                    drs_object_of_proof = srl(drs_object_of_proof)[0]

                print ("% " + str(sentence["sentence_id"])+"\t"+str(reading["reading_id"])+"."+str(proof_id))
                print (nltk_drs_to_pmb_drs(drs_object_of_proof))
            else:
                if parsed_arguments.semantic_role_labeling:
                    drs_object_of_proof = srl(drs_object_of_proof)[0]

                print (get_drs_of_proof_nltk_drs(drs_object_of_proof))
            proof_id += 1

        # Breaking because we found what we wanted
        if break_readings_also:
            break

    if parsed_arguments.least_dummy:
        eprint("The first non-dummy-including proof, or if not, with the most non-dummy-conditions reading.proof is choosen.")
        eprint("The proof chosen for the sentence is : " + chosen_proof)
        print("% The first non-dummy-including proof, or if not, with the most non-dummy-conditions reading.proof is choosen.")
        if parsed_arguments.output_for_comparison:
            if parsed_arguments.semantic_role_labeling:
                chosen_proof_drs_object = srl(chosen_proof_drs_object)[0]
                chosen_proof=str(chosen_proof_drs_object)
            print (str(sentence["sentence_id"])+"" + "\t"+ chosen_proof)
        elif parsed_arguments.clause_notation or parsed_arguments.clause_notation_i:
            if parsed_arguments.semantic_role_labeling:
                chosen_proof_drs_object = srl(chosen_proof_drs_object)[0]
            print ("% " + str(sentence["sentence_id"])+"\t"+str(chosen_reading)+"."+str(chosen_proof_id))
            print (nltk_drs_to_pmb_drs(chosen_proof_drs_object))
        else:
            if parsed_arguments.semantic_role_labeling:
                chosen_proof_drs_object = srl(chosen_proof_drs_object)[0]
            print (get_drs_of_proof_nltk_drs(chosen_proof_drs_object))


# Get each line of UD2DRS into an array
lines = out.decode()
# Parse UD2DRS output and obtain the tree structure
sentences = parse_sentences(lines)
for sentence in sentences:
    SENTENCE_NOW=sentence
    p = multiprocessing.Process(target=process_single_sentence)
    p.start()

    # Wait for 10 mins or until process finishes
    p.join(TOTAL_TIMEOUT_PER_SENTENCE)

    # If thread is still active
    if p.is_alive():
        eprint ("It has been  " +str(TOTAL_TIMEOUT_PER_SENTENCE) +" seconds  and still processing this sentence. So killing it...")

        p.terminate()

        p.join()
        print("")

