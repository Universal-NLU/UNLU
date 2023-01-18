# This file includes code to compare two NLTK DRSes logically. The main 
# function to be called is compareTwoDRSes. This function gets two 
# NLTK DRSes (either as string or as DRS objects). It returns True if the 
# DRSes are logically equavalent. False otherwise.
# It converts an NLTK DRS string to FOL using PDRT-Sandbox. So, path to DRSToFOL
# executable must be set correctly in PDRT_COMPILED_EXECUTABLE.
# It also uses NLTK and PROVER9 to do the comparison. So, NLTK must be installed
# and could be import-able. PROVER9 binary path must be set as environment variable
# in os.environ["PROVER9"].
import os
import re
import sys
import copy
sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)) + '/nltk')
import nltk
import functools

os.environ["PROVER9"] = "prover9/LADR-2009-11A/bin/"
PDRT_COMPILED_EXECUTABLE="pdrt/DRSToFOL"

EQUALITY_FUNCTION_NAME="equals"
SEARCH_SET = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","r","s","t","u","v","x","y","z", "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","R","S","T","U","V","X","Y","Z","0","1","2","3","4","5","6","7","8","9"]
SEARCH_SET = set(SEARCH_SET)
def process_unneccessary_equality(inp):
    # This function replaces any PDRS FOL equality expression in form of 
    # eq(w,e1,e2) with e1=e2 . This is required to for PROVER9 to interpret FOL.
#    print(inp)
    global EQUALITY_FUNCTION_NAME
    matches = []
    start = 0
    inc_len = len(EQUALITY_FUNCTION_NAME)
    while start < len(inp):
        s = inp.find(EQUALITY_FUNCTION_NAME + "(", start)
        if s == -1:
            break
        if s + inc_len >= len(inp):
            break
        s1 = inp.find(")", s + inc_len)
        matches.append((s,s1))
        start = s + inc_len
#        print(inp[s:s1])
    pointer = 0
    matches.reverse()
    for (start,end) in matches:
        string = inp[start+inc_len+1:end]
        equal_index = string.find("=")
        first_operand = ""
        second_operand = ""
        position = equal_index -1
        char = string[position:position+1]
        while char in SEARCH_SET:
            first_operand = char + first_operand
            position = position -1
            char = string[position:position+1]
        position = equal_index +1
        char = string[position:position+1]
        while char in SEARCH_SET:
            second_operand += char
            position = position +1
            char = string[position:position+1]
#        if second_operand[0:1].isnumeric():
#            inp = inp[0:start] + " " + first_operand + "='" + second_operand + "' " + inp[end+1:]
#        else:
        inp = inp[0:start] + "("+first_operand + "=" + second_operand+")" + inp[end+1:]
    return inp


def pdrs_sandbox_drs_to_fol(inp):
    # This function is used to convert a DRS in PDRS sandbox string notation to FOL.
    # To do this, this function calls PDRS sandbox based DRSToFOL binary for 
    # conversion. Output of this operation is used to syntactically convert FOL
    # expressions to FOL expressions interpretable by NLTK (prover9).
    # This function uses process_unneccesary_equality to deal with equality 
    # expressions that do not exist in PDRS-Sandbox DRSes.
    global PDRT_COMPILED_EXECUTABLE
    inp = inp.replace("`", "\\`")
    output = os.popen('echo "' + inp  + '" | ' + PDRT_COMPILED_EXECUTABLE)
    output = output.read()
    output = re.sub('∃([a-zA-Z][a-zA-Z]*[1-9]*[0-9]*)', r'exists \1. ', output)
    output = re.sub('∀([a-zA-Z][a-zA-Z]*[1-9]*[0-9]*)', r'all \1. ', output)    
    output = output.replace ("∧", "&").replace("λ","\\").replace("→","->")
    output = output.replace("imp", "->")
    output = output.replace("¬"," -").replace("⊤"," (exists x. x<->x)  ")
    output = output.replace("⊥"," -(exists x. x<->x)  ").replace("∨"," | ")
    output = output.replace("\n","")
    output = process_unneccessary_equality(output)
    output = output.replace("<->", "=")
    # Update speacial characters that Proover9 does not accept
    #    output = output.replace("~","_").replace("`","_")
    output = re.sub("[^0-9a-zA-Z.\(\)\ &|,<>\!=\-\\\\]+", "_", output)
    return output

def get_nltk_string(nltk_var):
    # This function gets an NLTK DRS object and returns its string representative.
    # Here, we are not using NLTK's own string converter because, we need special
    # handling of equality and box notation (b1: ([...][...]) ) type of conditions
    # to be fed as input to PDRS Sandbox DRS parser.
    # Basically, this function is called recursively to identify these type of
    # conditions. When found, it creates forms its own representative string.
    # In other conditions, it uses NLTK's own converter.
    # Here we also change order or conditions. This is because P-DRS interpreter
    # can only accept sub DRS definitions that are at the beginning of the 
    # conditions. So we need to arrenge it here too.

    global EQUALITY_FUNCTION_NAME
    my_var=vars(copy.deepcopy(nltk_var))
    refs = ""
    conds = ""
    if ("refs" in my_var):
        refs = "[" + ",".join([str(i) for i in my_var["refs"]]) + "]"
    if ("conds" in my_var):
        conds = ""
        for cnd in my_var["conds"]:
            str_cnd = str(cnd)
            type_cnd = type(cnd)
            cnd_vars = vars(cnd)
            if type_cnd == nltk.sem.drt.DrtProposition:
                conds += str(cnd_vars["variable"])+":" + get_nltk_string(cnd_vars["drs"]) + ","
            elif type_cnd == nltk.sem.drt.DrtApplicationExpression and str_cnd[0:5] == "prop(":
                function_vars = vars(cnd_vars["function"])
                function_name = get_nltk_string(function_vars["argument"])
                # Here we change the order of the conditions.
                # We add this condition to the beginning of the condition list
                conds = function_name + ":" + get_nltk_string(cnd_vars["argument"]) + "," + conds
            elif type_cnd == nltk.sem.drt.DrtEqualityExpression:
                first = get_nltk_string(cnd_vars["first"])
                second = get_nltk_string(cnd_vars["second"])
                conds += EQUALITY_FUNCTION_NAME + "(" + first + "=" + second + "),"
            elif type_cnd == nltk.sem.drt.DrtOrExpression:
                first = get_nltk_string(cnd_vars["first"])
                second = get_nltk_string(cnd_vars["second"])
                conds += first + " | " + second + ","
            else:
                conds += get_nltk_string(cnd) + ","
        conds = "[" + conds[0:-1] + "]"
    if refs == "" and conds == "":
        return str(nltk_var)  
    return "(" + refs + conds + ")"

def nltk_drs_to_pdrs_sandbox_drs(inp):
    # This function does a syntactic convertion of an NLTK DRS string expression to
    # PDRS sandbox DRS string expression. 
    if inp == "":
        return inp
#    try:
    nltk_rep = nltk.sem.drt.DrtExpression.fromstring(r"{}".format(inp))
    nltk_rep = get_nltk_string(nltk_rep)
#    print(nltk_rep)
#    except:
#        print ("Not a valid NLTK DRS input")
#        exit(0)
    output = nltk_rep
    output = output.replace("][","}, {").replace("],["," }, {").replace("([","<{ ").replace("])"," }>").replace("-", "not").replace("|", " or ")
    if (output[0]=="("):
        output = output[1:]
    if (output[-1:]==")"):
        output = output[0:-1]
    return output

def nltk_drs_to_fol(inp):
    # This takes a NLTK DRS string and converts it into PDRS sandbox DRS string
    # using nltk_drs_to_pdrs_sandbox_drs. Then converts this string into NLTK FOL
    # expression using pdrs_sandbox_drs_to_fol.
    # This function is actually an abstract function to make conversion easy using
    # PDRS-sandbox
    return pdrs_sandbox_drs_to_fol(nltk_drs_to_pdrs_sandbox_drs(inp))

def compareTwoDRSes(drs1, drs2):
    # The main function to do the comparison.
    # get FOL for NLTK DRSes and check equiv.
    from nltk.inference import Prover9
    prover = Prover9(timeout=600)
    drs1 = str(drs1)
    drs2 = str(drs2)
    fol1 = nltk.sem.logic.Expression.fromstring(nltk_drs_to_fol(drs1))
    fol2 = nltk.sem.logic.Expression.fromstring(nltk_drs_to_fol(drs2))
    if(fol1.equiv(fol2, prover) ):
        return True
    return False

def getConditionString(condition):
    type_cnd = type(condition)
    try:
        if type_cnd == nltk.sem.drt.DrtApplicationExpression:
            condition_vars = vars(condition)
            return condition_vars["function"]
    except:
        return ""

def compareCondition(cond1, cond2):
    str1 = getConditionString(cond1)
    str2 = getConditionString(cond2)
    if(str1 < str2):
        return -1
    elif (str2> str2):
        return 1
    return 0

def sortConditions(drs):
    conditions = list(drs["conds"])
    print(conditions)
    conditions.sort(key=getConditionString)

    print(conditions)
    exit(0)


def syntacticComparison(drs1, drs2):
    # This function takes two NLTK drs objects as inputs and
    # Compares these DRSes syntactically. If they are syntactically equal,
    # it returns True. Otherwise, False.

    # Simplify DRSes
    drs1 = drs1.simplify()
    drs2 = drs2.simplify()

    # Eliminate equalities
    drs1 = drs1.eliminate_equality()
    drs2 = drs2.eliminate_equality()

    # Get variables
    drs1 = vars(drs1)
    drs2 = vars(drs2)

    # TODO: Eliminate same conditions if any

    # Check if they have the same number of referents. If not, return false
    if len(drs1["refs"]) != len(drs2["refs"]):
        return False

    # Check if they have the same number of conditions. If not, return false
    if len(drs1["conds"]) != len(drs2["conds"]):
        return False

    # Sort DRS conditions
    drs1 = sortConditions(drs1)
    drs2 = sortConditions(drs2)


#    if len(vars

    print(variables)
    drs1 = vars(drs1)
    drs2 = vars(drs2)
    print(drs1)
    print(drs2)
    return False


#inp2="([E,F2],[arrive(E), (([j1][someDef(j1)]) | ([l1][someOtherDef(l1)])) nsubj(E,F2), named(F2,karla), ant(F2), owner(F2)])"
#inp1= "([E,F1,F2,X],[arrive(Z), nsubj(E,F2), named(F1,karla), (F2 = F1), ant(X), owner(X), (F2 = X), b1: ([N1][])])"

#drs1=nltk.sem.drt.DrtExpression.fromstring(r"{}".format(inp1))
#drs2=nltk.sem.drt.DrtExpression.fromstring(r"{}".format(inp2))
#print(syntacticComparison(drs1,drs2))
