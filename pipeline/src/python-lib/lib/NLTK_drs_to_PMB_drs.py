# This file has a function that converts an NLTK DRS to PMB DRS clause format.
# The NLTK drs is assumed to be produced by this framework. For example,
# it is assumed that the NLTK DRS is produced by the pipeline or by converting
# a PMB DRS to an NLTK DRS using DRS_pmb_clause2_nltk in PMBProcessing.py. 

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)) + '/nltk')
import nltk
import re

ignore_makeup_wordnet ={"PRESUPPOSITION" }

# Returns the highest box id given a DRS
# This is needed to enumerate unnamed boxes and for these numbers not to
# conflict with the ones that are already in the DRS 
def get_highest_box_id(drs, max_num):
    drs_vars = vars(drs)
    my_max_num = max_num
    if "conds" in drs_vars:
        for cond in drs_vars["conds"]:
            type_cnd = type(cond)
            cnd_vars = vars(cond)
            if type_cnd == nltk.sem.drt.DrtProposition:
                box_id = int(str(cnd_vars["variable"])[1:])
                if box_id > my_max_num:
                    my_max_num = box_id
                box_id = get_highest_box_id(cnd_vars["drs"], my_max_num)
                if box_id > my_max_num:
                    my_max_num = box_id
            elif type_cnd == nltk.sem.drt.DrtOrExpression:
                box_id = get_highest_box_id(cnd_vars["first"], my_max_num)
                if box_id > my_max_num:
                    my_max_num = first_max
                box_id = get_highest_box_id(cnd_vars["second"], my_max_num)
                if box_id > my_max_num:
                    my_max_num = box_id
            elif type_cnd == nltk.sem.drt.DrtNegatedExpression:
                box_id = get_highest_box_id(cnd_vars["term"], my_max_num)
                if box_id > my_max_num:
                    my_max_num = box_id
    return my_max_num

# This function extracts the DRSes that does not have any box ids. 
# It increments the box id each time it finds a DRS that does not have an id
def extract_DRSes(DRSes, drs_id, highest_box_id):
    drs_vars=DRSes[drs_id]
    if "conds" in drs_vars:
        for i in range(len(drs_vars["conds"])):
            if i>=len(drs_vars["conds"]):
                break
            cond = drs_vars["conds"][i]
            type_cnd = type(cond)
            cnd_vars = vars(cond)
        
            if type_cnd == nltk.sem.drt.DrtProposition:
                
                new_drs_id = str(cnd_vars["variable"]) 
                DRSes[new_drs_id] = vars(cnd_vars["drs"])
                highest_box_id = extract_DRSes(DRSes, new_drs_id, highest_box_id)
                del DRSes[drs_id]["conds"][i]
                i -=1
                continue
            elif type_cnd == nltk.sem.drt.DrtApplicationExpression and type(cnd_vars["argument"]) == nltk.sem.drt.DRS:
                
                new_drs_id = "b" + str(highest_box_id)
                DRSes[new_drs_id] = vars(cnd_vars["argument"])
                DRSes[drs_id]["conds"][i]={"type":"app", "function": cnd_vars["function"], "argument":new_drs_id }
                highest_box_id += 1
                highest_box_id = extract_DRSes(DRSes, new_drs_id, highest_box_id)

            elif type_cnd == nltk.sem.drt.DrtOrExpression:
                DRSes[drs_id]["conds"][i] = {"type":"DIS", "first":cnd_vars["first"], "second": cnd_vars["second"]}
                if type(cnd_vars["first"]) == nltk.sem.drt.DRS:
                    new_drs_id = "b" + str(highest_box_id)
                    DRSes[new_drs_id] = vars(cnd_vars["first"])
                    DRSes[drs_id]["conds"][i]["first"] = new_drs_id
                    highest_box_id += 1
                    highest_box_id = extract_DRSes(DRSes, new_drs_id, highest_box_id)
                if type(cnd_vars["second"]) == nltk.sem.drt.DRS:
                    new_drs_id = "b" + str(highest_box_id)
                    DRSes[new_drs_id] = vars(cnd_vars["second"])
                    DRSes[drs_id]["conds"][i]["second"] = new_drs_id
                    highest_box_id += 1
                    highest_box_id = extract_DRSes(DRSes, new_drs_id, highest_box_id)
            elif type_cnd == nltk.sem.drt.DrtNegatedExpression:
                if type(cnd_vars["term"]) == nltk.sem.drt.DrtEqualityExpression:
                    cnd_vars_2 = vars(cnd_vars["term"])
                    DRSes[drs_id]["conds"][i] = {"type":"NEQ", "first":cnd_vars_2["first"], "second": cnd_vars_2["second"]}
                    if type(cnd_vars_2["first"]) == nltk.sem.drt.DRS:
                        new_drs_id = "b" + str(highest_box_id)
                        DRSes[new_drs_id] = vars(cnd_vars["first"])
                        DRSes[drs_id]["conds"][i]["first"] = new_drs_id
                        highest_box_id += 1
                        highest_box_id = extract_DRSes(DRSes, new_drs_id, highest_box_id)
                    if type(cnd_vars_2["second"]) == nltk.sem.drt.DRS:
                        new_drs_id = "b" + str(highest_box_id)
                        DRSes[new_drs_id] = vars(cnd_vars_2["second"])
                        DRSes[drs_id]["conds"][i]["second"] = new_drs_id
                        highest_box_id += 1
                        highest_box_id = extract_DRSes(DRSes, new_drs_id, highest_box_id)
                else:
                    DRSes[drs_id]["conds"][i] = {"type":"NOT", "term": cnd_vars["term"]}
                    if type(cnd_vars["term"]) == nltk.sem.drt.DRS:
                        new_drs_id = "b" + str(highest_box_id)
                        DRSes[new_drs_id] = vars(cnd_vars["term"])
                        DRSes[drs_id]["conds"][i]["term"] = new_drs_id
                        highest_box_id += 1
                        highest_box_id = extract_DRSes(DRSes, new_drs_id, highest_box_id)
            elif type_cnd == nltk.sem.drt.DRS:
                if "consequent" in cnd_vars and type(cnd_vars["consequent"])==nltk.sem.drt.DRS:
                    imp_drs = cnd_vars["consequent"]
                    cnd_vars["consequent"] = None

                    new_drs_id1 = "b" + str(highest_box_id)
                    DRSes[new_drs_id1] = cnd_vars
                    highest_box_id += 1
                    highest_box_id = extract_DRSes(DRSes, new_drs_id1, highest_box_id)

                    new_drs_id2 = "b" + str(highest_box_id)
                    DRSes[new_drs_id2] = vars(imp_drs)
                    highest_box_id += 1
                    highest_box_id = extract_DRSes(DRSes, new_drs_id2, highest_box_id)

                    DRSes[drs_id]["conds"][i] = {"type" : "IMP", "first" : new_drs_id1, "second" : new_drs_id2}

                else:
                    print ("Unhandled piece of drs. Check here and update accordingly to handle")
                    print (cnd_vars)
                    exit(0)
    return highest_box_id

# Returns the clause notation of the DRSes given as paramter.
def drs_clause_notation(DRSes, DRS_clause, make_up_wordnet):
    global ignore_makeup_wordnet
    for drs_id in DRSes:
        my_drs = DRSes[drs_id]
        if "refs" in my_drs:
            for ref in my_drs["refs"]:
                DRS_clause += drs_id+ " REF " + str(ref) + "\n"
        if "conds" in my_drs:
            for cond in my_drs["conds"]:
                type_cond =type(cond)
                if type_cond == dict and "type" in cond:
                    if "term" in cond:
                        rest = cond["term"]
                    elif "function" in cond:
                        function = cond["function"]
                        argument = cond["argument"]
                        # If this is presupposition output b2 PRESUPPOSITION b1 instead of b1 PRESUPPOSITION b2
                        if str(function).strip() == "PRESUPPOSITION":
                            rest = process_function_argument(function, argument, make_up_wordnet).split(" ")
                            DRS_clause += rest[1] + " PRESUPPOSITION " + drs_id + "\n"
                        else:            
                            DRS_clause += drs_id + " " + process_function_argument(function, argument, make_up_wordnet) + "\n"

                        rest = ""
                        continue
                    else:
                        first = str(cond["first"])
                        second = str(cond["second"])
                        first = first.replace("`", "\"").replace("~dot~", ".")
                        second = second.replace("`", "\"").replace("~dot~", ".")
                        rest = first+" "+second
                    DRS_clause += drs_id + " " + cond["type"] + " " + rest + "\n"
                elif type_cond == nltk.sem.drt.DrtEqualityExpression:
                    vars_cond = vars(cond)
                    first = str(vars_cond["first"])
                    second = str(vars_cond["second"])
                    first = first.replace("`", "\"").replace("~dot~", ".")
                    second = second.replace("`", "\"").replace("~dot~", ".")
                    DRS_clause += drs_id + " EQU " + first + " " + second + "\n"
                elif type_cond == nltk.sem.drt.DrtConstantExpression:
                    vars_cond=vars(cond)
                    if "variable" in vars_cond and str(vars_cond["variable"]) == "DUMMY":
                        DRS_clause += drs_id + " REF DUMMY " + "\n"
                    else:
                        var_var=vars(vars_cond["variable"])
                        DRS_clause += drs_id + " "+str(vars_cond["variable"])+" % This is an unexpected constant!" + "\n" 
                elif "term" in vars(cond) :
                    vars_cond = vars(cond)
                    if type(vars_cond["term"]) == nltk.sem.drt.DrtEqualityExpression:
                        eqvars = vars(vars_cond["term"])
                        first = str(eqvars["first"])
                        second = str(eqvars["second"])
                        first = first.replace("`", "\"").replace("~dot~", ".")
                        second = second.replace("`", "\"").replace("~dot~", ".")
                        DRS_clause += drs_id + " EQU " + first + " " + second + "\n"
                    else:
                        print("This is not expected. If you see this, you need the handle here.")
                else :
                    vars_cond = vars(cond)
                    function = str(vars_cond["function"])
                    argument = str(vars_cond["argument"])
                    DRS_clause += drs_id + " " + process_function_argument(function, argument, make_up_wordnet) + "\n"
    return DRS_clause

# This function mainly process function and argument style conditions
def process_function_argument(function, argument, make_up_wordnet):
                    function_o=function
                    argument_o=argument
                    function = str(function)
                    argument = str(argument)
                    function = function.replace("(", " ").replace(")","").replace("_","-")
                    argument = argument.replace("`","\"").replace("~dot~", ".")
                    if type(argument_o)==nltk.sem.drt.DrtConstantExpression and argument[0] != "\"":
                        argument = "\"" + argument + "\""

                    matched = re.match(r".*`[a-z]`[0-9][0-9]`", function)
                    if matched:
                        function = function.replace("`", " \"", 1).replace("`", ".",1).replace("`","\"")
                    elif make_up_wordnet:
                        function_splt = function.split(" ")
                        if len(function_splt)==1 and function not in ignore_makeup_wordnet:
                            function = function + " \"n.01\""
#                            if function[0] not in ignore_makeup_wordnet:
#                                function[0] = function[0] + " \"n.01\""
#                        function = " ".join(function)
                    return function.replace("`","\"") + " " + argument 

# The main function that takes an NLTK drs and returns a string of clause notation
def nltk_drs_to_pmb_drs (drs, make_up_wordnet = True):
    highest_box_id = get_highest_box_id(drs,0)
    highest_box_id += 1
    DRSes = {}
    drs_id = "b" + str(highest_box_id)
    DRSes[drs_id] = vars(drs)
    highest_box_id += 1
    highest_box_id = extract_DRSes(DRSes, drs_id, highest_box_id)
    DRS_clause = ""
    DRS_clause = drs_clause_notation(DRSes, DRS_clause, make_up_wordnet)
    return DRS_clause

