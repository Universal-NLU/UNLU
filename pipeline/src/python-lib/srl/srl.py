import sys
from srl import verbnet
import re
import argparse
import copy

from nltk.sem.drt import *
dexpr = DrtExpression.fromstring

IGNORED_RELATIONS = ['advmod', 'advcl']

VERBNET_TO_PMB = {"Initial-Time" : "Start",
                  "Final-Time" : "Finish",
                  "Initial-Location" : "Source",
                  "Final-Location" : "Destination",
                  "Result" : "Goal",
                  "Trajectory" : "Path",
                  "Product" : "Result",
                  "Material" : "Source",
                  "Predicate" : "Attribute",
                  "Reflexive" : "Theme" }

# Find the unary predicate that introduces the variable *var* in *drs*. Raise an error if not unique. We may want to introduce explicit Event(X) predicates to be more certain.
def find_predicate(var, conds):
    candidates = [c.pred for c in conds if type(c) is DrtApplicationExpression and str(c.pred)[0].islower() and c.args == [var]]
    if len(candidates) == 1:
        return str(candidates[0])
    else:
        raise verbnet.AmbiguityError(f"Several unary predicates {candidates} for {var} in {conds}")
    return 

# return the string without an offset (e.g. §4§), and the offset
def remove_index(string):
    pred, offset = re.search(r"(^[^§]*)(§\d+§)?", string).group(1,2)
    if offset is None : offset = ''
    return pred, offset

# return the +strings without offsets of+ underspecified binary predicates introducing arguments of the event given by +var+.
def find_args(var, conds):
    return [remove_index(str(c.pred))[0] for c in conds if underspecified(c) and c.args[0] == var ]


def deal_with_passives(args, full_frame):
    if "nsubj_pass" in args:
        if "(csubj|nsubj)" in full_frame:
            del full_frame["(csubj|nsubj)"]
        if "obj" in full_frame:
            full_frame['nsubj_pass'] = full_frame['obj']
            del full_frame['obj']
    return args, list(full_frame.keys()).copy(), full_frame


# returns the number of positions in the frame that are not filled by the args
# positions in the frame are given by regexes
def compare_keys(args, frame, full_frame, mapping):
    for pos in frame:
        for arg in args:
            if re.match(pos, arg):
                mapping[arg]=pos
                args.remove(arg)
                frame.remove(pos)
                compare_keys(args, frame, full_frame, mapping)
    # deal with unresolved roles
    if "gf" in args:
        unfilled = [f for f in ['(csubj|nsubj)', 'obj', 'iobj'] if f in frame]
        if unfilled:
            pos = unfilled[0]
            mapping['gf']=pos
            args.remove('gf')
            frame.remove(pos)
    # we have a mapping, but we want to turn this into the real mapping by substituting in the ud_frames
    real_mapping = { k : full_frame[mapping[k]].replace("-", "_") for k in mapping }
    # Now check if there incoherent args (not licensed by the frame); if so, this is a bad match anyway
    if any(a in ['nsubj', 'obj', 'iobj', 'ccomp', 'that_ccomp', 'whether_ccomp', 'xcomp'] for a in args):
        return len(frame), 99, real_mapping
    else:
        return len(frame), len(args), real_mapping

# run the mapping through some substitutions and deal with offsets
def pmbify(mapping, pred, args):
    p, o = remove_index(pred)
    if mapping[p] in ['Theme', 'Stimulus'] and type(args[1]) is DRS:
        return dexpr(f"Proposition{o}({args[0]},{args[1]})")
    elif mapping[p] in VERBNET_TO_PMB:
        return dexpr(f"{VERBNET_TO_PMB[mapping[p]]}{o}({args[0]},{args[1]})")
    else:
        return dexpr(f"{mapping[p]}{o}({args[0]},{args[1]})")

# takes a condition in need of translation and returns a dictionary
# with the event variable as key and the possible mappings for its arguments as a value
# *predicate* is here the string of the unary predicate introducing the verb
def translations_for_condition(predicate, event_var, args):
    ud_frames = verbnet.find_frames(predicate)
    scored_frames = [compare_keys(*deal_with_passives(args.copy(), frame), {}) for frame in ud_frames ]
    n = 0
    while n < 10:
        best_frames = [ f for f in scored_frames if f[0] == 0 and f[1] == n]
        if best_frames:
            # f[0] is the arg_score, f[1] is the frame_score, f[2] is the mapping
            return { event_var : [f[2] for f in best_frames] }  
        n += 1
    # insert an empty mapping to make sure we don't redo the work for
    # this variable. We remove this mapping at the end.
    return {event_var : [] }
        
# iteratively build a dictionary with event variables as keys and possible mappings as values
# this works over a list of conditions
def get_translations(conds):
    mappings = {}
    for c in conds:
        if underspecified(c) and not first_argument(c) in mappings:
            try:
                pred = find_predicate(c.args[0], conds)
            except verbnet.AmbiguityError as e:
                print(e, file=sys.stderr)
                break
            mappings.update(translations_for_condition(remove_index(pred)[0], first_argument(c), find_args(c.args[0], conds)))
    return { k : v for k, v in mappings.items() if v != [] }

    
# return the first argument of c as a string
def first_argument(c):
    return str(c.args[0])

# TODO: other kinds of embedded DRSs (negation, disjunction) that aren't so far produced by our pipeline
def direct_sub_drss(drs):
    arg_drss = [drs_argument(c) for c in drs.conds if drs_argument(c) ]
    arg_drss = arg_drss + [c for c in drs.conds if type(c) is DRS and c.consequent]
    if drs.consequent:
        arg_drss = arg_drss + [drs.consequent]
    return arg_drss

def sub_drss(drs):
    dir_sub = direct_sub_drss(drs)
    return dir_sub + flatten(sub_drss(a) for a in dir_sub)

# return the second argument of a condition is it is a DRS; otherwise
# the first argument of the condition if that is aDRS; otherwise
# +None+
# Check: do we have conditions with two DRS arguments?
def drs_argument(c):
    if type(c) is DrtApplicationExpression and len(c.args) == 2 and type(c.args[1]) is DRS:
        return c.args[1]
    elif type(c) is DrtApplicationExpression and len(c.args) > 0 and type(c.args[0]) is DRS:
        return c.args[0]
    else:
        return None


# return +True+ if this is an underspecified condition subject to translation
def underspecified(c):
    return type(c) is DrtApplicationExpression and (not(type(c) is DrtEqualityExpression)) and str(c.pred)[0].islower() and str(c.pred)[0] not in IGNORED_RELATIONS and len(c.args) == 2

def flatten(lst):
    return [item for sublist in lst for item in sublist]

# build a list of all conditions in the DRS and embedded DRSs
# for now we only deal with DRSs as second arguments and implications, this could be fixed in direct_sub_drss
def extract_conditions(drs):
    return drs.conds + flatten(extract_conditions(d) for d in direct_sub_drss(drs))

# So can we iterate over the keys in translations, each of which corresponds to at least one rewriting
def perform_translations(drs, translations):
    if translations != {}:
        var = list(translations.keys())[0]
        mappings = translations.pop(var)
        drss = [drs]
        # each time we have more than one mapping, we need to create copies
        for i in range(0,len(mappings) - 1):
            drss.append(copy.deepcopy(drs))
        for n, d in enumerate(drss):
            perform_translation(d, var, mappings[n])
        return flatten(perform_translations(d, copy.deepcopy(translations)) for d in drss)
    else:
        return [drs]
        
# given a single mapping, replace conditions in the DRS modifying the
# object in-place
def perform_translation(drs, var, mapping):
    # 1) perform all translations in the current drs
    drs.conds = [pmbify(mapping, str(c.pred), c.args) if underspecified(c) and remove_index(str(c.pred))[0] in mapping and first_argument(c) == var else c for c in drs.conds]
    # 2) perform all translations in any embedded drss
    for sub_drs in sub_drss(drs):
        perform_translation(sub_drs, var, mapping)
    return        

# This is the main function that performs Verbnet-based SRL on an NLTK drs object and returns a list of DRSs with all possible role resolutions.
def srl(drs):
    # 1) Deal with the special case of "to" with second argument DRS, which we translate into Purpose
    drs.conds = [dexpr(f"Purpose({c.args[0]},{c.args[1]})") if type(c) is DrtApplicationExpression and str(c.pred) == "to" and type(c.args[1]) is DRS else c for c in drs.conds]
    for sub_drs in sub_drss(drs):
        sub_drs.conds = [dexpr(f"Purpose({c.args[0]},{c.args[1]})") if type(c) is DrtApplicationExpression and str(c.pred) == "to" and type(c.args[1]) is DRS else c for c in sub_drs.conds]
    # 2) get a list of all conditions
    conditions = extract_conditions(drs)
    # 3) use that to build a dictionary of translations
    translations = get_translations(conditions)
    # 4) and now do the translation on a copy of the drs
    return perform_translations(copy.deepcopy(drs), translations)
