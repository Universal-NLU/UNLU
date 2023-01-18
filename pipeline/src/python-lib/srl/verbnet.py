import os
os.environ['NLTK_DATA'] = os.path.abspath(os.path.join(os.path.dirname(__file__), "../nltk_data"))

from nltk.corpus import verbnet as vn

import itertools
import re
import sys


PREPOSITIONS = "(above|across|against|along|among|around|at|before|behind|below|beneath|beside|between|by|down|from|in|into|near|of|off|on|to|toward|under|upon|with|within)"


def pairwise(iterable):
    a, b = itertools.tee(iterable)
    next(b, None)
    return zip(a, b)

class AmbiguityError(BaseException):
    pass

# return a list with the verbnet class and its superclasses
def with_superclasses(vnclass):
    return [vnclass] + _superclasses(vnclass)

# unfortunately, verbnet is inconsistent with the superframes. We get
# 'learn-14-2-1' but 'give-13.1-1', so accept both
def _superclasses(vnclass):
    superclass = re.sub(r'(\w+-\d+[.-]\d+.*)-\d+$', r"\1", vnclass)
    if vnclass == superclass:
        return []
    else:
        return [superclass] + _superclasses(superclass)

# we might not have enough info here (theme -> topic if a drs)
def _themrole(e):
    return e['modifiers']['value']

def find_criteria(synrestrs):
    criteria = []
    for synrestr in synrestrs:
        if synrestr['value'] == "+":
            criteria.append(synrestr['type'])
    if len(criteria) == 0:
        return("obj")
    elif len(criteria) > 1:
        raise AmbiguityError(f"Multiple criteria {criteria}")
    else: 
        if criteria[0] in ["plural", "poss_ing"]:
            return("obj")
        # what with "sentential"? only in "I feel like the RIAA has too much power" so ignore for now
        elif criteria[0] in ["quotation"]:
            return("ccomp")
        elif criteria[0] in ["that_comp"]:
            return(".*ccomp")
        elif criteria[0] in ["wh_comp"]:
            return(".*ccomp")
        elif criteria[0] in ["for_comp"]:
            return("for")
        # since for comps will be advcl in the UD, they will only have a for relation, not for-comp
        # To be really sure we are dealing with a complement (and not
        # say, the preposition for) we should check that the
        # complement is a DRS but ok for now. Note that we take
        # arbitrary control infinitives to be xcomp since that's what
        # they usually are in UD (despite the guidelines)
        elif criteria[0] in ["oc_to_inf", "what_inf", "ac_to_inf", "np_to_inf", "sc_to_inf", "small_clause", "np_ppart"]:
            return("xcomp")
        elif criteria[0] == "genitive":
            return("")
        else:
            return(f"obj({criteria[0]})")

def find_frames(lemma, verbose=False):
    ud_frames = []

    vn_classes = [ c for cl in vn.classids(lemma) for c in with_superclasses(cl) ]
    
    for id in vn_classes:
        v = vn.vnclass(id)

        for frame in vn.frames(v):
            if verbose:
                print(frame['syntax'], file=sys.stderr)
                print(frame['example'], file=sys.stderr)
            ud_frame = {}
            preverbal = True
            preceding_prep = False
            for i, e in enumerate(frame['syntax']):
                # question: how do we deal with pre_marked
                # complements ("forced him into coming" and the
                # like)??
                # preverbal NPs are subjects (clausal or nominal)
                if e['pos_tag'] == 'NP' and preverbal:
                    ud_frame['(csubj|nsubj)'] = _themrole(e)
                # ignore the verb
                elif e['pos_tag'] == 'VERB':
                    preverbal = False
                # ignore other preverbal elements (mostly expletives anyway)
                elif preverbal:
                    pass
                # assume *NP* in VERB *NP* NP is an iobj (will be
                # changed to dobj if there turns out not to be any
                # other dobj)
                elif e['pos_tag'] == 'NP' and frame['syntax'][i-1]['pos_tag'] == 'VERB' and len(frame['syntax']) > i + 1 and frame['syntax'][i+1]['pos_tag'] == 'NP':
                    ud_frame['iobj'] = _themrole(e)
                # for other postverbal NPs not preceded by PREP we
                # need to figure out based on the synrestrs
                elif e['pos_tag'] == 'NP' and not preceding_prep:
                    rel = find_criteria(e['modifiers']['synrestrs'])
                    if rel != "":
                        ud_frame[rel] = _themrole(e)
                # Only NP case left will be if preceded by a PREP
                elif e['pos_tag'] == 'NP':
                    for prep in preceding_prep.strip().split():
                        ud_frame[prep] = _themrole(e)
                    preceding_prep = False
                elif e['pos_tag'] == 'PREP':
                    preceding_prep = _themrole(e)
                    if preceding_prep == '':
                        # In many cases the preposition (class) is
                        # given by selrestrs. For now we give those as
                        # PREP, probably need some regexes here.
                        preceding_prep = PREPOSITIONS
                # if none of the above, ignore

            # we marked the first of two NPs in a row as the iobj, but
            # the second NP could turn out to be a comp or a ccomp. So
            # now we check for the presence of an obj at the end: if
            # there is no obj, the iobj must be turned into an obj.
            if 'iobj' in ud_frame and 'obj' not in ud_frame:
                ud_frame['obj'] = ud_frame.pop('iobj')
            # add the frame unless we already have it (because many
            # frames can be identical under the simplifications that
            # we make)
            if not ud_frame in ud_frames:
                ud_frames.append(ud_frame)

    # return the frames with fewer disjunctions first, in case the end
    # user wants to e.g. choose the more specific frame first
    return sorted(ud_frames, key = lambda x : "".join(list(x.keys())).count('|'))
            

if __name__ == '__main__':
    while True:
        l = input("Give me a lemma: ")
        print([ c for cl in vn.classids(l) for c in with_superclasses(cl) ])
        
        frames = find_frames(l, verbose=True)
        for frame in frames:
            print(frame)
