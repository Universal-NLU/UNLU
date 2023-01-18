# This file includes functions to process a text file content in 
# Parallel Meaning Bank (PMB, https://pmb.let.rug.nl/drs.php) format
# and returns the DRSes within the file expressed as the 
# NLTK DRS expressions (https://www.nltk.org/howto/drt.html).
# 
# In this code and its comments, the term 'merge' means merging of
# two DRSes (b1, b2) that are expressing the PMB clause notation.
# The aim of this merge to represent the DRSes as NLTK DRSes, and NLTK
# DRSes does not support referring to one other by DRS naming such as
# b1 and b2.

import nltk
import copy

PRESUPPOSITION_MARKER_STRING_REFERENT = "999"
PRESUPPOSITION_MARKER_STRING_CONDITION = "`~"
EQUALITY_STRING_MARKER = "`"

REPLACE_LIST = {
        "+":"<plus>",
        "&":"~amp~",
        ".":"~dot~",
        "\"":"`"
        }

PRESUPPOSITION_DONT_REPLACE = {"NOT", "DIS",
        "TPR", "PRP", "ROL", "APX", "LES", "LEQ", "TAB", "SZP",
        "SZN", "SXP", "SXN", "STI", "STO", "SY1", "SY2", "SYX",
        "PRESUPPOSITION", "CONTINUATION", "CONTRAST", "NEGATION", "ATTRIBUTION",
        "POS", "NEC", "NECESSITY", "IMP", "DRS", "EQU", "NEQ", "("
        }

# A helping function to insert the same element into a list.
# Taken from https://stackoverflow.com/questions/5920643/add-an-item-between-each-item-already-in-the-list
def intersperse(lst, item):
    result = [item] * (len(lst) * 2 - 1)
    result[0::2] = lst
    return result

def DRS_pmb_clause2_nltk(s, presupposition_marker = False, remove_wordnet = False):
    global REPLACE_LIST
    global PRESUPPOSITION_MARKER_STRING_REFERENT
    global PRESUPPOSITION_MARKER_STRING_CONDITION
    global PRESUPPOSITION_DONT_REPLACE 
    global EQUALITY_STRING_MARKER

    # Create the main DRSes dictionary to keep track of them
    DRSes = dict()

    # Process the input string.
    # First split into lines
    s= s.split('\n')

    # Process each line and identify the DRS_ids
    for line in s:

        # For each line, remove the comments and skip any empty lines
        comment_index = line.find("%")
        if comment_index == -1:
            comment_index = len(line)
        line = line[0:comment_index]
        line = line.strip()
        if line == "":
            continue

        # Split the line into multiple the multiple words that 
        # represent the definition of the DRSes
        line = line.split(' ')
        
        # Process each line and remove whitepaces
        line = [ k.strip() for k in line ]
        line = [ k for k in line if k != ""]

        # Identify the DRS ids and create its entry in the main DRSes
        # dictionary
        if line[0] not in DRSes:
            DRSes[line[0]] = list()

        # Prefilter or replace anything needed
        for i in range(1,len(line)):
            for key in REPLACE_LIST.keys():
                line[i] = line[i].replace(key, REPLACE_LIST[key])

        # Add anything related to a DRS to its corresponding DRS entry
        # in the main DRSes dictionary
        DRSes[line[0]].append(line[1:])

    # Here we process two consecutive ALTERNATION(b1) ALTERNATION(b2)
    # expressions as DIS b1, b2. We do this before we start any computation,
    # so, when PMB export gets updated for this issue, we can just remove the 
    # following part of the program, or modify accordingly:
    for drs_id in DRSes:
        alternations = list()
        for drs_line_index in range(len(DRSes[drs_id])-1,-1,-1):
            if(DRSes[drs_id][drs_line_index][0] == "ALTERNATION"):
                alternations.insert(0,DRSes[drs_id][drs_line_index][1])
                del DRSes[drs_id][drs_line_index]
        for alternation_index in range(0,len(alternations),2):
            DRSes[drs_id].append(["DIS",alternations[alternation_index], alternations[alternation_index+1]])

    # Here we convert disjunctions expressed in -(-b1 & -b2) format to b1 | b2. 
    # We do this to simplify reading the DRSes.
    # Here we apply a very straightforward algorithm.
    # First we find a statement of NEGATION expression.
    # Then we follow the box that points. If the box has only two NEGATION 
    # expression, but nothing else, then we consider that these are in DIS relation.
    to_be_deleted = []
    for drs_id in DRSes:
        for drs_line_index in range(len(DRSes[drs_id])-1,-1,-1):
            if DRSes[drs_id][drs_line_index][0] == "NEGATION" or DRSes[drs_id][drs_line_index][0] == "NOT":
                # Here we find an expression of NEGATION. Now we check the box it
                # points to.
                check_box_id = DRSes[drs_id][drs_line_index][1]

                if check_box_id not in DRSes:
                    # If there is no DRS box referred as check_box_id:
                    continue

                if len(DRSes[check_box_id]) != 2:
                    # If the size of the box is not two, that means there are
                    # other statements that we cannot simplify, so:
                    continue

                if DRSes[check_box_id][0][0] != "NEGATION" and DRSes[check_box_id][0][0] != "NOT":
                    # If the first statement in the box is not a negation statement,
                    continue

                if DRSes[check_box_id][1][0] != "NEGATION" and DRSes[check_box_id][1][0] != "NOT":
                    # If the second statement in the box is not a negation 
                    # statement,
                    continue

                # So far we have everything we need. Now the DRS box referred with
                # check_box_id has to be removed and dis_first and dis_second
                # has to be added do drs_id as disjunction relationship.
                DRSes[drs_id].append(["DIS", DRSes[check_box_id][0][1], DRSes[check_box_id][1][1]])
                # Remove the reference to the intermediate box (drs)
                del DRSes[drs_id][drs_line_index]

                # Add the intermediate box (drs) to the list to be deleted 
                # when everything is over.
                to_be_deleted.append(check_box_id)

    # Remove the DRSes that are not needed anymore
    for drs_id in to_be_deleted:
        del DRSes[drs_id]



    # To keep which DRSes (boxes) merge to which ones:
    merge_graph = dict()   

    # See PRESUPPOSITION condition below for the explanation of the following:
    presupposition_graph = dict()

    # See ATTRIBUTION condition below for the explanation of the following:
    attribution_graph = dict()

    # Just to make sure that every referenced DRS exists at least as empty
    empty_referenced_drses = set()

    for drs_id in DRSes:
        referents = list()
        conditions = list()

        elements = DRSes[drs_id]

        # Process each element type and add to referents or conditions
        # When needed. Do not add anything to discard
        for element in elements:
            if element[0] == 'Proposition':
                # Handle 'Proposition' as a subDRS with the 'Proposition' condition
                # This may be updated later
                conditions.append(["Proposition","("] + intersperse(element[1:],",") + [")"])

                # Here add box references as a subDRS
                for elem in element[1:]:
                    if elem[0] == "b":
                        conditions.append(["DRS",elem])
                        if elem not in DRSes:
                            empty_referenced_drses.add(elem)

            if element[0] == 'EXPLANATION':
                # Handle 'EXPLANATION' as a subDRS with the 'EXPLANATION' condition
                # This may be updated later
                conditions.append(["EXPLANATION","(",element[1],")"])
                conditions.append(["DRS",element[1]])
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])

            elif element[0] == 'REF':

                # Add the discourse refernt to the referents list.
                referents.append(element[1])

            elif element[0] == 'PRESUPPOSITION' :

                # Handle 'PRESUPPOSITON' of two DRSes as
                # merging these. So add this information to merge_graph.
                # Add an arc from DRS B' to DRS B.

                if element[1] not in presupposition_graph:
                    presupposition_graph[element[1]] = set()
                presupposition_graph[element[1]].add( drs_id )
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])

            elif element[0] == 'CONTINUATION' :

                # Handle 'CONTINUATION' of two DRSes as
                # merging these. So add this information to merge_graph.
                # Add an arc from DRS B' to DRS B.

                if element[1] not in merge_graph:
                    merge_graph[element[1]] = set()
                merge_graph[element[1]].add( drs_id )
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])

            elif element[0] == 'CONTRAST' :

                # Handle 'CONTRAST' of two DRSes as
                # merging these. So add this information to merge_graph.
                # Add an arc from DRS B' to DRS B.

                if element[1] not in merge_graph:
                    merge_graph[element[1]] = set()
                merge_graph[element[1]].add( drs_id )
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])

            elif element[0] == 'TPR':
                pass
            elif element[0] == 'NOT':
                conditions.append(element)
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])

            elif element[0] == 'NEGATION' or element[0] == 'NEG':
                conditions.append(["NOT", element[1]])
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])

            elif element[0] == "ATTRIBUTION":

                # Handle B ATTRIBUTION B' or two DRSes as a directed graph of
                # the DRSes. The nodes are DRSes.
                # if DRS B ATTRIBUTION B'
                # add an arc from DRS B' to DRS B.
                #if element[1] not in attribution_graph:
                #    attribution_graph[element[1]] = set()
                #attribution_graph[element[1]].add( drs_id )
                #if element[1] not in DRSes:
                #    empty_referenced_drses.add(element[1])
                conditions.append(["DRS",element[1]])
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])

            elif element[0] == 'POS' or element[0] == 'POSSIBILITY' :
                # Handle 'POSSIBILITY' as a subDRS with the 'POSSIBILITY' condition
                # This may be updated later
                conditions.append(["POSSIBILITY","(",element[1],")"])
                conditions.append(["DRS",element[1]])
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])

            elif element[0] == 'NECESSITY' or element[0] == 'NEC':
                # Handle 'NECESSITY' as a subDRS with the 'NECESSITY' condition
                # This may be updated later
                conditions.append(["NECESSITY","(",element[1],")"])
                conditions.append(["DRS",element[1]])
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])

            elif element[0] == 'IMP':
                conditions.append(element)
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])
                if element[2] not in DRSes:
                    empty_referenced_drses.add(element[2])

            elif element[0] == 'DIS':
                conditions.append(element)
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])
                if element[2] not in DRSes:
                    empty_referenced_drses.add(element[2])

            elif element[0] == 'PRP':
                pass

            elif element[0] == 'ROL':
                pass

            elif element[0] == 'DRS':
                conditions.append(element)
                if element[1] not in DRSes:
                    empty_referenced_drses.add(element[1])

            elif element[0] == 'EQU':
                conditions.append(["(", element[1].replace("\"", EQUALITY_STRING_MARKER).replace("-","_"), "=" , element[2].replace("\"", EQUALITY_STRING_MARKER).replace("-","_") , ")"])
            elif element[0] == 'NEQ':
                conditions.append(["-(", element[1].replace("\"", EQUALITY_STRING_MARKER).replace("-","_"), "=" , element[2].replace("\"", EQUALITY_STRING_MARKER).replace("-","_") , ")"])
            elif element[0] == 'APX':
                pass
            elif element[0] == 'LES':
                pass
            elif element[0] == 'LEQ':
                pass
            elif element[0] == 'TPR':
                pass
            elif element[0] == 'TAB':
                pass
            elif element[0] == 'SZP':
                pass
            elif element[0] == 'SZN':
                pass
            elif element[0] == 'SXP':
                pass
            elif element[0] == 'SXN':
                pass
            elif element[0] == 'STI':
                pass
            elif element[0] == 'STO':
                pass
            elif element[0] == 'SY1':
                pass
            elif element[0] == 'SY2':
                pass
            elif element[0] == 'SYX':
                pass
            else: 

                # Process the type of DRS element 
                # that is not one of the above types:

                i = 0
                while (True):

                    # Since NLTK has constraints in naming, 
                    # the following characters are replaced in naming:
                    # . to `
                    # - to _
                    # " to   (deleted)


                    element[i] = element[i].replace("-", "_")

                    # Uncomment the following line to convert any ~ to space
                    #element[i] = element[i].replace("~"," ")

                    # The following handles references to wordnet
                    if element[i][0] == "`" and element[i].find("~dot~") != -1 and element[i][-3:-1].isnumeric():
                        if not remove_wordnet:
                            element[i -1] = element[i - 1] + element[i].replace("~dot~","`")
                        del element[i]
                        i = i - 1
                    element[i] = element[i].replace("\"", "")
#                    print(i)
#                    print(element[i])
                    # Increment the pointer to the element and exit if
                    # all elements are done
                    i = i + 1
                    if i == len(element):
                        break

                # Process the name of each relation in conditions.
                # Update any from SomeRelOf(x,y) to SomeRel(y,x).
                # Here only the first two parameters of the function is
                # substituted with each other even if there are
                # more than 2 parameters.
                if len(element[0]) > 2 and len(element) > 2:
                    if element[0][-2:] == "Of":
                        element[0] = element[0][0:-2]
                        temp = element[1]
                        element[1] = element[2]
                        element[2] = temp


                # Add the processed element to conditions. 
                # It is assumed that the elements of type REF is processed above,
                # so, the rest is considered to be on the conditions of the DRS
                elem = list()
                elem.append(element[0])
                elem.append("(")
                elem.extend(intersperse( element[1:], ","))
                elem.append(")")
                conditions.append(elem)

        # Update the DRS variables with the variables obtained from the 
        # process above
        DRSes[drs_id] = {"referents": referents, "conditions": conditions}
        
    for ref_drs_id in empty_referenced_drses:
        if ref_drs_id not in DRSes:
            DRSes[ref_drs_id] = {"referents": [], "conditions": []}
    
    # Here in presupposition handling, we acutally copy the contents of one BOX to
    # the other.
    presupposution_merged_drses = set()
    there_is_presupposition_merge = True
    while there_is_presupposition_merge:
        there_is_presupposition_merge = False
        for drs1 in presupposition_graph:
            for drs2 in presupposition_graph[drs1]:
                if drs1==drs2:
                    continue
                if presupposition_marker:
                    DRSes[drs1]["referents"]=set(DRSes[drs1]["referents"])
                    DRSes[drs1]["referents"]=DRSes[drs1]["referents"].difference(set(DRSes[drs2]["referents"]))
                    DRSes[drs1]["referents"]=list(DRSes[drs1]["referents"])
                    DRSes[drs1]["referents"].extend([ i + PRESUPPOSITION_MARKER_STRING_REFERENT if i.find(PRESUPPOSITION_MARKER_STRING_REFERENT)==-1 else i for i in DRSes[drs2]["referents"]])
                    marker_will_be_added = DRSes[drs2]["conditions"]
                    for ind in range(len(marker_will_be_added)):
                        con_name = marker_will_be_added[ind]
                        if marker_will_be_added[ind][0][0:2] != PRESUPPOSITION_MARKER_STRING_CONDITION and con_name[0] not in PRESUPPOSITION_DONT_REPLACE:
                            marker_will_be_added[ind][0] = PRESUPPOSITION_MARKER_STRING_CONDITION+marker_will_be_added[ind][0]
                    DRSes[drs1]["conditions"].extend(marker_will_be_added)
                else:
                    DRSes[drs1]["referents"].extend(DRSes[drs2]["referents"])
                    DRSes[drs1]["referents"] = list(set(DRSes[drs1]["referents"]))
                    DRSes[drs1]["conditions"].extend(DRSes[drs2]["conditions"])
                presupposution_merged_drses.add((drs1,drs2))
                attribution_graph = update_graph(drs1, drs2, attribution_graph) 
                merge_graph = update_graph(drs1,drs2, merge_graph)
                presupposition_graph = update_graph(drs1, drs2, presupposition_graph)
                there_is_presupposition_merge = True
                break
            if there_is_presupposition_merge:
                break
    for (drs1,drs2) in presupposution_merged_drses:
        DRSes = update_drs_references(DRSes, drs1, drs2)
        if drs2 in DRSes:
            del DRSes[drs2]

    merge =True
    while merge :
        merge = False
        for drs1 in merge_graph:
            for drs2 in merge_graph[drs1]:
                if drs1==drs2:
                    continue
                (status, DRSes, merge_graph, attribution_graph) = do_merge(drs1, drs2, DRSes, merge_graph, attribution_graph)
                if status:
                    merge = True
                    break
            if merge:
                break

    # ATTRIBUTION processing
    DRSes = attribution_graph_handling(attribution_graph, DRSes)

    # NOT processing
    DRSes = not_handling(DRSes, DRSes)

    # POS processing
    DRSes = pos_handling(DRSes, DRSes)

    # NEC processing
    DRSes = nec_handling(DRSes, DRSes)

    # IMP processing
    DRSes = implication_handling(DRSes, DRSes)

    # DRS processing
    DRSes = sub_drs_handling (DRSes, DRSes)

    # DIS processing
    DRSes = disjunction_handling (DRSes, DRSes)

    # Create NLTK representations
    DRSes = createNLTKReps(DRSes)

    # Return DRSes
    return DRSes

def update_graph(drs1, drs2, graph):
    # This function updates the graph given as parameter according
    # to drs1 and drs2 indentifiers (B' and B respectively)
    # The operations are replacing drs2 with drs1,
    # deleting any self references after the replace,
    # deleting any empty references after all these operations (such as b1-> nothing)

    my_graph = copy.deepcopy(graph)

    for d1 in my_graph:
        nodes = set()
        for i in my_graph[d1]:
            if i == drs2:
                nodes.add(drs1)
            else:
                nodes.add(i)
        my_graph[d1] = nodes

    if drs2 in my_graph:
        obj = my_graph[drs2]
        if drs1 not in my_graph:
            my_graph[drs1] = set()
        for o in obj:
            my_graph[drs1].add(o)
        del my_graph[drs2]

    for elm1 in my_graph:
        removed = True
        while removed:
            removed = False
            for elm2 in my_graph[elm1]:
                if elm1 == elm2:
                    my_graph[elm1].remove(elm2)
                    removed = True
                    break
    removed = True
    while removed:
        removed = False
        for elm1 in my_graph:
            if len(my_graph[elm1]) ==0:
                del my_graph[elm1]
                removed = True
                break
    return my_graph

def update_drs_references(my_DRSes, drs1, drs2):
    # This function updates all drs2 references as drs1 in all DRSes and returns
    # the drses.
    # Update to all references to B in the DRSes themselves 
    
    for drs_i in my_DRSes:
        for condition_index in range(len(my_DRSes[drs_i]["conditions"])):
            # Is any filtering of condition type needed?
            # If so, do it here. Currently no filter is
            # applied. So for example, whenever B' is
            # observed as string somewhere in a DRS 
            # condition it is replaced with the reference 
            # to B
            my_DRSes[drs_i]["conditions"][condition_index] = [drs1 if i==drs2 else i for i in my_DRSes[drs_i]["conditions"][condition_index]  ]
#            if my_DRSes[drs_i]["conditions"][condition_index] == drs2:
#                my_DRSes[drs_i]["conditions"][condition_index] = drs1
    return my_DRSes



def do_merge(drs1, drs2, DRSes, merge_graph, attribution_graph):
    # This function merges two DRSes that are in merging relationsip.
    # After the merge, it updates anything related to merging such as the
    # DRSes and any other data structre.

    # The graph of DRSes provides the information of which DRS 
    # should be joined into which DRS. In our computation, if an edge
    # is (B, B') then, the elements in B' are joined into the
    # elements of B. This is how it is shown when the 'show pointers' 
    # option is selected in a PMB discourse view (such as: 
    # https://pmb.let.rug.nl/explorer/explore.php?part=86&doc_id=0712&type=drs.xml&searchMode=direct)
    # So, below we do this join operation. When the operation is
    # over, B' must be deleted, and all references to 
    # it must be updated as B.
    if drs1==drs2:
        return (False, DRSes, merge_graph, attribution_graph)

    my_DRSes = copy.deepcopy(DRSes)
    my_merge_graph = copy.deepcopy(merge_graph)
    my_attribution_graph = copy.deepcopy(attribution_graph)

    my_DRSes[drs1]["referents"].extend(my_DRSes[drs2]["referents"])
    my_DRSes[drs1]["referents"] = list(set(my_DRSes[drs1]["referents"]))
    my_DRSes[drs1]["conditions"].extend(my_DRSes[drs2]["conditions"])

    del my_DRSes[drs2]

    # TODO: The following will not work as a set of list is not possible
    # So, later, a mechanism may be needed to check that 
    # all conditions are unique in a DRS after a merge.
    # 
    # DRSes[drs1]["conditions"] = list(set(DRSes[drs1]["conditions"]))


    # update presupposition graph
    my_merge_graph = update_graph(drs1, drs2, my_merge_graph)

    # update attribution graph
    my_attribution_graph = update_graph(drs1, drs2, my_attribution_graph)

    # Update to all references to B in the DRSes themselves 
    my_DRSes = update_drs_references(my_DRSes, drs1, drs2)

    return (True, my_DRSes, my_merge_graph, my_attribution_graph)


def getNLTKRep(DRS):
    # This function recursively goes over all elements of a DRS
    # and produce NLTK representation strings for them
    global PRESUPPOSITION_MARKER_STRING_REFERENT
    global PRESUPPOSITION_MARKER_STRING_CONDITION

    my_drs = copy.deepcopy(DRS)
    for condition_index in range(len(my_drs["conditions"])):
        if my_drs["conditions"][condition_index][0] == "NOT":
            my_drs["conditions"][condition_index] = "-(" + getNLTKRep(my_drs["conditions"][condition_index][1]) + ")"
        elif my_drs["conditions"][condition_index][0] == "ATTRIBUTION":
            my_drs["conditions"][condition_index] = [my_drs["conditions"][condition_index][1], ":" ,  "(" + getNLTKRep(my_drs["conditions"][condition_index][2]) , ")" ]
        elif my_drs["conditions"][condition_index][0] == "IMP":
            my_drs["conditions"][condition_index] = "(" + getNLTKRep(my_drs["conditions"][condition_index][1]) + ") -> (" + getNLTKRep(my_drs["conditions"][condition_index][2]) + ")"
        elif my_drs["conditions"][condition_index][0] == "DRS":
            my_drs["conditions"][condition_index] = [my_drs["conditions"][condition_index][1], ":" ,  "(" + getNLTKRep(my_drs["conditions"][condition_index][2]) , ")" ]
        elif my_drs["conditions"][condition_index][0] == "POS":
            my_drs["conditions"][condition_index] = "POS(" + getNLTKRep(my_drs["conditions"][condition_index][1]) + ")"
        elif my_drs["conditions"][condition_index][0] == "NEC":
            my_drs["conditions"][condition_index] = "NEC(" + getNLTKRep(my_drs["conditions"][condition_index][1]) + ")"
        elif my_drs["conditions"][condition_index][0] == "DIS":
             my_drs["conditions"][condition_index] = "(" + getNLTKRep(my_drs["conditions"][condition_index][1]) + ") | (" + getNLTKRep(my_drs["conditions"][condition_index][2]) + ")"

        my_drs["conditions"][condition_index] = "".join(my_drs["conditions"][condition_index])
        # Here post-process condition to remove unneccessary markers from equality
        # expressions. When this is not done, if presupposition marker is picked
        # the NLTK DRS cannot be formed.
        if(my_drs["conditions"][condition_index].find("=")!=-1):
            my_drs["conditions"][condition_index] = my_drs["conditions"][condition_index].replace(PRESUPPOSITION_MARKER_STRING_CONDITION,"")

    # Here we make sure that referents and conditions are unique. 
    my_drs["referents"] = remove_duplicate(my_drs["referents"], PRESUPPOSITION_MARKER_STRING_REFERENT)
    my_drs["conditions"] = remove_duplicate(my_drs["conditions"], PRESUPPOSITION_MARKER_STRING_CONDITION)

    drs_string= "([" + ",".join(my_drs["referents"]) + "][" + ",".join(my_drs["conditions"])  + "])"
    return drs_string

def remove_duplicate(lst, marker):
    # This function removes duplicate elements in the list lst
    # It considers markers as non-existing while comparing the elements
    # with each other.

    my_set = dict((el,0) for el in lst) # We do not want to loose the order of
                                        # the elements. So 'dict' is used.
    my_set_copy = copy.deepcopy(my_set)
    for item in my_set_copy:
        item_before_replace = item
        item_after_replace = item.replace(marker, "")
        if item_before_replace!=item_after_replace:
            if item_after_replace in my_set:
                del my_set[item_after_replace]
    return list(my_set)

def createNLTKReps(DRSes):
    # This function goes over all the DRSes and creates NLTK objects using
    # the representative strngs
    my_drses = copy.deepcopy(DRSes)
    
    for drs_id in my_drses:
        my_drses[drs_id] = nltk.sem.drt.DrtExpression.fromstring(r"{}".format(getNLTKRep(my_drses[drs_id])))
    return my_drses

def recursive_handle_attribution(DRS, referenceDRSes, visited_DRSes, parent_drs_referents ):
    # This function recursively handles the ATTRIBUTION relationship. 
    # The reason that this function is separated from the recursive_handle 
    # function for ATTRIBUTION is to do special operations for the 
    # ATTRIBUTION relationship. Namely, the referents in the DRS that is
    # put as an argument to a condition must be checked against the upper
    # DRS. The common referents are excluded from the referents of 
    # this DRS. 

    my_drs = copy.deepcopy(DRS)
    typ = "ATTRIBUTION"
    all_referents =set()
    all_referents.update(my_drs["referents"])
    all_referents.update(parent_drs_referents)
    my_drs["referents"] = set(my_drs["referents"])
    my_drs["referents"] = my_drs["referents"].difference(parent_drs_referents)
    my_drs["referents"] = list (my_drs["referents"])

    #add_as_ref = dict()

    for condition_index in range(len(my_drs["conditions"])):

        if my_drs["conditions"][condition_index][0] == typ:
            visited_DRSes.add(my_drs["conditions"][condition_index][1])
            my_drs["conditions"][condition_index][2] = recursive_handle_attribution(my_drs["conditions"][condition_index][2], referenceDRSes, visited_DRSes, all_referents )
        else:
            for i in range(len(my_drs["conditions"][condition_index] )):
                if type(my_drs["conditions"][condition_index][i])!= str:
                    my_drs["conditions"][condition_index][i] = recursive_handle_attribution (my_drs["conditions"][condition_index][i], referenceDRSes, visited_DRSes, all_referents)

    return my_drs

def recursive_handle(typ, DRS, referenceDRSes, visited_DRSes ):
    # This function recursively handles some specific unary relations
    # such as NOT. 
    # This function puts addressed DRS content to where it is referred.
    # Later, these updates will be used to construct NLTK representative 
    # strings that refer to other DRSes in a sub DRS manner

    my_drs = copy.deepcopy(DRS)

    for condition_index in range(len(my_drs["conditions"])):
        if my_drs["conditions"][condition_index][0] == typ:
            reference = my_drs["conditions"][condition_index][1]
            for index in range(1,len(my_drs["conditions"][condition_index])):
                if my_drs["conditions"][condition_index][index] == reference:
                    visited_DRSes.add(my_drs["conditions"][condition_index][index])
                    my_drs["conditions"][condition_index][index] = recursive_handle(typ, referenceDRSes[my_drs["conditions"][condition_index][index]], referenceDRSes, visited_DRSes)
        else:
            for i in range(len(my_drs["conditions"][condition_index] )):
                if type(my_drs["conditions"][condition_index][i])!= str:
                    my_drs["conditions"][condition_index][i] = recursive_handle(typ, my_drs["conditions"][condition_index][i], referenceDRSes, visited_DRSes)
        
    return my_drs

def binary_recursive_handle(typ, DRS, referenceDRSes, visited_DRSes ):
    # This function recursively handles specific binary relations
    # such as IMP and DIS
    # This function puts addressed DRS content to where it is referred
    # Later, these updates be used to construct NLTK representative strings that
    # refer to other DRSes in a sub DRS manner

    my_drs = copy.deepcopy(DRS)
    for condition_index in range(len(my_drs["conditions"])):
        if my_drs["conditions"][condition_index][0] == typ:
            visited_DRSes.add(my_drs["conditions"][condition_index][1])
            visited_DRSes.add(my_drs["conditions"][condition_index][2])
            my_drs["conditions"][condition_index][1] = binary_recursive_handle(typ, referenceDRSes[my_drs["conditions"][condition_index][1]], referenceDRSes, visited_DRSes)
            my_drs["conditions"][condition_index][2] = binary_recursive_handle(typ, referenceDRSes[my_drs["conditions"][condition_index][2]], referenceDRSes, visited_DRSes)
        else:
            for i in range(len(my_drs["conditions"][condition_index] )):
                if type(my_drs["conditions"][condition_index][i])!= str:
                    my_drs["conditions"][condition_index][i] = binary_recursive_handle(typ, my_drs["conditions"][condition_index][i], referenceDRSes, visited_DRSes)

    return my_drs 

def unary_operation_handling(operation, DRSes, referenceDRSes):
    # This function starts processing of constructs of type ´operation´
    # In the end, the visited DRSes are deleted as their content are directly 
    # utilized in the referring DRS
    my_DRSes = copy.deepcopy(DRSes)
    visited_all_DRSes = {}
    for drs_id in my_DRSes:
        visited_DRSes = set()
        my_DRSes[drs_id] = recursive_handle(operation, my_DRSes[drs_id], referenceDRSes, visited_DRSes)
        visited_all_DRSes[drs_id] = visited_DRSes

    for drs_id1 in visited_all_DRSes:
        for drs_id2 in visited_all_DRSes[drs_id1]:
            my_DRSes=update_drs_references(my_DRSes, drs_id1, drs_id2)
            if drs_id2 in my_DRSes:
                del my_DRSes[drs_id2]
    return my_DRSes

def binary_operation_handling(operation, DRSes, referenceDRSes):
    # This function starts processing of constructs of type ´operation´
    # In the end, the visited DRSes are deleted as their content are directly 
    # utilized in the referring DRS
    my_DRSes = copy.deepcopy(DRSes)
    visited_DRSes = set()
    for drs_id in my_DRSes:
        my_DRSes[drs_id] = binary_recursive_handle(operation, my_DRSes[drs_id], referenceDRSes, visited_DRSes)
    for drs_id in visited_DRSes:
        if drs_id in my_DRSes:
            del my_DRSes[drs_id]
    return my_DRSes

def not_handling(DRSes, referenceDRSes):
    # This function starts processing of constructs of type NOT
    return unary_operation_handling("NOT", DRSes, referenceDRSes)

def sub_drs_handling(DRSes, referenceDRSes):
    # This function starts processing of constructs of type DRS
    visited_drses = set()
    DRSes = copy.deepcopy(DRSes)
    for drs_id in DRSes:
        for condition_id in range(len(DRSes[drs_id]["conditions"])):
            if DRSes[drs_id]["conditions"][condition_id][0] == "DRS":
                DRSes[drs_id]["conditions"][condition_id].append(DRSes[referenceDRSes[drs_id]["conditions"][condition_id][1]])
                visited_drses.add((drs_id,DRSes[drs_id]["conditions"][condition_id][1]))

    for (drs1,drs2) in visited_drses:
        if drs2 in DRSes:
            del DRSes[drs2]

    return DRSes

def nec_handling(DRSes, referenceDRSes):
    # This function starts processing of constructs of type NEC
    return unary_operation_handling("NEC", DRSes, referenceDRSes)

def pos_handling(DRSes, referenceDRSes):
    # This function starts processing of constructs of type POS
    return unary_operation_handling("POS", DRSes, referenceDRSes)

def implication_handling(DRSes, referenceDRSes):
    # This function starts processing of constructs of type IMP
    return binary_operation_handling("IMP", DRSes, referenceDRSes)

def disjunction_handling(DRSes, referenceDRSes):
    # This function starts processing of constructs of type DIS
    return binary_operation_handling("DIS", DRSes, referenceDRSes)

def attribution_graph_handling(graph, DRSes):
    # This function starts processing of constructs of type ATTRIBUTION
    # In the end, the visited DRSes are deleted as their content are directly 
    # utilized in the referring DRS
    # This function utilizes the initial attribution_graph to process ATTRIBUTION
    # This graph keeps which DRS is in attribution relationship witch which DRS.
    # If this graph was not used, all the elements in conditions must be checked
    # if they refer to another DRS. This is not a proper way of doing this computation.
    # The references may not always mean that they refer to a DRS. We can understand
    # this only if it keeps track of ATTRIBUTION relationships.
    my_DRSes = copy.deepcopy(DRSes)
    my_graph = copy.deepcopy(graph)

    added_attributions = set()

    # First go over all DRSes and add ATTRIBUTION mark when neccessary
    for referred_drs in my_graph:
        for referring_drs in my_graph[referred_drs]:
            for condition_index in range(len(my_DRSes[referring_drs]["conditions"])):

                if my_DRSes[referring_drs]["conditions"][condition_index][0] == "ATTRIBUTION":
                    continue
                condition_set = set(my_DRSes[referring_drs]["conditions"][condition_index])
                if referred_drs in condition_set:
                    marker = referring_drs + "," + referred_drs
                    if marker not in added_attributions:
                        my_DRSes[referring_drs]["conditions"].insert(0,["ATTRIBUTION", referred_drs, my_DRSes[referred_drs]])
                        added_attributions.add(marker)
                    
    visited_DRSes = set()
        
    referenceDRSes = copy.deepcopy(my_DRSes)

    for drs_id in my_DRSes:
        my_DRSes[drs_id] = recursive_handle_attribution(my_DRSes[drs_id], referenceDRSes, visited_DRSes, set())
    for drs_id in visited_DRSes:
        if drs_id in my_DRSes:
            del my_DRSes[drs_id]
    return my_DRSes

# Taken from https://stackoverflow.com/questions/5920643/add-an-item-between-each-item-already-in-the-list to add an element between each element in a list
def intersperse(lst, item):
    result = [item] * (len(lst) * 2 - 1)
    result[0::2] = lst
    return result
