#! /usr/bin/env python

import os
import sys
sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)) + '/../DRT-Python/nltk')
from nltk import sem


with open(sys.argv[1]) as rules:
    for rule in rules:
        rule = rule.strip()
        if rule != "" and not rule.startswith("#"):
            criterion, rest = [x.strip() for x in rule.split("->")]
            if rest == "STOP" or rest == "":
                pass
            else:
                elements = rest.split(" : ")
                lambda_term, typus = elements[0:2]
                if len(elements) == 3:
                    local_name = elements[2]
                print(criterion)
                print(sem.DrtExpression.fromstring(lambda_term.replace(':', '')).pretty_print())
