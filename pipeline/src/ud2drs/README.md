About
=====

This repository contains code that converts UD graphs to glue semantics meaning constructors that can be combined to form DRSs. To install the package you need stack <https://docs.haskellstack.org/en/stable/install_and_upgrade/>. Running `stack install' will build the binaries and copy the executable to ~/.local/bin

ud2drs needs a set of rules to guide the rewriting. See below for some documentation of the rule format. Sample rules are found in <https://github.uio.no/Universal-NLU/glue-for-UD>.

The executable should be invoked with the path to the rule file as a command line argument. It will read conllu from stdin.

`ud2drs heads.dat < test.conllu'

The output will be written in a compact format to stdout and a more human-readable format with debugging information to stderr. 

As a default, the program will divide proof premises into subproofs. The rules for this is for now hard-coded in the program. If given the option `--nochop', it will produce a single, flat list of meaning constructors.


Rule format
===========

This documentation is very preliminary. It explains how the rules of the conversion should be specified. The parsing of these rules is currently very brittle and if there are small errors in the rule, weird errors can occur. Blank lines and lines starting with # are ignored. Otherlines must be of the form "criterion -> rule".

Criteria
========

A criterion is a comma-separated list of constraints that a token must satisfy in order to contribute a meaning as specified by the rule. Notice that dependency relations are treated as features of dependents. Each constraint consists of an attribute and a value separated by '='. CoNLL-U features are the value of the features attribute and specified as in CoNLL-U (i.e. '|'-separated), but with '-' instead of '=' for parsing convenience.

A single token can match several criteria and thus generate several meaning constructors. For example, a verb might match both "relation = root" and "coarsePos = VERB". However, if a token matches an empty rule, the search for more meaning constructors is aborted. If a token matches a criterion with the rule STOP, the whole subtree under the token will be ignored.


Head rules
==========

A head rule consists of two obligatory and one optional field, separated with ':'. The first is a meaning, the second a linear logic type and the third (optional) a local name definition.

Meanings can be given in any format you wish. The special symbol :LEMMA: will be replaced by the token's lemma.

The linear logic type is given using functional uncertainties. Any functional uncertainty must be enclosed in parentheses directly preceded by a letter [a-zA-Z] (a propositional function - this is intended to represent the corresponding type on the meaning side). The valid metavariables over nodes are ^ (mother node) and ! (this node). Since dependency relations are features of the dependent, ^ and ! will correspond to the head and dependent respectively. Functional uncertainties should start with ^ or ! and continue with relation labels, interpreted as steps down the tree. so e(! nsubj) designated the "nsubj" dependent of the current node. "dep" matches any dependent. Kleene star is allowed: nsubj* means an arbitrary number of nsubj edges down, ccomp* an arbitrary number of ccomp edges down, and dep* an arbitrary number of edges down. Features can be specified inside curly brackets, so dep{PronType=Rel} matches any dependent as long as it has the PronType=Rel feature.

A local name is a variable in the linear logic type representing a functional uncertainty which will be instantiated to the same node in each reading of the sentence. There is a predefined local name %R which will always match the top node. This is useful for projective semantics, e.g. proper names.
