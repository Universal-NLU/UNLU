from srl.srl import *

# TODO: mutually exclusive group
parser = argparse.ArgumentParser(description="Add verbnet thematic roles")
parser.add_argument('input', help="Input file as produced by the pipeline with -o option")
parser.add_argument('-d', '--debug', action='store_true', help="Produce human-readable DRSs")
parser.add_argument('-c', '--clause-notation', action='store_true', help="Produce clause notation output")

# Default is to produce output in NLTK serialization
args = parser.parse_args()

if args.clause_notation:
    sys.path.append('./lib')
    from NLTK_drs_to_PMB_drs import *

with open(args.input) as f:
    
    for line in f:
        if (not line.startswith('%') and not line.isspace()):
            data = line.strip().split("\t")
            if len(data) == 2:
                name, reading = line.strip().split("\t")
                id = 1
            elif len(data) == 3:
                (name, id, reading) = line.strip().split("\t")
            else:
                raise BaseException(f"Unknown input data format {data}")
            drs = dexpr(reading).simplify()
            new_drss = srl(drs)

            if args.debug:
                print()
                print(f"OLD DRS: {name} {id}")
                print("*********")
                drs.pretty_print()

                print()
                print("NEW DRS")
                print("*********")

                for i, n in enumerate(new_drss):
                    print(f"Roles {i}")
                    n.pretty_print()

            elif args.clause_notation:
                for i, n in enumerate(new_drss):
                    print(f"% Roles {i}")
                    print(nltk_drs_to_pmb_drs(n))
            else:
                for i, n in enumerate(new_drss):
                    print(f"{name}\t{id}.{i}\t{str(n)}")
