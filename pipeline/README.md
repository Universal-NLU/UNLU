# UNLU Pipeline

## run\_pipeline.sh manual

run\_pipeline.sh can start the pipeline both in a docker container or using the setup on the machine itself. See the [installation](#installation) section to setup this repository. This script can be called outside of the setup directory of this repository. Here is how it could be used:

    $ /path/to/pipeline/run_pipeline.sh <input_file> <rules_file> <chop_rules_file> <templates_file> <other_options>

<input_file> must be provided. Other files (<rules_file>, <chop_rules_file>, and <templates_file>) are optional. When not given as parameter, the script will try to find rules.dat chopRules.dat and templates.dat in the working directory where this script is called. If it can't find any of these files then it will not start the pipeline.

### <other_options>

| option | longer version of the parameter | used to  |
| :---   | :-| :- |
| -d | --run-in-docker |start the pipeline in a docker container. See [installation using docker](#installation-using-docker) for more information. |
| -e | --explaination | explain failing cases in stderr |
| -i | --clause-notation-i |produce output in clause notation with -i provided to UD2DRS |
| -c | --clause-notation | produce output in clause notation without -i provided to UD2DRS |
| -o | --output-for-comparison| produce output to be used in comparison. See [running the comparison script](#running-the-comparison-script-not-in-docker) for more information. |
| -g | --prove-goal | consider a subproof with the longest index covereage as the proof for its proof process |
| -l | --least-dummy | Choose the reading.proof without any DUMMY conditions if any, if not chose the reading.proof with the most number of conditions that are not DUMMY |
| -s{en,it,nl,de,fr} | --sentence | Consider the input file as a list of sentences where each line has one sentence. If this option is not provided, then the input file is considered a conll file. This option uses Stanza to get conll of a sentence. Stanza utilizes CPUs to load the model and produce conll. The language options are en, it, nl, de. The usage is -sit for Italian and -snl for Dutch etc. The default is "en" when only used as -s. |
| -r | --semantic-role-labeling | Do semantic role labeling.| 
| -f | --use-language-config | Use a language config file. The path the file could be given as the next parameter. If a language config file is not provided, then the default language config file config/languages.json will be used. If this paramter is provided, then the -Xlan parameter must also be provided. See the next option about this. If a language config file is used, then there is no need to give <rules_file>, <chop_rules_file>, and <templates_file> parameters as these files will be compiled using the language config file and the compiled files will automatically be used. See the [using a language configuration](#using-a-language-configuration) section for an example of this option. When this mode is used, there is no longer need for giving -s a language parameter such as -sen. Because the stanza config will be used instead. There is still need to use -s if the input is plain text. |
| -Xlan | | The language code as defined in the config file. Usage: -Xeng for English |

## Using a language configuration

To run with a language config run using the following scheme:

    $ /path/to/pipeline/run_pipeline.sh -f -X<lang_code> my_input_file.conllu <other_pipeline_parameters>

For example:

    $ /path/to/pipeline/run_pipeline.sh -f my_config/my_language_config.json -Xeng my_input_file.conllu -c -l -g -e
 
## Installation

There are two types of installations. The first option is that you can install the prerequisities into your computer and run. The second option is to using Docker by building a docker image which handles all the prerequisities.

### Installing directly

The prerequisities are Python 3.8 environment, python3-regex library, and Java VE 1.8 or a later version. When you set all these up you should be able to see the installed versions to the commands "java -version" and "python --version". 

Then git clone this project.
Then do:

    $ cd pipeline
    $ pip3 install -r requirements.txt
    $ cd src/ud2drs
    $ curl -sSL https://get.haskellstack.org/ | sh
    $ stack install

If you encounter any library error here, do not forget to set LIBRARY\_PATH and LD\_LIBRARY\_PATH environment variables as to point to where the missing libraries are installed.
Run the following command to add ~/.local/bin to your path

    $ export PATH=~/.local/bin/:$PATH

You should add this command in ~/.bashrc (if it is not already added) to make the path configuration permanent. 
  
Now you can use the pipeline as:

    $ /path/to/pipeline/run_pipeline.sh test.conllu rules.dat chopRules.dat templates.dat -e > out 2> log
    
You can run run\_pipeline.sh anywhere outside the pipeline directory. The first input file name (conll file) is mandatory. The rest is optional and if they are not provided, the script will search for files rules.dat chopRules.dat templates.dat in the directory run\_pipline.sh was called.

### Installation using docker

To use the docker image, please install docker daemon if not installed. (This tutorial was also tested on a shared server running podman. It works perfect with podman. The /etc/subuid and /etc/subgid must be edited to allow the user run podman (Normally system administrator does this edit). You can use the command podman instead of docker. Some systems define docker as an alias for podman.

then do:

    $ docker images
    
if you see something like the following:

    REPOSITORY   TAG       IMAGE ID       CREATED       SIZE

then it seems that the docker (or podman) daemon is running. Sometimes in some systems

    $ sudo docker
    
may be needed. After cloning this repository, tun

    $ cd pipeline/docker
    $ ./create_docker_image.sh
    
You can edit this file to set the temp directory while creating the image. Then, edit the line of  dockerCommand="docker" in run\_pipeline.sh. In some systems "sudo docker" may be needed. If you have podman configured as running when docker command is issued, you can leave this line as is. When this is done, you can test the system by running

    $ /path/to/pipeline/run_pipeline.sh test.conllu rules.dat chopRules.dat templates.dat -e -d > out 2> log

command. This command will first create a temporary container. Then it will take both input files and forward them to the container. Then in the container, haskell, java and python programs are called in order. In the end, the result is printed to stdout (and stderr) which are forwarded to the files named out and log respectively.

Basically, when -d is added to run\_pipeline.sh, the pipeline will be called from the docker container called "unlu". This script can be called from any directory and as the dockerless setup, it will search for files  rules.dat chopRules.dat templates.dat in the calling directory.

