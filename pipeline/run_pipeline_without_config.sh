#!/bin/bash
docker_command="docker"
docker_image_name="unlu"
defaultfile_2="rules.dat"
defaultfile_3="chopRules.dat"
defaultfile_4="templates.dat"
run_in_docker="False"

# Determine execution directory
# CALLING_DIR=$(pwd)

# Determine script path. 
# Taken from https://stackoverflow.com/questions/630372/determine-the-path-of-the-executing-bash-script
SCRIPT_PATH=$(dirname "$0")
SCRIPT_PATH=$(cd "$SCRIPT_PATH" && pwd)
if [[ -z "$SCRIPT_PATH" ]] ; then
    echo "Cannot access the script path $SCRIPT_PATH. The pipeline could not be started."
    exit 1 
fi

# Here the idea is if a parameter starts with - it means that it is not a file. 
# This way, we keep which ones are files and separate the rest
rest_of_parameters=""
parameter_file1=""
parameter_file2=""
parameter_file3=""
parameter_file4=""
files=()
for var in "$@"
do
    if [[ "$var" == "--run-in-docker" || "$var" == "-d" ]]; then
	    run_in_docker="True"
    elif [[ "$var" =~ ^-.*  ]]; then
	    	rest_of_parameters="$rest_of_parameters $var"
   	else
		files+=($var)
    fi
done

file1=""
file2=""
file3=""
file4=""

# Check if the first parameter file exists
if test ! -f "${files[0]}"
then
	echo "Cannot find file $1. The pipeline could not be started."
        exit 0
fi
file1=${files[0]}

# Check if the second paramter is provided
if test "${files[1]}" ==  ""
then
	if test ! -f "$defaultfile_2"
	then
		echo "The second parameter (rules file) is not provided. Also, the default file \"$defaultfile_2\" could not be found in the working directory. The pipeline could not be started."
	        exit 0
	else
		file2=$defaultfile_2
	fi
else
	file2=${files[1]}
fi

# Check if the third paramter is provided
if test "${files[2]}" ==  ""
then
        if test ! -f "$defaultfile_3"
        then
                echo "The third parameter (chopping-rules file) is not provided. Also, the default file \"$defaultfile_3\" could not be found in the working directory. The pipeline could not be started."
                exit 0
	else
		file3=$defaultfile_3
        fi
else
	file3=${files[2]}
fi

# Check if the forth paramter is provided
if test "${files[3]}" ==  ""
then
        if test ! -f "$defaultfile_4"
        then
                echo "The forth parameter (templates file) is not provided. Also, the default file \"$defaultfile_4\" could not be found in the working directory. The pipeline could not be started."
                exit 0
	else
		file4=$defaultfile_4
        fi
else
	file4=${files[3]}
fi

# Check if the second parameter file exists
if test ! -f "$file2"
then
       	echo "Cannot find file $file2. The pipeline could not be started."
	exit 0
fi

# Check if the second parameter file exists
if test ! -f "$file3"
then
       	echo "Cannot find file $file3. The pipeline could not be started."
        exit 0
fi

# Check if the second parameter file exists
if test ! -f "$file4"
then
       	echo "Cannot find file $file4. The pipeline could not be started."
        exit 0
fi

# If run_in_docker is set then forward the neccessary files given as input to the container.
if [[ "$run_in_docker" == "True"  ]]; then
	f1="$(cat $file1 | sed 's/\x5C\x5C/\[BCKSLSH\]/g' | sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/\[NEWLINE\]/g' )"
	f2="$(cat $file2 | sed 's/\x5C\x5C/\[BCKSLSH\]/g' | sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/\[NEWLINE\]/g' )"
	f3="$(cat $file3 | sed 's/\x5C\x5C/\[BCKSLSH\]/g' | sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/\[NEWLINE\]/g' )"
	f4="$(cat $file4 | sed 's/\x5C\x5C/\[BCKSLSH\]/g' | sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/\[NEWLINE\]/g' )"
echo -e "$f1\n---------------------------------------------------------------------------------------\n$f2\n---------------------------------------------------------------------------------------\n$f3\n---------------------------------------------------------------------------------------\n$f4" | $docker_command run --rm -i $docker_image_name /unlu/docker/container_pipeline_starter.sh "$rest_of_parameters"

# If not start the pipeline here as usual
else
	python3 $SCRIPT_PATH/src/python-lib/run_pipeline.py "$file1" "$file2" "$file3" "$file4" $rest_of_parameters
fi
