#!/bin/bash
# This is a wrapper script for the pipeline that processes parameters that checks if a language config file will be used

# Determine execution directory
# CALLING_DIR=$(pwd)

# Determine script path. 
# Taken from https://stackoverflow.com/questions/630372/determine-the-path-of-the-executing-bash-script
SCRIPT_PATH=$(dirname "$0")
SCRIPT_PATH=$(cd "$SCRIPT_PATH" && pwd)
if [[ -z "$SCRIPT_PATH" ]] ; then
    echo "Cannot access the script path $SCRIPT_PATH. The pipeline could not be started." 1>&2
    exit 1 
fi

default_config="$SCRIPT_PATH/config/languages.json"
file2="$SCRIPT_PATH/tmp/file2"
file3="$SCRIPT_PATH/tmp/file3"
file4="$SCRIPT_PATH/tmp/file4"

rest_of_parameters=""
files=()
file_str=""
use_config="False"
language=""
sentence_mode="False"
sentence_mode_value=""
for var in "$@"
do
    if [[ "$var" == "--use-language-config" || "$var" == "-f" ]] ; then
        use_config="True"
    elif  [[ "$var" =~ ^-X.*  ]] ; then
        language=$var
    elif [[ "$var" =~ ^-s.* ]]; then
        sentence_mode="True"
	sentence_mode_value="$var"
#    elif [[ "$var" == "--language-iso-code" || "$var" == "-x" ]]; then
#        language="$var"
    elif [[ "$var" =~ ^-.*  ]] ; then
        rest_of_parameters="$rest_of_parameters $var"
    else
        files+=($var)
        file_str="$file_str $var"
    fi
done

if test "$use_config" ==  "True" ; then
    
    config_file="$default_config"

    # Check if the first file is provided
    if ! test "${files[0]}" ==  "" ; then

	# Check if the file exists
        if test -f "${files[0]}" ; then

	    # Check if the file is a config file
            if grep -q "\"prefix\":" "${files[0]}"; then
               config_file="${files[0]}"
	       files=( "${files[@]:1}" )
            else
               echo "A config file is not specified or could not be found. Using the default config file : $default_config" 1>&2
	    fi
	else
	    echo "A config file is not specified or could not be found. Using the default config file : $default_config" 1>&2
        fi
    fi

    if test "$language" == "" ; then
        echo "Please specify the language to be used in the config file." 1>&2
	exit 1
    fi

    echo "Config file: $config_file" 1>&2
    echo "Language: $language" 1>&2
    echo "Input file: ${files[0]}" 1>&2
    echo "Parameters: $rest_of_parameters" 1>&2
    if test "$setence_mode" == "True"; then
	to_add="-sen"
    else
	to_add=""
    fi
    output=$(python3 $SCRIPT_PATH/src/python-lib/lib/process_language_code.py "$config_file" "$language" "$file2" "$file3" "$file4")
    if test "$?" == "0"; then
        $SCRIPT_PATH/run_pipeline_without_config.sh "${files[0]}" "$file2" "$file3" "$file4" $rest_of_parameters $output $to_add
	exit 0
    else
       echo "There has been a problem processing the language file and parameter. The pipeline will not start." 1>&2
       exit 1
    fi
else
    $SCRIPT_PATH/run_pipeline_without_config.sh $file_str $rest_of_parameters $sentence_mode_value
fi
