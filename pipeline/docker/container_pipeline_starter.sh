#!/bin/bash
# This program runs in the container created when the pipeline is invovoked
# in the container mode. Docker creates a container using the the image, 
# and this script gets invoked when this happens. 
# This program recieves the files' content from stdin and saves them. 
# It saves them as files in the container and starts the pipleline within the container.

fcounter=1
fstr=""
files=""
while IFS='$\n' read -r line; 
do
	if test "$line" == "---------------------------------------------------------------------------------------"
	then
		echo -e "$fstr" | sed 's/\[NEWLINE\]/\x0A/g' | sed 's/\[BCKSLSH\]/\x5C/g' > f_$fcounter.file
		fstr=""
		files="$files f_$fcounter.file "
		fcounter=$((fcounter+1))
	else
		fstr="$fstr$line"
	fi
done
echo -e "$fstr" | sed 's/\[NEWLINE\]/\x0A/g' | sed 's/\[BCKSLSH\]/\x5C/g' > f_$fcounter.file
files="$files f_$fcounter.file "
fcounter=$((fcounter+1))
./run_pipeline.sh $files $@
