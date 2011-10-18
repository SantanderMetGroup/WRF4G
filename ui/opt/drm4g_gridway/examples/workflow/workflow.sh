#!/bin/sh

# We create a simple function to submit a job template ($JT)
# optionally depending on a list of job identifiers ($DEP_JIDS)
# and return the new job identifier ($JID)

gwsubmit_and_get_jid(){
# We check the number of arguments and proceed
	if [ $# -lt 1 ]; then
		echo "gwsubmit_and_get_jid: ERROR, no arguments passed in"
		exit 1
	else
		JT=$1
		shift
		if [ $# -ge 1 ]; then
			DEP_JIDS="-d $@"
		fi
	fi
# We check the existance of the job template $1
        if [ ! -e "$JT" ]; then
                echo "Job template $JT does not exist!"
                exit 2
        fi
	JID=`gwsubmit -v $DEP_JIDS -t $JT | cut -d\  -f3`
	echo $JID
        return 0
}

# We launch the workflow
printf "Submitting the jobs... "
A_ID=`gwsubmit_and_get_jid A.jt`
B_ID=`gwsubmit_and_get_jid B.jt $A_ID`
C_ID=`gwsubmit_and_get_jid C.jt $A_ID`
D_ID=`gwsubmit_and_get_jid D.jt $B_ID $C_ID`
printf "done\n"

# Waiting for the last process to finish
printf "Waiting for the last job to finish... "
gwwait $D_ID
printf "done\n"

echo "Random number `cat out.A`"
echo "Workflow should be: 2 * ( `cat out.A` + 1 )"
echo "Workflow computation: `cat out.D`"
