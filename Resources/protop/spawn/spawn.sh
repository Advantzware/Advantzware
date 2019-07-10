#!/bin/sh
#
# spawn a bunch of test cases -- very useful for ensuring that -n, -Mn,
# -Ma, -Mpb and your kernel confguration etc can support your expected
# user count
#
# spawn.sh testCaseName numSessions [localdbName]
#

T=${1}
N=${2}

if [ -z "${N}" ]
then
	echo "usage: $0 testCaseName numSessions [localdbName]" 
	exit
fi

PF=
if [ -f "${T}.pf" ]
then
	PF="-pf ${T}.pf"
else
	echo "There is no ${T}.pf -- you must create ${T}.pf and it must include a -p procedure."
	exit
fi

FLAG=/tmp/${T}.flg

DB=

if [ ! -z ${3} ]
then
	if [ -f ${3}.db ]
	then
	 	DB="-db ${3}"
	fi
fi

date > ${FLAG}
date > ${T}.log

# ksh on AIX doesn't know how to do this :(
#
# for (( i=1; i<=N; i++ ))
# do

i=1
while [ "$i" -le "${N}" ]
do

	PROPATH=.
	export PROPATH

	echo $i

	_progres -pf spawn.pf ${DB} ${PF} -param "${FLAG}" >> ${T}.log 2>&1 &

	# if your "sleep" command supports fractions of a second this is nice to do
	#
	# sleep 0.1

        let i=i+1

done

echo ""
tail ${T}.log

echo ""

echo `ps -ef | grep ${FLAG} | grep -v grep | wc -l` '"'${T}'"' "sessions running."

echo ""
echo "To stop the test:"
echo "  rm ${FLAG}"
echo ""
