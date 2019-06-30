#!/bin/sh
#

# if you are doing repeated tests this can save some data entry ;)
#

COMMENT="edit rp.sh to pre-load comment"

# COMMENT="AIX 7.1 P8 65GB 8 CPUs at 4190Mhz"
# NCORE=8
# CPUMHZ=4190

SCENARIO="$*"

WHACK=1		# delete the db when we are done

DB=sports

PROPATH=.
RPDOTP=readprobe2017.p

# PATH=${DLC}/bin:/usr/bin:/etc:/usr/sbin:/usr/ucb:/home/oe_perft/bin:/usr/bin/X11:/sbin:.

export PROPATH DB NCORE CPUMHZ COMMENT SCENARIO

umask 0

if [ ! -f ${DB}.db ]
then
	prodb sports sports
fi

if [ ! -f rp.pf ]
then
	echo "# rp.pf" > rp.pf
fi

if [ -f ${DB}.lk ]
then
	echo "${DB} is running!"
	echo "Please shut it down prior to starting a readprobe run."
	echo ""
	exit
fi

PF=
if [ -f ${DB}.pf ]
then
	DBPF="-pf ${DB}.pf"
fi

# proserve ${DB} -spin 10000 -B 2000 -n 11000 -semsets 100 -lruskips 100 ${DBPF}

proserve ${DB} -spin 10000 -B 2000 -n 125 ${DBPF}

# BIW, APWs et al are not useful -- this is a read-only test

sleep 10

mpro ${DB} -p ${RPDOTP} -param "${COMMENT}" -T /tmp

if [ -f ${DB}.lk ]
then
	proshut -by ${DB}
fi

# this makes it much easier to test multiple Progress releases
#

if [ ${WHACK} -eq 1 ]
then
	echo y | prodel sports > /dev/null 2>&1
fi
