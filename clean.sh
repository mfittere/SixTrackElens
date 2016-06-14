#!/bin/bash
colldb_file='allelemLTCPlwbP1data.dat'
list="README fort.2 fort.3 clean.sh Makefile ${colldb_file} calc_firstimp.sh beep.sh blp.sh BeamLossPattern  SurveyWithCrossing_XP_lowb_b1.dat  allapert.b1"


for i in `ls`;do
	saved=0
	for j in $list; do
		if [ $i == $j ];then
			saved=1
			echo $i saved
		fi
	done
	if [ -d $i ]; then
		saved=1
		echo $i saved
	fi
	if [ $saved = 0 ]; then
		rm $i
	fi
done
