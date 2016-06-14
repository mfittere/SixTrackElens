#! /bin/bash
halfgap=0.0015781
idcoll=13
 awk ' {if ($3=='$idcoll') SUM+=$7} END {print SUM/(NR-1)-'$halfgap'}' FirstImpacts.dat 
