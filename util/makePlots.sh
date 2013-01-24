#! /bin/sh
for f in "English" "French" "German" "Mystery" "Swahili"
do
	./plot.gnuplot $f
done
