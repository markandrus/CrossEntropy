#! /bin/sh
gnuplot << EOF
	set title "$1"

	set terminal png
	set output "../graphs/$1.png"

	set datafile separator ','

	set multiplot layout 2,1 title "$1"

	unset key
	set ylabel "Cross Entropy"
	unset title
	plot "../output/$1.cross-entropies.csv" u 2:xtic(1) with histeps notitle

	set ylabel "Log Frequency"
	plot "../output/$1.freqs.csv" u 3:xtic(1) notitle
EOF
