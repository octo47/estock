#! /usr/bin/gnuplot -persist
set terminal png small
set out "/var/tmp/bff.png"
set encoding koi8r
set xlabel "X" font "Helvetica,18"
set ylabel "Noise Amplitude" font "Helvetica,18"
set style line 1 lt 1 lw 1 pt 0
set style line 2 lt 2 lw 1 pt 0
set style line 3 lt 3 lw 1 pt 0
set style line 4 lt 4 lw 1 pt 0
plot 	"./apps/estockd/.eunit/10_vol.txt" using 2 with linespoints linestyle 1, \
	"./apps/estockd/.eunit/07_vol.txt" using 2 with linespoints linestyle 2, \
	"./apps/estockd/.eunit/03_vol.txt" using 2 with linespoints linestyle 3, \
	"./apps/estockd/.eunit/01_vol.txt" using 2 with linespoints linestyle 4

