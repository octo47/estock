#! /usr/bin/gnuplot -persist
set terminal png small
set out "/var/tmp/bff.png"
set encoding koi8r
set xlabel "X" font "Helvetica,18"
set ylabel "Noise Amplitude" font "Helvetica,18"
set style line 1 lt 1 pt 0
plot "./apps/estockd/.eunit/bff.txt" using 2 title "Generated Noise" with linespoints linestyle 1

