set xlabel "Proof size"
set ylabel "Interpolant size"
set xrange [1:400000]
set yrange [1:400000]
set logscale
set terminal svg size 400,350
set output "itpsize2.svg"
plot "itpsize2.dat" using 2:3 notitle, x notitle
exit
