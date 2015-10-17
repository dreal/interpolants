set xlabel "Proof size"
set ylabel "Interpolant size"
set terminal svg size 400,350 fname "serif"
set output "itpsize2.svg"
set xrange [1:10000000]
set yrange [1:10000000]
set logscale
set tics font "serif,10"
set xtics rotate by -45
plot "itpsize2.dat" using 2:3 notitle, x notitle
exit
