set terminal png size 1200,600 enhanced font 'Verdana,10'
set output '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/financial-report--monthly-spending-stacked.png'
set style data histograms
set style histogram rowstacked
set boxwidth 0.75 relative
set style fill solid 1.0 border -1
set title 'Monthly Spending by Category'
set xlabel 'Month'
set ylabel 'Amount (£)'
set xtics rotate by -45
set key outside right top vertical
set auto x
set yrange [0:*]
set grid ytics
plot for [i=3:19] '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/monthly-categories-stacked.dat' using i:xtic(1) title columnheader(i), \
     '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/monthly-categories-stacked.dat' using ($0-1):2 with linespoints \
     linecolor rgb "#000000" linewidth 3 pointtype 7 pointsize 1.5 title "Total"
