set terminal png size 1200,600 enhanced font 'Verdana,10'
set output '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/financial-report--monthly-spending-categories.png'
set title 'Monthly Spending by Category'
set xlabel 'Month'
set ylabel 'Amount (£)'
set style data linespoints
set key outside right top vertical
set xtics rotate by -45
set grid
set auto x
plot for [i=3:19] '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/monthly-categories-lines.dat' using 0:i:xtic(1) title columnheader(i) with linespoints pointtype i-2 lw 2
