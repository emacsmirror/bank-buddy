set terminal png size 800,600
set output '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/monthly-progress-comparison.png'
set title 'Monthly Spending Progress Comparison'
set xlabel 'Day of Month'
set ylabel 'Cumulative Spending (£)'
set key outside right
set xtics 1,5
set grid
plot for [i=2:7] '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/monthly-progress-comparison.dat' using 1:i with lines title columnhead
