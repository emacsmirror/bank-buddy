set terminal png size 800,600 enhanced font 'Verdana,10'
set output '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/bank-buddy-monthly-plots/plot-202405-breakdown.png'
set style data histogram
set style fill solid 0.8 border -1
set xtics rotate by -45
set key off
set title 'Category Breakdown for 2024-05 (Total: £711.92)'
set ylabel 'Amount (£)'
set yrange [0:*]
set grid y
set boxwidth 0.8 relative
plot '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/bank-buddy-monthly-plots/data-breakdown-202405.txt' using 2:xtic(1) with boxes lc rgb '#4169E1'
