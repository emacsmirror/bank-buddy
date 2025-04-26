set terminal png size 1000,600 enhanced font 'Verdana,10'
set output '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/financial-report--monthly-transactions.png'
set title 'Monthly Transaction Counts and Spending'
set xlabel 'Month'
set ylabel 'Number of Transactions'
set y2label 'Spending (£)'
set ytics nomirror
set y2tics
set xtics rotate by -45
set key outside right top
set style data histogram
set style fill solid 1.0
set boxwidth 0.4
set offset 0,0,0,0
set y2range [0:*]
set yrange [0:*]
plot '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/monthly-transactions.dat' using 2:xtic(1) with boxes axes x1y1 title 'Transactions' lc rgb '#4169E1', \
     '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/monthly-transactions.dat' using ($0):3 with linespoints axes x1y2 title 'Spending' lw 2 pt 7 lc rgb '#FF4500'
