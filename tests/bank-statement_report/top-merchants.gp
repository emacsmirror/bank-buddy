set terminal png size 800,600 enhanced font 'Verdana,10'
set output '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/financial-report--top-merchants.png'
set style data histogram
set style fill solid
set boxwidth 0.8
set xtics rotate by -45
set ylabel "Amount"
set title "Top Merchants"
plot '/home/jdyer/source/repos/bank-buddy/tests/2024-06-30--bank-statement_report/top-merchants.dat' using 2:xtic(1) with boxes title "Amount"
