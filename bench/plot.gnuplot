data_dir = "."

set title "MCM Should Be O(v^3) In General Graphs.\n".\
"(Gabow, 1976, p. 221)"
set key left
set term png size 800, 500
set output data_dir."/plot.png"

set linetype 1 pointtype 7 linecolor rgb "#FF0000"
set linetype 2 linewidth 3 linecolor rgb "#00B800"
set linetype 3 pointtype 7 linecolor rgb "#0000FF"

set xlabel 'Number of Vertexes, v' textcolor rgb "black"
set ytics autofreq textcolor rgb "black"
set ylabel 'Time (s)' textcolor rgb "black"
set y2tics autofreq textcolor rgb "black"
set y2label 'v^3' textcolor rgb "black"

plot data_dir."/time_ruby.data" using 1:2 title "Time (s)" lt 1, \
data_dir."/time_clojure.data" using 1:2 title "Time (s)" lt 3, \
data_dir."/v_cubed.data" using 1:2 title "v^3" with lines lt 2 axes x1y2
# plot  \
# data_dir."/v_cubed.data" using 1:2 title "v^3" with lines lt 2 axes x1y2
# plot data_dir."/time.data" using 1:2 title "Time (s)" lt 1
