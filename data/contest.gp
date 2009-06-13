set title "Java 20 message/second"
set xlabel "pingers"
set ylabel "ms"
set term gif
set output "contest.20.java.gif"
set data style lp
plot "contest.20.java" using 1:2 t "Java Median", "contest.20.java" using 1:3 t "Java Deviation", "contest.20.java" using 1:4 t "Java Errors"
