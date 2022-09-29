#*****************************************************************************80
#
## trapezoidal_test() tests trapezoidal().
#
#  Modified:
#
#    26 April 2021
#
cat ( date ( ), "\n" )
cat ( "\n" )
cat ( "trapezoidal_test()\n" )
cat ( " ", version$version.string, "\n" )
cat ( "  Use trapezoidal() to solve an ordinary differential equation.\n" )

source ( "/home/burkardt/public_html/r_src/trapezoidal/trapezoidal.R" )  
#
#  Need the pracma library for fsolve() and linspace().
#
library ( "pracma" )
#
#  System of differential equations "Herr und Hund"
#
hund_deriv <- function ( x, y ) 
{
  y1 <- y[1]
  y2 <- y[2]
  s <- sqrt(y1^2 + y2^2)
  dy1 <- 0.5 - 0.5*y1/s
  dy2 <- -0.5*y2/s
  return ( c ( dy1, dy2 ) )
}

t0 <- 0.0
tstop <- 60.0
y0 <- c ( 0.0, 10.0 )
n <- 100

sol <- trapezoidal ( hund_deriv, t0, tstop, y0, n )
#
#  Phase plot.
#
png ( 'trapezoidal_phase.png' );
plot ( sol$y[, 1], sol$y[, 2], type = "l", lwd = 2, col = "blue",
     xlab = "y1", ylab = "y2", main = '"Hund trapezoidal: phase"')
grid ( )
#
#  Time plot.
#
png ( 'trapezoidal_plot.png' );
plot ( sol$t, sol$y[,1], type = "l", lwd = 2, col = "red",
     xlab = "t", ylab = "y(t)", main = '"Hund trapezoidal: plot"')
lines ( sol$t, sol$y[,2], type = "l", lwd = 2, col = "blue" )
grid ( )
#
#  Terminate.
#
cat ( "\n" )
cat ( "trapezoidal_test():\n" )
cat ( "  Normal end of execution.\n" )
cat ( date ( ), "\n" )

quit ( )

