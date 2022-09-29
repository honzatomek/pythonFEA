#*****************************************************************************80
#
## backward_euler_test() tests backward_euler().
#
#  Licensing:
#
#    This code is distributed under the GNU LGPL license.
#
#  Modified:
#
#    26 April 2021
#
cat ( date ( ), "\n" )
cat ( "\n" )
cat ( "backward_euler_test()\n" )
cat ( " ", version$version.string, "\n" )
cat ( "  Use backward_euler() to solve an ordinary differential equation.\n" )

source ( "/home/burkardt/public_html/r_src/backward_euler/backward_euler.R" )  

##  System of differential equations
# "Herr und Hund"

library ( "pracma" )

fhh <- function ( t, y ) 
{
  y1 <- y[1]
  y2 <- y[2]
  s <- sqrt(y1^2 + y2^2)
  dy1 <- 0.5 - 0.5*y1/s
  dy2 <- -0.5*y2/s
  return ( c ( dy1, dy2 ) )
}

t0 = 0.0
tstop = 60.0 
y0 = c ( 0.0, 10.0 )
n = 101

sol <- backward_euler ( fhh, t0, tstop, y0, n )
#
#  Phase plot.
#
png ( 'backward_euler_phase.png' )
plot ( sol$y[, 1], sol$y[, 2], type = "l", lwd = 2, col = "blue",
     xlab = "", ylab = "", main = '"Hund backward euler: phase"')
grid ( )
#
#  Time plot.
#
png ( 'backward_euler_plot.png' )
plot ( sol$t, sol$y[,1], type = "l", lwd = 2, col = "red",
     xlab = "", ylab = "", main = '"Hund backward_euler: plot"')
lines ( sol$t, sol$y[,2], type = "l", lwd = 2, col = "blue" )
grid ( )
#
#  Terminate.
#
cat ( "\n" )
cat ( "backward_euler_test():\n" )
cat ( "  Normal end of execution.\n" )
cat ( date ( ), "\n" )

quit ( )

