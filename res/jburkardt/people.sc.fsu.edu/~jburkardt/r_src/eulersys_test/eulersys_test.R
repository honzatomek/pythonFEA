#*****************************************************************************80
#
## eulersys_test() tests eulersys().
#
#  Licensing:
#
#    Copyright 2016 James P. Howard, II
#
#    The computer code and data files on this web page are distributed under
#    https://opensource.org/licenses/BSD-2-Clause, the BSD-2-Clause license.
#
#  Modified:
#
#    26 April 2021
#
#  Author:
#
#    John Burkardt
#
cat ( date ( ), "\n" )
cat ( "\n" )
cat ( "eulersys_test():\n" )
cat ( " ", version$version.string, "\n" )
cat ( "  eulersys() uses the Euler method to solve a system\n" )
cat ( "  of ordinary differential equations.\n" )

source ( "/home/burkardt/public_html/r_src/eulersys/eulersys.R" )  

odesys <- function ( t, y ) 
{
  y1 <- t
  y2 <- y[2] - y[1]
  y3 <- y[3] + y[1]
  return ( c ( y1 = y1, y2 = y2, y3 = y3 ) )
}
#
#  Set the initial condition.
#
t0 <- 0.0
y0 <- c ( y1 = 1.0, y2 = 2.0, y3 = 3.0 )
tstop <- 1.0
n <- 20
dt <- ( tstop - t0 ) / n

sol <- eulersys ( odesys, t0, y0, dt, n )
#
#  Time plot.
#  The LINES command does NOT allow me to display the three curves
#  on one plot.  Another lie.
#
png ( "eulersys_y1.png" )
plot ( sol$t, sol$y1, type = "l", col = "red" )
grid ( )

png ( "eulersys_y2.png" )
plot ( sol$t, sol$y2, type = "l", col = "green" )
grid ( )

png ( "eulersys_y3.png" )
plot ( sol$t, sol$y3, type = "l", col = "blue" )
grid ( )
#
#  Terminate.
#
cat ( "\n" )
cat ( "eulersys_test\n" )
cat ( "  Normal end of execution.\n" )
cat ( date ( ), "\n" )

quit ( )
