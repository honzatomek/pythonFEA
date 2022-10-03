hund_midpoint <- function ()

#*****************************************************************************80
#
## hund_midpoint() uses midpoint() to solve the Hund ODE.
#
#  Licensing:
#
#    This code is distributed under the GNU LGPL license.
#
#  Modified:
#
#    28 April 2021
#
{
  cat ( "\n" )
  cat ( "hund_midpoint\n" )
  cat ( " ", version$version.string, "\n" )
  cat ( "  Use midpoint() to solve the Hund ordinary differential equation.\n" )

  library ( "pracma" )

  source ( "/home/burkardt/public_html/r_src/midpoint/midpoint.R" ) 
  source ( "/home/burkardt/public_html/r_src/midpoint_test/hund_deriv.R" )   

  t0 <- 0.0
  tstop <- 60.0
  y0 <- c ( 0.0, 10.0 )
  n <- 50
  sol <- midpoint ( hund_deriv, t0, tstop, y0, n )
#
#  Phase plot.
#
  png ( 'hund_midpoint_phase.png' )
  plot ( sol$y[, 1], sol$y[, 2], type = "l", lwd = 2, col = "blue",
       xlab = "", ylab = "", main = '"Hund midpoint: phase"')
  grid ( )
#
#  Time plot.
#
  png ( 'hund_midpoint_plot.png' )
  plot ( sol$t, sol$y[,1], type = "l", lwd = 2, col = "red",
       xlab = "", ylab = "", main = '"Hund midpoint: plot"')
  lines ( sol$t, sol$y[,2], type = "l", lwd = 2, col = "blue" )
  grid ( )
}
