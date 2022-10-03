stiff_midpoint <- function ()

#*****************************************************************************80
#
## stiff_midpoint() uses midpoint() to solve the stiff ODE.
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
  cat ( "stiff_midpoint\n" )
  cat ( " ", version$version.string, "\n" )
  cat ( "  Use midpoint() to solve the stiff ordinary differential equation.\n" )

  library ( "pracma" )

  source ( "/home/burkardt/public_html/r_src/midpoint/midpoint.R" ) 
  source ( "/home/burkardt/public_html/r_src/midpoint_test/stiff_deriv.R" )
  source ( "/home/burkardt/public_html/r_src/midpoint_test/stiff_exact.R" )  
  source ( "/home/burkardt/public_html/r_src/midpoint_test/stiff_parameters.R" )   

  parameters <- stiff_parameters ( )

  t0 <- parameters$t0
  y0 <- parameters$y0
  tstop <- parameters$tstop
  n <- 27

  sol <- midpoint ( stiff_deriv, t0, tstop, y0, n )
#
#  Time plot.
#
  t <- sol$t
  y <- sol$y

  ye <- stiff_exact ( t )

  png ( 'stiff_midpoint_plot.png' )
  plot ( t, y, type = "l", lwd = 2, col = "red",
       xlab = "", ylab = "", main = '"stiff midpoint: plot"')
  lines ( t, ye, type = "l", lwd = 2, col = "blue" )
  grid ( )
}
