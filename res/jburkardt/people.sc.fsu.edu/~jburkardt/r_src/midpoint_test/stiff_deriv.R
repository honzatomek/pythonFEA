stiff_deriv <- function ( t, y )

#*****************************************************************************80
#
## stiff_deriv() evaluates the right hand side of the stiff ODE.
#
#  Discussion:
#
#    y' = lambda * ( cos(t) - y )
#    y(t0) = y0
#
#  Licensing:
#
#    This code is distributed under the GNU LGPL license.
#
#  Modified:
#
#    27 April 2021
#
#  Author:
#
#    John Burkardt
#
#  Input:
#
#    real T, Y: the time and solution value.
#
#  Output:
#
#    real DYDT: the derivative value.
#
{
  output <- stiff_parameters ( )
  lambda = output$lambda

  dydt <- lambda * ( cos ( t ) - y )

  return ( dydt )
}

