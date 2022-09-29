stiff_exact <- function ( t )

#*****************************************************************************80
#
## stiff_exact() evaluates the exact solution of the stiff ODE.
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
#    28 April 2021
#
#  Author:
#
#    John Burkardt
#
#  Input:
#
#    real T(:): the evaluation times.
#
#  Output:
#
#    real Y(:): the exact solution values.
#
{
  output <- stiff_parameters ( )
  lambda <- output$lambda

  value <- lambda * ( sin ( t ) + lambda * cos ( t ) - lambda * exp ( - lambda * t ) ) / ( lambda^2 + 1.0 )

  return ( value )
}

