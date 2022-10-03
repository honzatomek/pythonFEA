stiff_parameters <-function ( )

#*****************************************************************************80
#
## stiff_parameters() returns parameters of the stiff ODE.
#
#  Discussion:
#
#    If input values are specified, this resets the default parameters.
#    Otherwise, the output will be the current defaults.
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
#  Output:
#
#    real LAMBDA: a parameter.
#
#    real T0: the initial time.
#
#    real Y0: the initial condition.
#
#    real TSTOP: the final time.
#
{
  lambda = 50.0
  t0 = 0.0
  y0 = 0.0
  tstop = 1.0

  return ( list ( lambda = lambda, t0 = t0, y0 = y0, tstop = tstop ) )
}

