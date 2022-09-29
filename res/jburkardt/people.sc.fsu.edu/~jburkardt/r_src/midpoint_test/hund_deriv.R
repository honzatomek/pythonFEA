hund_deriv <- function ( t, y )

#*****************************************************************************80
#
## hund_deriv() evaluates the right hand side of the Hund ODE.
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
#    real T, Y(2): the time and solution value.
#
#  Output:
#
#    real DYDT(2): the derivative value.
#
{
  y1 <- y[1]
  y2 <- y[2]
  s <- sqrt ( y1^2 + y2^2 )
  dy1 <- 0.5 - 0.5 * y1 / s
  dy2 <- -0.5 * y2 / s

  return ( c ( dy1, dy2 ) )
}

