adamsbashforth <- function ( f, x0, y0, h, n )

#*****************************************************************************80
#
## adamsbashforth: 4th order Runge-Kutta for ordinary differential equation (ODE).
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
#    25 March 2020
#
#  Author:
#
#    Original R code by James Howard;
#    Modifications by John Burkardt.
#
#  Reference:
#
#    James Howard,
#    Computational Methods for Numerical Analysis with R,
#    CRC Press, 2017
#    ISBN13: 978-1-4987-2363-3.
#
{
  x1 <- x0 + h
  y1 <- y0 + h * f ( x0, y0 )

  x <- c ( x0, x1 )
  y <- c ( y0, y1 )
  n <- n - 1

  for ( i in 1 : n )
  {
    xn <- x1 + h
    yn <- y1 + 1.5 * h * f( x1, y1 ) - 0.5 * h * f ( x0, y0 )

    x0 <- x1
    y0 <- y1
    x1 <- xn
    y1 <- yn

    x <- c ( x, x1 )
    y <- c ( y, y1 )
  }

  return ( data.frame ( x = x, y = y ) )
}
