rungekutta4 <- function ( f, x0, y0, h, n )

#*****************************************************************************80
#
## rungekutta4: 4th order Runge-Kutta for ordinary differential equation (ODE).
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
  x <- x0
  y <- y0

  for ( i in 1 : n )
  {
    s1 <- h * f ( x0,     y0 )
    s2 <- h * f ( x0+h/2, y0+s1/2 )
    s3 <- h * f ( x0+h/2, y0+s2/2 )
    s4 <- h * f ( x0+h,   y0+s3 )
    y0 <- y0 + s1 / 6.0 + s2 / 3.0 + s3 / 3.0 + s4 / 6.0
    x0 <- x0 + h
    x <- c ( x, x0 )
    y <- c ( y, y0 )
  }

  return ( data.frame ( x = x, y = y ) )
}
