eulersys <- function ( f, t0, y0, h, n )

#*****************************************************************************80
#
## eulersys: Euler method on a system of ordinary differential equations (ODE).
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
#    28 March 2020
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
#  Input:
#
#    function f ( t, y ): evaluates the right hand side of the ODE at (t,y).
#
#    real T0: the initial time.
#
#    real Y0(*): the initial solution values.
#
#    real H: the stepsize to use.
#
#    integer N: the number of steps to take.
#
#  Output:
#
#    dataframe ( T(1:n+1), Y(1:n+1,*) ): the computed sequence of 
#    solution estimates.
#
{
  t <- t0
  y <- y0
  values <- data.frame ( t = t, t(y0) )

  for ( i in 1 : n )
  {
    y0 <- y0 + h * f ( t0, y0 )
    t0 <- t0 + h
    values <- rbind ( values, data.frame ( t = t0, t(y0) ) )
  }

  return ( values )
}
