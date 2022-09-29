trapezoidal_fixed <- function ( f, t0, t1, y0, ..., N = 100 ) 

#*****************************************************************************80
#
## trapezoidal_fixed() implements the trapezoidal ODE solver.
#
#  Licensing:
#
#    This code is distributed under the GNU LGPL license.
#
#  Modified:
#
#    26 April 2021
#
#  Author:
#
#    Adapted from "cranknic.R, Hans Borchers, the pracma library.
#    Modifications by John Burkardt
#
#  Input:
#
#    function f ( x, y ): evaluates the right hand side of the ODE at (x,y).
#
#    real T0: the initial time.
#
#    real T1: the final time.
#
#    real Y0: the initial solution values.
#
#    integer N: the number of steps to take.
#
#  Output:
#
#    list ( T(1:n+1), Y(1:n+1) ): the computed sequence of 
#    solution estimates.
#
{
  stopifnot ( 
    is.numeric(y0),
    is.numeric(t0),
    length(t0) == 1, 
    is.numeric(t1),
    length(t1) == 1 )

  if ( is.vector(y0) )
  {
    y0 <- as.matrix(y0)
  }
  else if ( is.matrix(y0) )
  {
    if (ncol(y0) != 1) 
    {
      stop ( "trapezoidal_fixed: Argument 'y0' must be a row or column vector.")
    }
  }

  fun <- match.fun(f)
  f <- function(t, y) fun(t, y, ...)

  n <- length(y0)
  yo <- y0
  to <- t0
  yout <- matrix ( NA, N, n )
  yout[1, ] <- c(y0)

  h <- ( t1 - t0 ) / ( N - 1 )
  t <- 0
  ts <- linspace ( t0, t1, N )

  dt = ( t1 - t0 ) / ( N - 1 )

  m <- length ( f ( t0, y0 ) )
  if ( m != n )
  {
    stop ( "trapezoidal_fixed: Function f must return a vector the same length as 'y0'.")
  }

  itmax = 10

  for ( i in 2 : N )
  {
    tn <- ts[i]
    yn <- yo + dt * f ( to, yo )

    for ( j in 1 : itmax )
    {
      yn <- yo + 0.5 * dt * ( f ( to, yo ) + f ( tn, yn ) )
    }

    yout[i, ] <- yn

    to <- tn
    yo <- yn
  }

  if ( n == 1 ) 
  {
    yout <- drop(yout)
  }

  return ( list ( t = ts, y = yout ) )
}
