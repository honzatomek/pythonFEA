trapezoidal <- function ( f, t0, tstop, y0, n, ... ) 

#*****************************************************************************80
#
## trapezoidal() implements the trapezoidal ODE solver.
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
#    Adapted from "cranknic.R, Hans Borchers, the pracma library.
#    Modifications by John Burkardt
#
#  Input:
#
#    function f ( t, y, ... ): evaluates the right hand side of the ODE.
#
#    real T0: the initial time.
#
#    real TSTOP: the final time.
#
#    real Y0: the initial solution values.
#
#    integer N: the number of steps to take.
#
#    ...: optional additional arguments to be passed to F().
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
    is.numeric(tstop),
    length(tstop) == 1 )

  if ( is.vector(y0) )
  {
    y0 <- as.matrix(y0)
  }
  else if ( is.matrix(y0) )
  {
    if ( ncol(y0) != 1 ) 
    {
      stop ( "trapezoidal: Argument 'y0' must be a row or column vector.")
    }
  }

  fun <- match.fun(f)
  f <- function(t, y) fun ( t, y, ... )

  y_length <- length ( y0 )
  y <- y0
  yout <- matrix ( NA, n, y_length )
  yout[1, ] <- c(y0)

  dt <- ( tstop - t0 ) / ( n - 1 )
  t <- 0
  ts <- linspace ( t0, tstop, n )
#
#  internal function used for root finding
#
  cnfun <- function(w)  w - y - 0.5* dt * ( f(t, w) + f(t, y) )
  f_length <- length ( f ( t0, y0 ) )
  if ( f_length != y_length )
  {
    stop ( "trapezoidal: Function f must return a vector the same length as 'y0'.")
  }
#
#  Choose the solver used for root finding.
#
  if ( y_length == 1 )
  {
    solver <- fzero
  }
  else
  {
    solver <- fsolve
  }

  for ( i in 2 : n )
  {
    t <- ts[i]
    w <- solver ( cnfun, y )$x
    yout[i, ] <- w
    y <- w
  }

  if ( y_length == 1 ) 
  {
    yout <- drop ( yout )
  }

  return ( list ( t = ts, y = yout ) )
}
