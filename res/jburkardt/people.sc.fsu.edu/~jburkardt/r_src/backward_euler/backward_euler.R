backward_euler <- function ( f, t0, tstop, y0, n, ... ) 

#*****************************************************************************80
#
## backward_euler() implements the backward Euler ODE solver.
#
#  Licensing:
#
#    This code is distributed under the GNU LGPL license.
#
#  Modified:
#
#    28 April 2021
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
#    ...: optional additional arguments to be passed to f.
#
#  Output:
#
#    list ( T(1:n), Y(1:n) ): the computed sequence of 
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
      stop ( "backward_euler: Argument 'y0' must be a row or column vector.")
    }
  }
#
#  The ... allows additional arguments to be passed to the function
#  through the backward_euler argument list.
#
  fun <- match.fun ( f )
  f <- function ( t, y ) fun ( t, y, ... )

  y_length <- length(y0)
  y <- y0
  yout <- matrix ( NA, n, y_length )
  yout[1, ] <- c(y0)

  dt <- ( tstop - t0 ) / ( n - 1 )
  t <- 0
  ts <- linspace ( t0, tstop, n )
#
#  internal function used for root finding
#
  befun <- function(w) w - y - dt * f ( t, w )

  f_length <- length ( f(t0, y0) )
  if ( f_length != y_length )
  {
    stop ( "backward_euler: Function f must return a vector the same length as 'y0'.")
  }
#
#  Solver used for root finding
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
    w <- solver ( befun, y )$x
    yout[i, ] <- w
    y <- w
  }

  if ( y_length == 1 )
  {
    yout <- drop ( yout )
  }

  return ( list ( t = ts, y = yout ) )
}
