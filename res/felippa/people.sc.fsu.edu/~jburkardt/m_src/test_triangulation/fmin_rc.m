function [ a, b, arg, status ] = fmin_rc ( a, b, status, value )

%*****************************************************************************80
%
%% fmin_rc() seeks a minimizer of a scalar function of a scalar variable.
%
%  Discussion:
%
%    FMIN_RC seeks an approximation to the point where F attains a minimum on
%    the interval (A,B).
%
%    The method used is a combination of golden section search and
%    successive parabolic interpolation.  Convergence is never much
%    slower than that for a Fibonacci search.  If F has a continuous
%    second derivative which is positive at the minimum (which is not
%    at A or B), then convergence is superlinear, and usually of the
%    order of about 1.324....
%
%    The routine is a revised version of the Brent FMIN algorithm,
%    which now uses reverse communication.
%
%    It is worth stating explicitly that this routine will NOT be
%    able to detect a minimizer that occurs at either initial endpoint
%    A or B.  If this is a concern to the user, then the user must
%    either ensure that the initial interval is larger, or to check
%    the function value at the returned minimizer against the values
%    at either endpoint.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Richard Brent,
%    Algorithms for Minimization without Derivatives,
%    Prentice Hall, 1973.
%
%    David Kahaner, Clever Moler, Steven Nash,
%    Numerical Methods and Software,
%    Prentice Hall, 1988.
%
%  Input:
%
%    real A, B, the current estimates for the left and right 
%    endpoints of the interval.  It is required that A < B.
%
%    integer STATUS, set to zero by the user on the first call only.
%    Thereafter, the input value of STATUS should be the output value from
%    the previous call.
%
%    real VALUE, the function value at ARG.  On the very first call,
%    VALUE is not needed, and may be set to zero.
%
%  Output:
%
%    real A, B, a smaller interval containing the minimizer.  
%
%    real ARG, the currently considered point.  On return with 
%    STATUS positive, the user is requested to evaluate the function at ARG, 
%    and return the value in VALUE.  On return with STATUS zero, ARG 
%    is the routine's estimate for the function minimizer.
%
%    integer STATUS, used to communicate between the user
%    and the routine.  The routine returns STATUS positive to request 
%    that the function be evaluated at ARG, or returns STATUS as 0, to 
%    indicate that the iteration is complete and that ARG is the 
%    estimated minimizer.
%
%  Local:
%
%    C is the squared inverse of the golden ratio.
%
%    EPSI is the square root of the relative machine precision.
%
  persistent c;
  persistent d;
  persistent e;
  persistent epsi;
  persistent fu;
  persistent fv;
  persistent fw;
  persistent fx;
  persistent midpoint;
  persistent p;
  persistent q;
  persistent r;
  persistent tol;
  persistent tol1;
  persistent tol2;
  persistent u;
  persistent v;
  persistent w;
  persistent x;
%
%  STATUS (INPUT) = 0, startup.
%
  if ( status == 0 )

    if ( b <= a )
      fprintf ( 1, '\n' );
      fprintf ( 1, 'FMIN_RC - Fatal error!\n' );
      fprintf ( 1, '  A < B is required, but\n' );
      fprintf ( 1, '  A = %f\n', a );
      fprintf ( 1, '  B = %f\n', b );
      status = -1;
      error ( 'FMIN_RC - Fatal error!' );
    end

    c = 0.5 * ( 3.0 - sqrt ( 5.0 ) );

    epsi = sqrt ( eps );
    tol = eps;

    v = a + c * ( b - a );
    w = v;
    x = v;
    e = 0.0;

    status = 1;
    arg = x;

    return
%
%  STATUS (INPUT) = 1, return with initial function value of FX.
%
  elseif ( status == 1 )

    fx = value;
    fv = fx;
    fw = fx;
%
%  STATUS (INPUT) = 2 or more, update the data.
%
  elseif ( 2 <= status )

    fu = value;

    if ( fu <= fx )

      if ( x <= u )
        a = x;
      else
        b = x;
      end

      v = w;
      fv = fw;
      w = x;
      fw = fx;
      x = u;
      fx = fu;

    else

      if ( u < x )
        a = u;
      else
        b = u;
      end

      if ( fu <= fw || w == x )
        v = w;
        fv = fw;
        w = u;
        fw = fu;
      elseif ( fu <= fv || v == x || v == w )
        v = u;
        fv = fu;
      end

    end

  end
%
%  Take the next step.
%
  midpoint = 0.5 * ( a + b );
  tol1 = epsi * abs ( x ) + tol / 3.0;
  tol2 = 2.0 * tol1;
%
%  If the stopping criterion is satisfied, we can exit.
%
  if ( abs ( x - midpoint ) <= ( tol2 - 0.5 * ( b - a ) ) )
    arg = 0.0;
    status = 0;
    return
  end
%
%  Is golden-section necessary?
%
  if ( abs ( e ) <= tol1 )

    if ( midpoint <= x )
      e = a - x;
    else
      e = b - x;
    end

    d = c * e;
%
%  Consider fitting a parabola.
%
  else

    r = ( x - w ) * ( fx - fv );
    q = ( x - v ) * ( fx - fw );
    p = ( x - v ) * q - ( x - w ) * r;
    q = 2.0 * ( q - r );
    if ( 0.0 < q )
      p = -p;
    end
    q = abs ( q );
    r = e;
    e = d;
%
%  Choose a golden-section step if the parabola is not advised.
%
    if ( ...
      ( abs ( 0.5 * q * r ) <= abs ( p ) ) || ...
      ( p <= q * ( a - x ) ) || ...
      ( q * ( b - x ) <= p ) )

      if ( midpoint <= x )
        e = a - x;
      else
        e = b - x;
      end

      d = c * e;
%
%  Choose a parabolic interpolation step.
%
    else

      d = p / q;
      u = x + d;

      if ( ( u - a ) < tol2 )
        d = abs ( tol1 ) * r8_sign ( midpoint - x );
      end

      if ( ( b - u ) < tol2 )
        d = abs ( tol1 ) * r8_sign ( midpoint - x );
      end

    end

  end
%
%  F must not be evaluated too close to X.
%
  if ( tol1 <= abs ( d ) )
    u = x + d;
  end

  if ( abs ( d ) < tol1 )
    u = x + abs ( tol1 ) * r8_sign ( d );
  end
%
%  Request value of F(U).
%
  arg = u;
  status = status + 1;

  return
end
