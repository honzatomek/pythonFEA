function value = gud ( x )

%*****************************************************************************80
%
%% gud() evaluates the Gudermannian function.
%
%  Discussion:
%
%    The Gudermannian function relates the hyperbolic and trigonometric
%    functions.  For any argument X, there is a corresponding value
%    G so that
%
%      sinh(x) = tan(g).
%
%    The value G is called the Gudermannian of X.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 July 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real X, the argument of the Gudermannian.
%
%  Output:
%
%    real VALUE, the value of the Gudermannian.
%
  value = 2.0 * atan ( tanh ( 0.5 * x ) );

  return
end
