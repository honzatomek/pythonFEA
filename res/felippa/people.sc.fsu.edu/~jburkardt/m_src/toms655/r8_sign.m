function value = r8_sign ( x )

%*****************************************************************************80
%
%% r8_sign() returns the sign of an R8.
%
%  Discussion:
%
%    r8_sign(x) is:
%    +1:     0 <= x
%    -1: x < 0
%
%    This function can handle any real scalar, vector, or array argument.
%
%    Note that, in MATLAB, we have instead a three-way function:
%
%    r8_sign(x) is:
%    +1:     0 <  x
%     0:     0 == x
%    -1: x < 0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real X, the number whose sign is desired.
%
%  Output:
%
%    real VALUE, +1 or -1, the sign of X.
%
  value = 2.0 * ( 0.0 <= x ) - 1.0;

  return
end
 
