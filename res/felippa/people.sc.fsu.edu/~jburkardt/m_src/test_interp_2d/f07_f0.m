function f = f07_f0 ( n, x, y )

%*****************************************************************************80
%
%% f07_f0() returns the value of function 7.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    29 January 2012
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of evaluation points.
%
%    real X(N,1), Y(N,1), the evalution points.
%
%  Output:
%
%    real F(N,1), the function values.
%
  f(1:n,1) = 2.0 * cos ( 10.0 * x(1:n,1) ) .* sin ( 10.0 * y(1:n,1) ) ...
    + sin ( 10.0 * x(1:n,1) .* y(1:n,1) );

  return
end
