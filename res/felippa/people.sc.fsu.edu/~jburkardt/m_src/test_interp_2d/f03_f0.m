function f = f03_f0 ( n, x, y )

%*****************************************************************************80
%
%% f03_f0() returns the value of function 3.
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
  f(1:n,1) = ( 1.25 + cos ( 5.4 * y(1:n,1) ) ) ...
    ./ ( 6.0 + 6.0 * ( 3.0 * x(1:n,1) - 1.0 ).^2 );

  return
end
