function f = f13_f0 ( n, x, y )

%*****************************************************************************80
%
%% f13_f0() returns the value of function 13.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 August 2012
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
  f(1:n,1) = 1.0 ./ ( ( 10 * x(1:n,1) - 5 ).^2 + ( 10 * y(1:n,1) - 5 ).^2 + 1.0 );

  return
end
