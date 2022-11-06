function [ fxx, fxy, fyy ] = f05_f2 ( n, x, y )

%*****************************************************************************80
%
%% f05_f2() returns second derivatives of function 5.
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
%    real FXX(N,1), FXY(N,1), FYY(N,1), second derivatives.
%
  t1(1:n,1) = x(1:n,1) - 0.5;
  t2(1:n,1) = y(1:n,1) - 0.5;
  t3(1:n,1) = - 13.5 * exp ( - 20.25 * ( t1(1:n,1) .* t1(1:n,1) + t2(1:n,1) .* t2(1:n,1) ) );

  fxx(1:n,1) = ( 1.0 - 40.5 * t1(1:n,1) .* t1(1:n,1) ) .* t3(1:n,1);
  fxy(1:n,1) = - 40.5 * t1(1:n,1) .* t2(1:n,1) .* t3(1:n,1);
  fyy(1:n,1) = ( 1.0 - 40.5 * t2(1:n,1) .* t2(1:n,1) ) .* t3(1:n,1);

  return
end
