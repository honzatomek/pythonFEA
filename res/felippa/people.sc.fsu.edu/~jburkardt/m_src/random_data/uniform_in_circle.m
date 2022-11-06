function x = uniform_in_circle ( n )

%*****************************************************************************80
%
%% uniform_in_circle() maps uniform points into the unit circle.
%
%  Discussion:
%
%    The unit circle has center at the origin, and radius 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    09 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(N,2), the points.
%
  x = zeros ( n, 2 );

  r = rand ( 1, n );
  r(1:n) = sqrt ( r(1:n) );

  t = rand ( 1, n );
  t(1:n) = 2.0 * pi * t(1:n);

  x(1:n,1) = r(1:n) .* cos ( t(1:n) );
  x(1:n,2) = r(1:n) .* sin ( t(1:n) );

  return
end
