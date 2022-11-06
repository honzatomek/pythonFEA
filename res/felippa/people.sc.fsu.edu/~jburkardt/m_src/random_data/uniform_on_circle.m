function p = uniform_on_circle ( n )

%*****************************************************************************80
%
%% uniform_on_circle() returns randomly selected points on a circle.
%
%  Discussion:
%
%    The circle is assumed to have the equation:
%
%      x^2 + y^2 = 1
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points to compute.
%
%  Output:
%
%    real P(N,2), points uniformly distributed on the perimeter of the circle.
%
  r = 2.0 * pi * rand ( n, 1 );

  p(:,1) = cos ( r );
  p(:,2) = sin ( r );

  return
end

