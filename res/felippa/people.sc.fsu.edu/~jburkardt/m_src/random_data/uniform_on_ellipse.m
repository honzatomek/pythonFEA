function p = uniform_on_ellipse ( n, a, b )

%*****************************************************************************80
%
%% uniform_on_ellipse() returns randomly selected points on an ellipse.
%
%  Discussion:
%
%    The ellipse is assumed to have the equation:
%
%      (x/a)^2 + (y/b)^2 = 1
%
%    Actually, the points are not uniformly distributed along the
%    circumference.  They are only uniformly distributed in the
%    angular sense.
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
%    real A, B: the ellipse parameters:
%
%  Output:
%
%    real P(N,2), points uniformly distributed on the perimeter of the ellipse.
%
  r = 2.0 * pi * rand ( n, 1 );

  p(:,1) = a * cos ( r );
  p(:,2) = b * sin ( r );

  return
end

