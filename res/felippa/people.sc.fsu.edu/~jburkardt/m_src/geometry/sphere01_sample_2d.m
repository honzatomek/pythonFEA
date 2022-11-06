function x = sphere01_sample_2d ( )

%*****************************************************************************80
%
%% sphere01_sample_2d() picks a random point on the unit sphere (circle) in 2D.
%
%  Discussion:
%
%    The unit sphere in 2D satisfies:
%
%      X * X + Y * Y = 1
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 February 2005
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real X(2), a random point on the unit circle.
%
  u = rand ( 1, 1 );

  x(1) = cos ( 2.0 * pi * u );
  x(2) = sin ( 2.0 * pi * u );

  return
end
