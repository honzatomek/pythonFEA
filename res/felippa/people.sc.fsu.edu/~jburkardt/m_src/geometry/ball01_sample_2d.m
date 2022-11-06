function x = ball01_sample_2d ( )

%*****************************************************************************80
%
%% ball01_sample_2d() picks a random point in the unit ball in 2D.
%
%  Discussion:
%
%    The unit ball is the set of points (X,Y) such that
%
%      X(1) * X(1) + X(2) * X(2) <= 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real X(2), a random point in the unit ball.
%
  u = rand ( 2, 1 );

  r = sqrt ( u(1) );
  theta = 2.0 * pi * u(2);

  x(1) = r * cos ( theta );
  x(2) = r * sin ( theta );

  return
end
