function value = ball_unit_volume_3d ( )

%*****************************************************************************80
%
%% ball_unit_volume_3d() computes the volume of a unit ball in 3D.
%
%  Integration region:
%
%    Points (X,Y,Z) such that
%
%      X^2 + Y^2 + Z^2 <= 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    26 May 2004
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real VALUE, the volume of the ball.
%
  value = ( 4.0E+00 / 3.0E+00 ) * pi;

  return
end
