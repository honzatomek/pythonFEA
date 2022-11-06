function value = cot_rad ( angle )

%*****************************************************************************80
%
%% cot_rad() returns the cotangent of an angle given in radians.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 January 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real ANGLE, the angle, in radians.
%
%  Output:
%
%    real VALUE, the cotangent of the angle.
%
  value  = cos ( angle ) / sin ( angle );

  return
end
