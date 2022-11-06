function angle = degrees_to_radians ( angle )

%*****************************************************************************80
%
%% degrees_to_radians() converts an angle from degrees to radians.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 April 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real ANGLE, an angle in degrees.
%
%  Output:
%
%    real ANGLE, the equivalent angle in radians.
%
  angle = ( angle / 180.0 ) * pi;

  return
end
