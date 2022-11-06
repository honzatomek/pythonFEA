function radians = degrees_to_radians ( degrees )

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
%    13 June 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real DEGREES, an angle in degrees.
%
%  Output:
%
%    real RADIANS, the equivalent angle in radians.
%
  radians = ( degrees / 180.0 ) * pi;

  return
end
