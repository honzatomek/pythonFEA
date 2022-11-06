function degrees = radians_to_degrees ( radians )

%*****************************************************************************80
%
%% radians_to_degrees() converts an angle from radians to degrees.
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
%    real RADIANS, an angle in radians.
%
%  Output:
%
%    real DEGREES, the equivalent angle in degrees.
%
  degrees = ( radians / pi ) * 180.0;

  return
end
