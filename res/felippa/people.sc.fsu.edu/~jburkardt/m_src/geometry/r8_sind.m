function value = r8_sind ( degrees )

%*****************************************************************************80
%
%% r8_sind() returns the sine of an angle given in degrees.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 July 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real DEGREES, the angle in degrees.
%
%  Output:
%
%    real VALUE, the sine of the angle.
%
  radians = pi * ( degrees / 180.0 );

  value = sin ( radians );

  return
end
