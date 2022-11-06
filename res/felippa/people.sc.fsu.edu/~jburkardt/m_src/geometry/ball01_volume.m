function value = ball01_volume ( )

%*****************************************************************************80
%
%% ball01_volume(): volume of the unit ball in 3D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real VALUE, the volume.
%
  r = 1.0;
  value = 4.0 * pi * r ^ 3 / 3.0;

  return
end
