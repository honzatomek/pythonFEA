function value = cone_volume ( r, h )

%*****************************************************************************80
%
%% cone_volume() returns the volume of a cone in 3D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 May 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real R, the radius of the base of the cone.
%
%    real H, the height of the cone.
%
%  Output:
%
%    real VALUE, the volume of the cone.
%
  value = ( pi / 3.0 ) * h * r * r;

  return
end
