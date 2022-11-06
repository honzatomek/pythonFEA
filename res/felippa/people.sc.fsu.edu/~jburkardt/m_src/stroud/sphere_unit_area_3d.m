function value = sphere_unit_area_3d ( )

%*****************************************************************************80
%
%% sphere_unit_area_3d() computes the surface area of a unit sphere in 3D.
%
%  Integration region:
%
%    Points (X,Y,Z) such that:
%
%      X**2 + Y**2 + Z**2 = 1.
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
%    real VALUE, the area of the sphere.
%
  value = 4.0E+00 * pi;

  return
end
