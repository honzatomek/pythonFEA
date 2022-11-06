function value = pyramid_volume_3d ( r, h )

%*****************************************************************************80
%
%% pyramid_volume_3d() returns the volume of a pyramid with square base in 3D.
%
%  Discussion:
%
%    A pyramid with square base can be regarded as the upper half of a
%    3D octahedron.
%
%    Z - R <= X <= R - Z
%    Z - R <= Y <= R - Z
%    0 <= Z <= H.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 July 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real R, the "radius" of the pyramid, that is, half the
%    length of one of the sides of the square base.
%
%    real H, the height of the pyramid.
%
%  Output:
%
%    real VALUE, the volume of the pyramid.
%
  value = ( 4.0 / 3.0 ) * h * r * r;

  return
end
