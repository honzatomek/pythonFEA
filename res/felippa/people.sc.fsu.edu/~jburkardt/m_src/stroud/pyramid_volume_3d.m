function value = pyramid_volume_3d ( r, h )

%*****************************************************************************80
%
%% pyramid_volume_3d() returns the volume of a pyramid with square base in 3D.
%
%  Integration region:
%
%    Z - 1 <= X <= 1 - Z
%    Z - 1 <= Y <= 1 - Z
%    0 <= Z <= 1.
%
%  Discussion:
%
%    A pyramid with square base can be regarded as the upper half of a
%    3D octahedron.
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
