function centroid = cone_centroid_3d ( r, xc, yc, zc, xh, yh, zh )

%*****************************************************************************80
%
%% cone_centroid_2d() returns the centroid of a cone in 3D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Adrian Bowyer and John Woodwark,
%    A Programmer's Geometry,
%    Butterworths, 1983.
%
%  Input:
%
%    real R, the radius of the circle at the base of the cone.
%
%    real XC, YC, ZC, the coordinates of the center of the circle.
%
%    real XH, YH, ZH, the coordinates of the tip of the cone.
%
%  Output:
%
%    real CENTROID(3), the coordinates of the centroid of the cone.
%
  centroid(1:3) = 0.75 * [ xc, yc, zc ] + 0.25 * [ xh, yh, zh ];

  return
end
