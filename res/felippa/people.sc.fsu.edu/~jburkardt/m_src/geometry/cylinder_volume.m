function volume = cylinder_volume ( p1, p2, r )

%*****************************************************************************80
%
%% cylinder_volume() determines the volume of a cylinder.
%
%  Discussion:
%
%    A (right) (finite) cylinder in 3D is the set of points
%    contained on or inside a circle of radius R, whose center
%    lies along the line segment from point P1 to P2, and whose
%    plane is perpendicular to that line segment.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 August 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real P1(3), P2(3), the first and last points
%    on the axis line of the cylinder.
%
%    real R, the radius of the cylinder.
%
%  Output:
%
%    real VOLUME, the volume of the cylinder.
%
  dim_num = 3;

  h = r8vec_distance ( dim_num, p1, p2 );

  volume = pi * r * r * h;

  return
end
