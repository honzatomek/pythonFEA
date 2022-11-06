function v = tp_to_xyz ( theta, phi )

%*****************************************************************************80
%
%% tp_to_xyz() converts spherical TP coordinates to XYZ coordinates.
%
%  Discussion:
%
%    The point is assume to lie on the unit sphere centered at the origin.
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
%    real THETA, PHI, the angular coordinates of a point
%    on the unit sphere.
%
%  Output:
%
%    real V(3,1), the XYZ coordinates.
%
  v = zeros ( 3, 1 );

  v(1) = cos ( theta ) * sin ( phi );
  v(2) = sin ( theta ) * sin ( phi );
  v(3) =                 cos ( phi );

  return
end
