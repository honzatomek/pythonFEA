function x = sphere_unit_sample ( n )

%*****************************************************************************80
%
%% sphere_unit_sample() picks a random point on the unit sphere in 3D.
%
%  Discussion:
%
%    The unit sphere in 3D satisfies:
%
%      X * X + Y * Y + Z * Z = 1
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points to compute.
%
%  Output:
%
%    real X(3,N), the sample point.
%
  for j = 1 : n
%
%  Pick a uniformly random VDOT, which must be between -1 and 1.
%  This represents the dot product of the random vector with the Z unit vector.
%
%  Note: this works because the surface area of the sphere between
%  Z and Z + dZ is independent of Z.  So choosing Z uniformly chooses
%  a patch of area uniformly.
%
    vdot = -1.0 + 2.0 * rand ( 1, 1 );

    phi = acos ( vdot );
%
%  Pick a uniformly random rotation between 0 and 2 Pi around the
%  axis of the Z vector.
%
    theta = 2.0 * pi * rand ( 1, 1 );

    x(1,j) = cos ( theta ) * sin ( phi );
    x(2,j) = sin ( theta ) * sin ( phi );
    x(3,j) = cos ( phi );

  end

  return
end
 
