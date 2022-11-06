function x = sphere01_sample_3d ( )

%*****************************************************************************80
%
%% sphere01_sample_3d() picks a random point on the unit sphere in 3D.
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
%  Output:
%
%    real X(3), the sample point.
%

%
%  Pick a uniformly random VDOT, which must be between -1 and 1.
%  This represents the dot product of the random vector with the Z unit vector.
%
%  Note: this works because the surface area of the sphere between
%  Z and Z + dZ is independent of Z.  So choosing Z uniformly chooses
%  a patch of area uniformly.
%
  vdot = - 1.0 + 2.0 * rand ( 1, 1 );

  phi = acos ( vdot );
%
%  Pick a uniformly random rotation between 0 and 2 Pi around the
%  axis of the Z vector.
%
  theta = 2.0 * pi * rand ( 1, 1 );

  x(1) = cos ( theta ) * sin ( phi );
  x(2) = sin ( theta ) * sin ( phi );
  x(3) = cos ( phi );

  return
end
