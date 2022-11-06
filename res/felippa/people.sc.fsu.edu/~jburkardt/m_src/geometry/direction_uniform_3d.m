function vran = direction_uniform_3d ( )

%*****************************************************************************80
%
%% direction_uniform_3d() picks a random direction vector in 3D.
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
%    real VRAN(3), the random direction vector, with unit norm.
%
  dim_num = 3;
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

  vran(1) = cos ( theta ) * sin ( phi );
  vran(2) = sin ( theta ) * sin ( phi );
  vran(3) = cos ( phi );

  return
end
