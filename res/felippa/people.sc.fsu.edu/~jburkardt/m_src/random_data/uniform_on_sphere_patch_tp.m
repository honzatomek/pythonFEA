function p = uniform_on_sphere_patch_tp ( n, phi1, phi2, theta1, theta2 )

%*****************************************************************************80
%
%% uniform_on_sphere_patch_tp() maps uniform points to a spherical TP patch.
%
%  Discussion:
%
%    The sphere has center 0 and radius 1.
%
%    A spherical TP patch on the surface of the unit sphere contains those 
%    points with radius R = 1 and angles (THETA,PHI) such that
%
%      0.0 <= THETA1 <= THETA <= THETA2 <= 2 * PI
%      0.0 <= PHI1   <= PHI   <= PHI2   <=     PI
%
%    Thus, given THETA and PHI, we have
%      x = cos ( theta ) * sin ( phi )
%      y = sin ( theta ) * sin ( phi )
%      z =                 cos ( phi )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    18 August 2010
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Peter Shirley,
%    Nonuniform Random Point Sets Via Warping,
%    Graphics Gems, Volume III,
%    edited by David Kirk,
%    AP Professional, 1992, 
%    ISBN: 0122861663,
%    LC: T385.G6973.
%
%  Input:
%
%    integer N, the number of points.
%
%    real PHI1, PHI2, the latitudinal angle range.
%
%    real THETA1, THETA2, the longitudinal angle range.
%
%  Output:
%
%    real P(N,3), the points, in (X,Y,Z) coordinates.
%
  r = rand ( n, 1 );
  theta = ( 1.0 - r ) * theta1 + r * theta2;

  r = rand ( n, 1 );
  phi = acos ( ( 1.0 - r ) * cos ( phi1 ) + r * cos ( phi2 ) );

  p = zeros ( n, 3 );
  p(1:n,1) = cos ( theta ) .* sin ( phi );
  p(1:n,2) = sin ( theta ) .* sin ( phi );
  p(1:n,3) =                  cos ( phi );

  return
end
