function x = uniform_on_sphere_patch_xyz ( n, phi1, phi2, theta1, theta2 )

%*****************************************************************************80
%
%% uniform_on_sphere_patch_xyz() maps uniform points to a spherical XYZ patch.
%
%  Discussion:
%
%    The sphere has center 0 and radius 1.
%
%    A spherical XYZ patch on the surface of the unit sphere contains those 
%    points with radius R = 1 and angles (THETA,PHI) such that
%
%      0.0 <= THETA1 <= THETA <= THETA2 <= 2 * PI
%      0.0 <= PHI1   <= PHI   <= PHI2   <=     PI
%
%    mapped to
%
%      X = cos ( THETA ) * sin ( PHI )
%      Y = sin ( THETA ) * sin ( PHI )
%      Z =                 cos ( PHI )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    13 April 2022
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
%    real X(N,3), the points.
%
  x = zeros ( n, 3 );

  phi = rand ( n, 1 );

  phi(1:n) = acos ( ( 1.0 - phi(1:n) ) * cos ( phi1 ) ...
                  +         phi(1:n)   * cos ( phi2 ) );

  theta = rand ( n, 1 );

  theta(1:n) = ( 1.0 - theta(1:n) ) * theta1 ...
             +         theta(1:n)   * theta2;

  x(1:n,1) = cos ( theta(1:n) ) .* sin ( phi(1:n) );
  x(1:n,2) = sin ( theta(1:n) ) .* sin ( phi(1:n) );
  x(1:n,3) = cos ( phi(1:n) );

  return
end
