function x = uniform_on_hemisphere_phong ( n, m )

%*****************************************************************************80
%
%% uniform_on_hemisphere_phong() maps uniform points onto the unit hemisphere.
%
%  Discussion:
%
%    The sphere has center 0 and radius 1.
%
%    The Phong density is used, with exponent M:
%
%    rho ( theta, phi; m ) = ( m + 1 ) * cos ( phi )^M / ( 2 * pi )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    11 April 2022
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
%    integer M, the Phong exponent.
%
%  Output:
%
%    real X(N,3), the points.
%
  x = zeros ( n, 3 );

  power = 1.0 / ( m + 1 );
  phi = rand ( n, 1 );
  phi(1:n,1) = acos ( ( 1.0 - phi(1:n,1) ).^power );

  theta = rand ( n, 1 );
  theta(1:n,1) = 2.0 * pi * theta(1:n,1);

  x(1:n,1) = cos ( theta ) .* sin ( phi );
  x(1:n,2) = sin ( theta ) .* sin ( phi );
  x(1:n,3) =                  cos ( phi );

  return
end
