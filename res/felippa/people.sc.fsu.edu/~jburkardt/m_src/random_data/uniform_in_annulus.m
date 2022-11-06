function p = uniform_in_annulus ( n, pc, r1, r2 )

%*****************************************************************************80
%
%% uniform_in_annulus() samples a circular annulus.
%
%  Discussion:
%
%    A circular annulus with center PC, inner radius R1 and
%    outer radius R2, is the set of points P so that
%
%      R1^2 <= (P(1)-PC(1))^2 + (P(2)-PC(2))^2 <= R2^2
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
%    integer N, the number of points to generate.
%
%    real PC(2), the center.
%
%    real R1, R2, the inner and outer radii.
%
%  Output:
%
%    real P(N,2), sample points.
%
  p = zeros ( n, 2 );

  u = rand ( 1, n );

  theta(1:n) = u(1:n) * 2.0 * pi;

  v = rand ( 1, n );

  r(1:n) = sqrt ( ( 1.0 - v(1:n) ) * r1 * r1 ...
       +                  v(1:n)   * r2 * r2 );

  p(1:n,1) = pc(1) + r(1:n) .* cos ( theta(1:n) );
  p(1:n,2) = pc(2) + r(1:n) .* sin ( theta(1:n) );

  return
end
