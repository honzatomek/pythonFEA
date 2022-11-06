function x = uniform_in_sector ( n, r, t1, t2 )

%*****************************************************************************80
%
%% uniform_in_sector() maps uniform points into a circular sector.
%
%  Discussion:
%
%    The sector lies between circles with center at 0 and radius 0 and R,
%    and between rays from the center at the angles T1 and T2.
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
%    real R, the outer radius.
%
%    real T1, T2, the two angles, which should
%    be measured in radians, with T1 < T2.
%
%  Output:
%
%    real X(N,2), the points.
%
  x = zeros ( n, 2 );

  u = rand ( 1, n );
  v = rand ( 1, n );

  t(1:n) = ( 1.0 - u(1:n) ) * t1 + u(1:n) * t2;
  r2(1:n) = r * sqrt (  v(1:n) );

  x(1:n,1) = r2(1:n) .* cos ( t(1:n) );
  x(1:n,2) = r2(1:n) .* sin ( t(1:n) );

  return
end
