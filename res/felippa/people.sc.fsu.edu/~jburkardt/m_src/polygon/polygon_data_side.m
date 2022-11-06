function [ area, radin, radout ] = polygon_data_side ( n, side )

%*****************************************************************************80
%
%% polygon_data_side() determines polygonal data from its side length.
%
%  Discussion:
%
%    28 April 2022: the previous formula for area was too big by a factor or 2.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of sides of the polygon.
%    N must be at least 3.
%
%    real SIDE, the length of one side of the polygon.
%
%  Output:
%
%    real AREA, the area of the regular polygon.
%
%    real RADIN, the inner radius of the polygon, that is,
%    the radius of the largest circle that can be inscribed within
%    the polygon.
%
%    real RADOUT, the outer radius of the polygon, that is,
%    the radius of the smallest circle that can be described about
%    the polygon.
%
  if ( n < 3 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'polygon_data_side(): Fatal error!\n' );
    fprintf ( 1, '  Input value of N must be at least 3.\n' );
    fprintf ( 1, '  but your input value was N = ', n );
    error ( 'polygon_data_side(): Fatal error!' );
  end

  angle = 0.5 * pi / n;
  area = 0.5 * n * side * side / tan ( angle );
  radin = 0.5 * side / tan ( angle );
  radout = 0.5 * side / sin ( angle );

  return
end
