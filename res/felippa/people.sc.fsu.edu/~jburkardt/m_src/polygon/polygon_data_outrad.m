function [ area, radin, side ] = polygon_data_outrad ( n, radout )

%*****************************************************************************80
%
%% polygon_data_outrad() determines polygonal data from its outer radius in 2D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 September 2003
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
%    real RADOUT, the outer radius of the polygon, that is,
%    the radius of the smallest circle that can be described
%    around the polygon.
%
%  Output:
%
%    real AREA, the area of the regular polygon.
%
%    real RADIN, the inner radius of the polygon, that is,
%    the radius of the largest circle that can be inscribed
%    within the polygon.
%
%    real SIDE, the length of one side of the polygon.
%
  if ( n < 3 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'polygon_data_outrad(): Fatal error!\n' );
    fprintf ( 1, '  Input value of N must be at least 3.\n' );
    fprintf ( 1, '  but your input value was N = %d\n', n );
    error ( 'polygon_data_outrad(): Fatal error!' );
  end

  angle = pi / n;
  area = 0.5 * n * radout * radout * sin ( 2.0 * angle );
  side = 2.0 * radout * sin ( angle );
  radin = 0.5 * side / tan ( angle );

  return
end
