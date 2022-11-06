function result = polygon_integral_x ( n, v )

%*****************************************************************************80
%
%% polygon_integral_x() integrates the function X over a polygon in 2D.
%
%  Discussion:
%
%    The polygon is bounded by the points (X(1:N), Y(1:N)).
%
%    INTEGRAL = (1/6) * sum ( 1 <= I <= N )
%      ( X(I)^2 + X(I) * X(I-1) + X(I-1)^2 ) * ( Y(I) - Y(I-1) )
%
%    where X(0) and Y(0) should be replaced by X(N) and Y(N).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 February 2005
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    S F Bockman,
%    Generalizing the Formula for Areas of Polygons to Moments,
%    American Mathematical Society Monthly,
%    1989, pages 131-132.
%
%  Input:
%
%    integer N, the number of vertices of the polygon.
%    N should be at least 3 for a nonzero result.
%
%    real V(2,N), the coordinates of the vertices
%    of the polygon.  These vertices should be given in counter-clockwise order.
%
%  Output:
%
%    real RESULT, the value of the integral.
%
  result = 0.0;

  if ( n < 3 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'polygon_integral_X(): Warning!\n' );
    fprintf ( 1, '  The number of vertices must be at least 3.\n' );
    fprintf ( 1, '  The input value of N = %d\n', n );
    error ( 'polygon_integral_x(): Warning!' );
    return
  end

  im1 = n;
  for i = 1 : n
    result = result + ( v(1,i).^2 + v(1,i) * v(1,im1) + v(1,im1).^2 ) ...
      * ( v(2,i) - v(2,im1) );
    im1 = i;
  end

  result = result / 6.0;

  return
end
