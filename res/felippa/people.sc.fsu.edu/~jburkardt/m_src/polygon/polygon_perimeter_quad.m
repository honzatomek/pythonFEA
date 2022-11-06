function value = polygon_perimeter_quad ( n, v, hmax, f )

%*****************************************************************************80
%
%% polygon_perimeter_quad() estimates an integral over the perimeter of a polygon.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    20 October 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of vertices of the polygon.
%
%    real V(2,N), the vertices.
%
%    real HMAX, the maximum length of a quadrature interval.
%
%    function result = F ( X, Y ), a function whose integral 
%    over the perimeter is desired.
%
%  Output:
%
%    real VALUE, the estimated integral.
%
  value = 0.0;

  for i = 1 : n

    ip1 = i4_wrap ( i + 1, 1, n );
    l = sqrt ( ( v(1,ip1) - v(1,i) ) ^ 2 + ( v(2,ip1) - v(2,i) ) ^ 2 );
    m = ceil ( l / hmax );
    dxy = l / m;

    for j = 1 : 2 : 2 * m - 1
      x = ( ( 2 * m - j ) * v(1,i) ...
          + (         j ) * v(1,ip1) ) ... 
          / ( 2 * m     );
      y = ( ( 2 * m - j ) * v(2,i) ...
          + (         j ) * v(2,ip1) ) ...
          / ( 2 * m     );
      fxy = f ( x, y );
      value = value + fxy * dxy;
    end

  end

  return
end
