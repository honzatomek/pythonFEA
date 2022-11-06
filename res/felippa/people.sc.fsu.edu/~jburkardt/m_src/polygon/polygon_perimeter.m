function perimeter = polygon_perimeter ( n, v )

%*****************************************************************************80
%
%% polygon_perimeter() computes the perimeter of a polygon.
%
%  Discussion:
%
%    The perimeter is simply the sum of the side lengths.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    16 October 2015
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
%  Output:
%
%    real PERIMETER, the perimeter.
%
  perimeter = 0.0;

  im1 = n;

  for i = 1 : n
    l = sqrt ( ( v(1,im1) - v(1,i) ) ^ 2 + ( v(2,im1) - v(2,i) ) ^ 2 );
    perimeter = perimeter + l;
    im1 = i;
  end

  return
end
