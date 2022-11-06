function x = uniform_on_triangle ( n, v )

%*****************************************************************************80
%
%% uniform_on_triangle() maps uniform points onto the boundary of a triangle.
%
%  Discussion:
%
%    The triangle is defined by the three vertices V1, V2, V3.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 May 2013
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points.
%
%    real V(3,2), the vertices of the triangle.
%
%  Output:
%
%    real X(N,2), the points.
%
  x = zeros ( n, 2 );

  l1 = sqrt ( ( v(2,1) - v(1,1) ) ^ 2 ...
            + ( v(2,2) - v(1,2) ) ^ 2 );

  l2 = sqrt ( ( v(3,1) - v(2,1) ) ^ 2 ...
            + ( v(3,2) - v(2,2) ) ^ 2 );

  l3 = sqrt ( ( v(1,1) - v(3,1) ) ^ 2 ...
            + ( v(1,2) - v(3,2) ) ^ 2 );
  
  for i = 1 : n
%
%  R can be regarded as the distance of the point on the perimeter,
%  as measured from the origin, along the perimeter.
%
    r = rand ( 1, 1 );

    r = ( l1 + l2 + l3 ) * r;
%
%  Case 1: between V1 and V2.
%
    if ( r <= l1 )

      s = ( l1 - r ) / l1;
      t =        r   / l1;
      x(i,1) = s * v(1,1) + t * v(2,1);
      x(i,2) = s * v(1,2) + t * v(2,2);
%
%  Case 2: between V2 and V3.
%
    elseif ( r <= l1 + l2 )

      s = ( l2 - r + l1 ) / l2;
      t = (      r - l1 ) / l2;
      x(i,1) = s * v(2,1) + t * v(3,1);
      x(i,2) = s * v(2,2) + t * v(3,2);
%
%  Case 3: between V3 and V1.
%
    else

      s = ( l3 - r + l1 + l2 ) / l3;
      t = (      r - l1 - l2 ) / l3;
      x(i,1) = s * v(3,1) + t * v(1,1);
      x(i,2) = s * v(3,2) + t * v(1,2);

    end

  end

  return
end
