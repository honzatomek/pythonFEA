function inside = polygon_contains_point_2d ( n, v, p )

%*****************************************************************************80
%
%% polygon_contains_point_2d() finds if a point is inside a polygon.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 November 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of nodes or vertices in the polygon.
%    N must be at least 3.
%
%    real V(2,N), the coordinates of the vertices of the polygon.
%
%    real P(2,1), the coordinates of the point to be tested.
%
%  Output:
%
%    logical INSIDE, is TRUE if the point is inside the polygon.
%
  inside = 0;

  px1 = v(1,1);
  py1 = v(2,1);
  xints = p(1) - 1.0;

  for i = 1 : n

    px2 = v(1,mod(i,n)+1);
    py2 = v(2,mod(i,n)+1);

    if ( min ( py1, py2 ) < p(2) )
      if ( p(2) <= max ( py1, py2 ) )
        if ( p(1) <= max ( px1, px2 ) )
          if ( py1 ~= py2 )
            xints = ( p(2) - py1 ) * ( px2 - px1 ) / ( py2 - py1 ) + px1;
          end
          if ( px1 == px2 || p(1) <= xints )
            inside = ~ inside;
          end
        end
      end
    end

    px1 = px2;
    py1 = py2;

  end

  return
end

