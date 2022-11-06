function polygon_is_convex_test ()

%*****************************************************************************80
%
%% polygon_is_convex_test() tests polygon_is_convex().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 April 2022
%
%  Author:
%
%    John Burkardt
%
  dim_num = 2;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_is_convex_test():\n' );
  fprintf ( 1, '  polygon_is_convex() determines if a polygon\n' );
  fprintf ( 1, '  is convex.\n' );
%
%  Shape 1: a point
%
  n = 1;

  v(1:dim_num,1:n) = [ ...
    0.0, 0.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  result = polygon_is_convex ( n, v );

  fprintf ( 1, '\n' );
  if ( result == -1 )
    fprintf ( 1, '  The polygon is not convex.\n' );
  elseif ( result == 0 )
    fprintf ( 1, '  The polygon is degenerate and convex.\n' );
  elseif ( result == 1 )
    fprintf ( 1, '  The polygon is convex and counterclockwise.\n' );
  elseif ( result == 2 )
    fprintf ( 1, '  The polygon is convex and clockwise.\n' );
  end
%
%  Shape 2: a line
%
  n = 2;

  v(1:dim_num,1:n) = [ ...
    0.0, 0.0; ...
    1.0, 2.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  result = polygon_is_convex ( n, v );

  fprintf ( 1, '\n' );
  if ( result == -1 )
    fprintf ( 1, '  The polygon is not convex.\n' );
  elseif ( result == 0 )
    fprintf ( 1, '  The polygon is degenerate and convex.\n' );
  elseif ( result == 1 )
    fprintf ( 1, '  The polygon is convex and counterclockwise.\n' );
  elseif ( result == 2 )
    fprintf ( 1, '  The polygon is convex and clockwise.\n' );
  end
%
%  Shape 3: a flat "triangle."
%
  n = 3;

  v(1:dim_num,1:n) = [ ...
    0.0, 0.0; ...
    2.0, 0.0; ...
    1.0, 0.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  result = polygon_is_convex ( n, v );

  fprintf ( 1, '\n' );
  if ( result == -1 )
    fprintf ( 1, '  The polygon is not convex.\n' );
  elseif ( result == 0 )
    fprintf ( 1, '  The polygon is degenerate and convex.\n' );
  elseif ( result == 1 )
    fprintf ( 1, '  The polygon is convex and counterclockwise.\n' );
  elseif ( result == 2 )
    fprintf ( 1, '  The polygon is convex and clockwise.\n' );
  end
%
%  Shape 4: a triangle.
%
  n = 3;

  v(1:dim_num,1:n) = [ ...
    0.0, 0.0; ...
    1.0, 0.0; ...
    0.0, 2.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  result = polygon_is_convex ( n, v );

  fprintf ( 1, '\n' );
  if ( result == -1 )
    fprintf ( 1, '  The polygon is not convex.\n' );
  elseif ( result == 0 )
    fprintf ( 1, '  The polygon is degenerate and convex.\n' );
  elseif ( result == 1 )
    fprintf ( 1, '  The polygon is convex and counterclockwise.\n' );
  elseif ( result == 2 )
    fprintf ( 1, '  The polygon is convex and clockwise.\n' );
  end
%
%  Shape 5: CW triangle.
%
  n = 3;

  v(1:dim_num,1:n) = [ ...
    0.0, 0.0; ...
    0.0, 2.0; ...
    1.0, 0.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  result = polygon_is_convex ( n, v );

  fprintf ( 1, '\n' );
  if ( result == -1 )
    fprintf ( 1, '  The polygon is not convex.\n' );
  elseif ( result == 0 )
    fprintf ( 1, '  The polygon is degenerate and convex.\n' );
  elseif ( result == 1 )
    fprintf ( 1, '  The polygon is convex and counterclockwise.\n' );
  elseif ( result == 2 )
    fprintf ( 1, '  The polygon is convex and clockwise.\n' );
  end
%
%  Shape 6: polygon with an interior angle of more than 90.
%
  n = 4;

  v(1:dim_num,1:n) = [ ...
    1.0, 0.0; ...
    2.0, 0.0; ...
    3.0, 1.0; ...
    0.0, 1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  result = polygon_is_convex ( n, v );

  fprintf ( 1, '\n' );
  if ( result == -1 )
    fprintf ( 1, '  The polygon is not convex.\n' );
  elseif ( result == 0 )
    fprintf ( 1, '  The polygon is degenerate and convex.\n' );
  elseif ( result == 1 )
    fprintf ( 1, '  The polygon is convex and counterclockwise.\n' );
  elseif ( result == 2 )
    fprintf ( 1, '  The polygon is convex and clockwise.\n' );
  end
%
%  Shape 7: polygon with an interior angle of more than 180.
%
  n = 5;

  v(1:dim_num,1:n) = [ ...
    0.0, 0.0; ...
    0.5, 0.5; ...
    1.0, 0.0; ...
    1.0, 1.0; ...
    0.0, 1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  result = polygon_is_convex ( n, v );

  fprintf ( 1, '\n' );
  if ( result == -1 )
    fprintf ( 1, '  The polygon is not convex.\n' );
  elseif ( result == 0 )
    fprintf ( 1, '  The polygon is degenerate and convex.\n' );
  elseif ( result == 1 )
    fprintf ( 1, '  The polygon is convex and counterclockwise.\n' );
  elseif ( result == 2 )
    fprintf ( 1, '  The polygon is convex and clockwise.\n' );
  end
%
%  Shape 8: star
%
  n = 5;

  for i = 1 : n
    angle = ( i - 1 ) * 4.0 * pi / n;
    v(1,i) = cos ( angle );
    v(2,i) = sin ( angle );
  end

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  result = polygon_is_convex ( n, v );

  fprintf ( 1, '\n' );
  if ( result == -1 )
    fprintf ( 1, '  The polygon is not convex.\n' );
  elseif ( result == 0 )
    fprintf ( 1, '  The polygon is degenerate and convex.\n' );
  elseif ( result == 1 )
    fprintf ( 1, '  The polygon is convex and counterclockwise.\n' );
  elseif ( result == 2 )
    fprintf ( 1, '  The polygon is convex and clockwise.\n' );
  end
%
%  Shape 9: regular hexagon
%
  n = 6;

  for i = 0 : n-1
    angle = ( i ) * 2.0 * pi / n;
    v(1,i+1) = cos ( angle );
    v(2,i+1) = sin ( angle );
  end

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  result = polygon_is_convex ( n, v );

  fprintf ( 1, '\n' );
  if ( result == -1 )
    fprintf ( 1, '  The polygon is not convex.\n' );
  elseif ( result == 0 )
    fprintf ( 1, '  The polygon is degenerate and convex.\n' );
  elseif ( result == 1 )
    fprintf ( 1, '  The polygon is convex and counterclockwise.\n' );
  elseif ( result == 2 )
    fprintf ( 1, '  The polygon is convex and clockwise.\n' );
  end
%
%  Shape 10: double triangle
%
  n = 6;

  v(1:dim_num,1:n) = [ ...
    0.0, 0.0; ...
    2.0, 0.0; ...
    1.0, 1.0; ...
    0.0, 0.0; ...
    2.0, 0.0; ...
    1.0, 1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  result = polygon_is_convex ( n, v );

  fprintf ( 1, '\n' );
  if ( result == -1 )
    fprintf ( 1, '  The polygon is not convex.\n' );
  elseif ( result == 0 )
    fprintf ( 1, '  The polygon is degenerate and convex.\n' );
  elseif ( result == 1 )
    fprintf ( 1, '  The polygon is convex and counterclockwise.\n' );
  elseif ( result == 2 )
    fprintf ( 1, '  The polygon is convex and clockwise.\n' );
  end
%
%  Shape 11: "square knot"
%
  n = 8;
  v(1:dim_num,1:n) = [ ...
    1.0, 0.0; ...
    3.0, 0.0; ...
    3.0, 3.0; ...
    0.0, 3.0; ...
    0.0, 1.0; ...
    2.0, 1.0; ...
    2.0, 2.0; ...
    1.0, 2.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  result = polygon_is_convex ( n, v );

  fprintf ( 1, '\n' );
  if ( result == -1 )
    fprintf ( 1, '  The polygon is not convex.\n' );
  elseif ( result == 0 )
    fprintf ( 1, '  The polygon is degenerate and convex.\n' );
  elseif ( result == 1 )
    fprintf ( 1, '  The polygon is convex and counterclockwise.\n' );
  elseif ( result == 2 )
    fprintf ( 1, '  The polygon is convex and clockwise.\n' );
  end

  return
end
