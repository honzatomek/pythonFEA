function polygon_angles_test ( )

%*****************************************************************************80
%
%% polygon_angles_test() tests polygon_angles().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 February 2009
%
%  Author:
%
%    John Burkardt
%
  dim_num = 2;
  n = 6;

  v = [ ...
    0.0, 0.0; ...
    1.0, 0.0; ...
    2.0, 1.0; ...
    3.0, 0.0; ...
    3.0, 2.0; ...
    1.0, 2.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_angles_test():\n' );
  fprintf ( 1, '  polygon_angles() computes the angles of a polygon.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of polygonal vertices = %d\n', n );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  angle = polygon_angles ( n, v );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Polygonal angles in degrees:\n' );
  fprintf ( 1, '\n' );

  for i = 1 : n
    fprintf ( 1, '  %6d  %14f\n', i, ( angle(i) * 180.0 / pi ) );
  end

  return
end
