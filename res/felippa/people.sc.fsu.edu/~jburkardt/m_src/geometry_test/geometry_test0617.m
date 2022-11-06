function geometry_test0617 ( )

%*****************************************************************************80
%
%% geometry_test0617() tests plane_normal_tetrahedron_intersect().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 June 2010
%
%  Author:
%
%    John Burkardt
%
  t = [ ...
    0.0, 0.0, 0.0 ;...
    1.0, 0.0, 0.0 ;...
    0.0, 1.0, 0.0 ;...
    0.0, 0.0, 1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'geometry_test0617():\n' );
  fprintf ( 1, '  plane_normal_tetrahedron_intersect() determines\n' );
  fprintf ( 1, '  the intersection of a plane and tetrahedron.\n' );

  for k = 1 : 2

    if ( k == 1 )
      normal = [ 0.0, 0.0, 1.0 ]';
    else
      normal = [ 1.0, 1.0, 0.0 ]' / sqrt ( 2.0 );
    end

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Plane normal vector number %d\n', k );
    fprintf ( 1, '\n' );
    fprintf ( 1, '  %14f  %14f  %14f\n', normal(1:3) );

    for i = 0 : 6

      pp(1:3) = normal(1:3) * i / 5.0;

      [ int_num, pint ] = plane_normal_tetrahedron_intersect ( pp, normal, t );

      fprintf ( 1, '\n' );
      fprintf ( 1, '  Point on plane:\n' );
      fprintf ( 1, '\n' );
      fprintf ( 1, '  %14f  %14f  %14f\n', pp(1:3) );
      fprintf ( 1, '\n' );
      fprintf ( 1, '  Number of intersection points = %d\n', int_num );
      fprintf ( 1, '\n' );
      for j = 1 : int_num
        fprintf ( 1, '  %4d  %14f  %14f  %14f\n', j, pint(1:3,j) );
      end

    end

  end

  return
end
