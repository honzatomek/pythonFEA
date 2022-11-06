function sphere_dia2imp_3d_test ( )

%*****************************************************************************80
%
%% sphere_dia2imp_3d_test() tests sphere_dia2imp_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 January 2019
%
%  Author:
%
%    John Burkardt
%
  p1 = [ -1.0, -1.0, 4.0 ];
  p2 = [  5.0,  7.0, 4.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_dia2imp_3d_test():\n' );
  fprintf ( 1, '  sphere_dia2imp_3d() converts a sphere from\n' );
  fprintf ( 1, '  diameter to implicit form.\n' );

  r8vec_print ( 3, p1, '  Point P1:' );
  r8vec_print ( 3, p2, '  Point P2:' );

  [ r, center ] = sphere_dia2imp_3d ( p1, p2 );

  fprintf ( 1, '\n' );
  fprintf ( 1, '    Radius: %f\n', r );
 
  r8vec_print ( 2, center, '  The center:' );

  return
end
