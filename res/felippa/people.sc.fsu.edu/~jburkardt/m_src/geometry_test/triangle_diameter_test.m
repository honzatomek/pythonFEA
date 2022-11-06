function triangle_diameter_test ( )

%*****************************************************************************80
%
%% triangle_diameter_test() tests triangle_diameter().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  dim_num = 2;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_diameter_test():\n' );
  fprintf ( 1, '  triangle_diameter() computes the diameter of \n' );
  fprintf ( 1, '  the SMALLEST circle around the triangle.\n' );

  t(1:dim_num,1:3) = [ ...
    4.0, 2.0; ...
    1.0, 5.0; ...
   -2.0, 2.0 ]';

  r8mat_transpose_print ( dim_num, 3, t, '  Triangle vertices:' );

  diameter = triangle_diameter ( t );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Diameter = %f\n', diameter );

  t(1:dim_num,1:3) = [ ...
    4.0, 2.0; ...
    5.0, 4.0; ...
    6.0, 6.0 ]';

  r8mat_transpose_print ( dim_num, 3, t, '  Triangle vertices:' );

  diameter = triangle_diameter ( t );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Diameter = %f\n', diameter );

  t(1:dim_num,1:3) = [ ...
    4.0, 2.0; ...
    1.0, 5.0; ...
    4.0, 2.0 ]';

  r8mat_transpose_print ( dim_num, 3, t, '  Triangle vertices:' );

  diameter = triangle_diameter ( t );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Diameter = %f\n', diameter );

  return
end
