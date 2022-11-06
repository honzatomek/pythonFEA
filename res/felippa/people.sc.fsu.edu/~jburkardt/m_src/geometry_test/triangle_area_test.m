function triangle_area_test ( )

%*****************************************************************************80
%
%% triangle_area_test() tests triangle_area();
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 May 2005
%
%  Author:
%
%    John Burkardt
%
  t = [ ...
    0.0, 1.0; ...
    0.0, 0.0; ...
    1.0, 0.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_area_test():\n' );
  fprintf ( 1, '  triangle_area() computes the area of a triangle.\n' );

  r8mat_print ( 2, 3, t, '  Triangle vertices (columns)' );

  area = triangle_area ( t );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Triangle area is %f\n', area );

  return
end
