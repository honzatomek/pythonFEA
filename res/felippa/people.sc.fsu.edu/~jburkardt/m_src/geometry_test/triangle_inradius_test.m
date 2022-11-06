function triangle_inradius_test ( )

%*****************************************************************************80
%
%% triangle_inradius_test() tests triangle_inradius();
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

  t = [ ...
    0.0, 1.0; ...
    0.0, 0.0; ...
    1.0, 0.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_inradius_test():\n' );
  fprintf ( 1, '  triangle_inradius() computes the inradius of a triangle.\n' );

  r8mat_transpose_print ( dim_num, 3, t, '  Triangle vertices:' );

  r = triangle_inradius ( t );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Incircle radius is %f\n', r );

  return
end
