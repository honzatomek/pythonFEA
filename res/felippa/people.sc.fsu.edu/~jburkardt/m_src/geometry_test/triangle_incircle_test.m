function triangle_incircle_test ( )

%*****************************************************************************80
%
%% triangle_incircle_test() tests triangle_incircle();
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
  dim_num = 2;

  t = [ ...
    0.0, 1.0; ...
    0.0, 0.0; ...
    1.0, 0.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_incircle_test():\n' );
  fprintf ( 1, '  triangle_incircle() computes the incircle of a triangle.\n' );

  r8mat_transpose_print ( dim_num, 3, t, '  Triangle vertices:' );

  [ r, center ] = triangle_incircle ( t );

  r8vec_print ( dim_num, center, '  Incenter' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Incircle radius is %f\n', r );

  return
end
