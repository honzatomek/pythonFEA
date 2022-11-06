function cylinder_sample_test ( )

%*****************************************************************************80
%
%% cylinder_sample_test() tests cylinder_sample().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 December 2010
%
%  Author:
%
%    John Burkardt
%
  n = 20;

  p1 = [ 0.0; -2.0; 0.0 ];
  p2 = [ 0.0;  2.0; 0.0 ];
  r = 1.0;
 
  fprintf ( 1, '\n' );
  fprintf ( 1, 'cylinder_sample_test():\n' );
  fprintf ( 1, '  cylinder_sample() samples points in a cylinder.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Radius R = %f\n', r );
  fprintf ( 1, '  Center of bottom disk = %f  %f  %f\n', p1(1:3,1) );
  fprintf ( 1, '  Center of top disk =    %f  %f  %f\n', p2(1:3,1) );

  p = cylinder_sample ( p1, p2, r, n );

  r8mat_transpose_print ( 3, n, p, '  Sample points:' );

  return
end
