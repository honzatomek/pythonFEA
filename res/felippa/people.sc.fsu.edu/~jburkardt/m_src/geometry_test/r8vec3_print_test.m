function r8vec3_print_test ( )

%*****************************************************************************80
%
%% r8vec3_print_test() tests r8vec3_print().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 July 2018
%
%  Author:
%
%    John Burkardt
%
  n = 5;

  a = [ 1.0, 2.0, 3.0, 4.0, 5.0 ];
  b = a .^ 2;
  c = sqrt ( a );

  fprintf ( 1, '\n' );
  fprintf ( 1, 'r8vec3_print_test():\n' );
  fprintf ( 1, '  r8vec3_print() prints three R8VEC''s.\n' );

  r8vec3_print ( a, b, c, '  n, n^2, sqrt(n):' );

  return
end
