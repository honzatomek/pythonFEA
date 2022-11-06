function line_exp_normal_test ( )

%*****************************************************************************80
%
%% line_exp_normal_test() tests line_exp_normal().
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
  ntest = 3;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'line_exp_normal_test():\n' );
  fprintf ( 1, '  line_exp_normal() determines a unit normal vector\n' );
  fprintf ( 1, '  to a given explicit line.\n' );

  p1(1:2,1) = [ 1.0; 3.0 ];
  p2(1:2,1) = [ 4.0; 0.0 ];

  r8vec_print ( 2, p1, '  Point 1: ' );
  r8vec_print ( 2, p2, '  Point 2: ' );

  normal = line_exp_normal ( p1, p2 );

  r8vec_print ( 2, normal, '  Normal vector N:' );

  return
end
