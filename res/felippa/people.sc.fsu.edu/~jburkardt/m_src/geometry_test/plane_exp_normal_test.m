function plane_exp_normal_test ( )

%*****************************************************************************80
%
%% plane_exp_normal_test() tests plane_exp_normal().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 April 2009
%
%  Author:
%
%    John Burkardt
%
  dim_num = 3;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'plane_exp_normal_test()\n' );
  fprintf ( 1, '  plane_exp_normal() finds the normal to a plane.\n' );

  p1(1:dim_num) = [ -10.56, -10.56, 78.09 ];
  p2(1:dim_num) = [  44.66, -65.77,  0.00 ];
  p3(1:dim_num) = [  44.66,  44.66,  0.00 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Three points on the plane:\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  P1: %10f  %10f  %10f\n', p1(1:dim_num) );
  fprintf ( 1, '  P2: %10f  %10f  %10f\n', p2(1:dim_num) );
  fprintf ( 1, '  P3: %10f  %10f  %10f\n', p3(1:dim_num) );

  normal = plane_exp_normal ( p1, p2, p3 );

  r8vec_print ( dim_num, normal, '  The normal vector:' );

  return
end
