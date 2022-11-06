function sphere_imp_contains_point_3d_test ( )

%*****************************************************************************80
%
%% sphere_imp_contains_point_3d_test() tests sphere_imp_contains_point_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 May 2005
%
%  Author:
%
%    John Burkardt
%
  dim_num = 3;
  ntest = 4;

  r = 3.0;
  center(1:dim_num) = [ 1.0, 2.0, 3.0 ];
%
%  Set the test points.
%
  ptest(1:dim_num,1:ntest) = [ ...
    center(1),           center(2),           center(3); ...
    center(1) + 2.0 * r, center(2),           center(3); ...
    center(1),           center(2) + r,       center(3); ...
    center(1) + 0.5 * r, center(2) + 0.5 * r, center(3) + 0.5 * r ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_imp_contains_point_3d_test()\n' );
  fprintf ( 1, '  sphere_imp_contains_point_3d() determines if a\n' );
  fprintf ( 1, '  point is within an implicit sphere;\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '    Inside, P\n' );
  fprintf ( 1, '\n' );

  for i = 1 : ntest

    p(1:dim_num) = ptest(1:dim_num,i);

    inside = sphere_imp_contains_point_3d ( r, center, p );

    fprintf ( 1, '  %d  %12f  %12f  %12f\n', inside, p(1:dim_num) );

  end

  return
end

