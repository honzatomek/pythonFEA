function sphere_imp_area_nd_test ( )

%*****************************************************************************80
%
%% sphere_imp_area_nd_test() tests sphere_imp_area_nd().
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
  r = 1.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_imp_area_nd_test\n' );
  fprintf ( 1, '  sphere_imp_area_nd() computes the area of an implicit sphere\n' );
  fprintf ( 1, '  in N dimensions;\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  We use a radius of R = %f\n', r );
  fprintf ( 1, '\n' );
  fprintf ( 1, '    DIM_NUM Area\n' );
  fprintf ( 1, '\n' );

  for dim_num = 2 : 10
    area = sphere_imp_area_nd ( dim_num, r );
    fprintf ( 1, '  %3d  %12f\n', dim_num, area );
  end

  return
end
