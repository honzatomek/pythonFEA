function sphere01_area_nd_test ( )

%*****************************************************************************80
%
%% sphere01_area_nd_test() tests sphere01_area_nd().
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
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere01_area_nd_test():\n' );
  fprintf ( 1, '  sphere01_area_nd() evaluates the area of the unit\n' );
  fprintf ( 1, '  sphere in N dimensions.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '     DIM_NUM    Exact          Computed\n' );
  fprintf ( 1, '             Area           Area\n' );
  fprintf ( 1, '\n' );

  n_data = 0;

  while ( true )

    [ n_data, dim_num, area ] = sphere01_area_values ( n_data );

    if ( n_data == 0 )
      break
    end

    area2 = sphere01_area_nd ( dim_num );

    fprintf ( 1, '  %6d  %10f  %10f\n', dim_num, area, area2 );

  end

  return
end
