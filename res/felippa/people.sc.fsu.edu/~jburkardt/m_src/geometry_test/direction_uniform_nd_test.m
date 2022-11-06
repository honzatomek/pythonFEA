function direction_uniform_nd_test ( )

%*****************************************************************************80
%
%% direction_uniform_nd_test() tests direction_uniform_nd().
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
  dim_num = 4;
 
  fprintf ( 1, '\n' );
  fprintf ( 1, 'direction_uniform_nd_test():\n' );
  fprintf ( 1, '  direction_uniform_nd() picks a random direction vector\n' );
  fprintf ( 1, '  in ND.\n' );
  fprintf ( 1, '\n' );

  for i = 1 : 10
    vran = direction_uniform_nd ( dim_num );
    fprintf ( 1, '  %10f  %10f  %10f  %10f\n', vran(1:dim_num) );
  end

  return
end
