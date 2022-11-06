function simplex01_volume_nd_test ( )

%*****************************************************************************80
%
%% simplex01_volume_nd_test() tests simplex01_volume_nd().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 July 2018
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'simplex01_volume_nd_test():\n' );
  fprintf ( 1, '  simplex01_volume_nd() gets volume of the unit M-dimensional simplex.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '   M    Volume\n' );
  fprintf ( 1, '\n' );

  for m = 1 : 10
    volume = simplex01_volume_nd ( m );
    fprintf ( 1, '  %2d  %12.8f\n', m, volume );
  end

  return
end
