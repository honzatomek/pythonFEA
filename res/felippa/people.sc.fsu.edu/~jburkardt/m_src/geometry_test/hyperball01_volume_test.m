function hyperball01_volume_test ( )

%*****************************************************************************80
%
%% hyperball01_volume_test() tests hyperball01_volume().
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
  fprintf ( 1, '\n' )
  fprintf ( 1, 'hyperball01_volume_test\n' )
  fprintf ( 1, '  MATLAB version\n' )
  fprintf ( 1, '  HYPERBALL01_VOLUME returns the volume of the unit hyperball\n' )
  fprintf ( 1, '  in M dimensions.\n' )
  fprintf ( 1, '\n' )
  fprintf ( 1, '   M  Volume\n' )
  fprintf ( 1, '\n' )

  for m = 1 : 10
    value = hyperball01_volume ( m );
    fprintf ( 1, '  %2d  %g\n', m, value );
  end
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hyperball01_volume_test\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end

