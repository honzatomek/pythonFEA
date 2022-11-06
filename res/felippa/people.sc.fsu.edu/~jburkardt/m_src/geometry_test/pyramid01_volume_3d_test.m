function pyramid01_volume_3d_test ( )

%*****************************************************************************80
%
%% pyramid01_volume_3d_test() tests pyramid01_volume_3d().
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
  fprintf ( 1, 'pyramid01_volume_3d_test():\n' );
  fprintf ( 1, '  pyramid01_volume_3d() returns the volume of the unit pyramid.\n' );

  value = pyramid01_volume_3d ( );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Volume = %g\n', value );

  return
end
