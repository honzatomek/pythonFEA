function pyramid_volume_3d_test ( )

%*****************************************************************************80
%
%% pyramid_volume_3d_test() tests pyramid_volume_3d().
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
  fprintf ( 1, 'pyramid_volume_3d_test():\n' );
  fprintf ( 1, '  pyramid_volume_3d() returns the volume of a pyramid.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '     Radius     Height     Volume\n' );
  fprintf ( 1, '\n' );

  for i = 1 : 5
    r = 1.0 + 9.0 * rand ( 1, 1 );
    h = 1.0 + 9.0 * rand ( 1, 1 );
    volume = pyramid_volume_3d ( r, h );
    fprintf ( 1, '  %8.4f  %8.4f  %8.4f\n', r, h, volume );
  end

  return
end
