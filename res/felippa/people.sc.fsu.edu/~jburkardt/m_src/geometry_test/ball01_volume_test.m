function ball01_volume_test ( )

%*****************************************************************************80
%
%% ball01_volume_test() tests ball01_volume().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 January 2018
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ball01_volume_test():\n' );
  fprintf ( 1, '  ball01_volume() returns the volume of the unit ball.\n' );

  volume = ball01_volume ( );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Volume = %g\n', volume );

  return
end
