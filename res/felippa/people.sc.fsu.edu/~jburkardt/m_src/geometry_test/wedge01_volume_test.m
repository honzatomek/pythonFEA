function wedge01_volume_test ( )

%*****************************************************************************80
%
%% wedge01_volume_test() tests wedge01_volume().
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
  fprintf ( 1, 'wedge01_volume_test():\n' );
  fprintf ( 1, '  wedge01_volume() returns the volume of the unit wedge.\n' );

  volume = wedge01_volume ( );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Unit wedge volume = %g\n', volume );

  return
end
