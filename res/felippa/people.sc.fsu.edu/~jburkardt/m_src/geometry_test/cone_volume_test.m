function cone_volume_test ( )

%*****************************************************************************80
%
%% cone_volume_test() tests cone_volume().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 August 2018
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'cone_volume_test():\n' );
  fprintf ( 1, '  cone_volume() computes the volume of a cone.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '        R        H        ConeVolume\n' );
  fprintf ( 1, '\n' );

  r = 1.0;
  h = 1.0;
  for i = 1 : 5
    fprintf ( 1, '  %14.8f  %14.8f  %14.8f\n', r, h, cone_volume ( r, h ) );
    h = h * 2.0;
  end

  fprintf ( 1, '\n' );

  r = 1.0;
  h = 1.0;
  for i = 1 : 5
    fprintf ( 1, '  %14.8f  %14.8f  %14.8f\n', r, h, cone_volume ( r, h ) );
    r = r * 2.0;
  end

  return
end
