function radians_to_dms_test ( )

%*****************************************************************************80
%
%% radians_to_dms_test() tests radians_to_dms().
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
  fprintf ( 1, '\n' );
  fprintf ( 1, 'radians_to_dms_test():\n' );
  fprintf ( 1, '  radians_to_dms() converts an angle from radians\n' );
  fprintf ( 1, '  to degrees/minutes/seconds;\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Radians     DMS     Radians\n' );
  fprintf ( 1, '\n' );

  for i = -2 : 15
    angle_rad = pi * i / 7.0;
    [ angle_deg, angle_min, angle_sec ] = radians_to_dms ( angle_rad );
    angle_rad2 = dms_to_radians ( angle_deg, angle_min, angle_sec );
    fprintf ( 1, '  %10f  %4d  %3d  %3d  %10f\n', ...
      angle_rad, angle_deg, angle_min, angle_sec, angle_rad2 );
  end

  return
end
