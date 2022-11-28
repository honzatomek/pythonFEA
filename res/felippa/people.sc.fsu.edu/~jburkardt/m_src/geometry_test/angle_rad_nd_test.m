function angle_rad_nd_test ( )

%*****************************************************************************80
%
%% angle_rad_nd_test() tests angle_rad_nd().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 December 2010
%
%  Author:
%
%    John Burkardt
%
  n_angle = 12;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'angle_rad_nd_test():\n' );
  fprintf ( 1, '  angle_rad_nd() computes an angle.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  X, Y, Theta, atan2(y, x), ANGLE_RAD_ND\n' );
  fprintf ( 1, '\n' );

  v1(1:2,1) = [ 1.0; 0.0 ];
  v3(1:2,1) = [ 0.0; 0.0 ];

  for i = 0 : n_angle

    thetad = i * 360.0 / n_angle;
    thetar = degrees_to_radians ( thetad );

    x = cos ( thetar );
    y = sin ( thetar );

    v2(1:2,1) = [ x; y ];

    temp1 = radians_to_degrees ( atan2 ( y, x ) );

    temp2 = angle_rad_nd ( 2, v1, v2 );

    fprintf ( 1, '  %10f  %10f  %10f  %10f  %10f\n', ...
      x, y, thetad, temp1, temp2 );

  end

  return
end