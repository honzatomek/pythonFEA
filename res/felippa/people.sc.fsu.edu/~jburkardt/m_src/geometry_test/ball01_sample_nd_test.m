function ball01_sample_nd_test ( )

%*****************************************************************************80
%
%% ball01_sample_nd_test() tests ball01_sample_nd().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 January 2021
%
%  Author:
%
%    John Burkardt
%
  dim_num = 3;
  n_sample = 1000;
 
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ball01_sample_nd_test():\n' );
  fprintf ( 1, '  ball_unit_sample_nd() samples the unit ball in N dimensions;\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  A few sample values:\n' );
  fprintf ( 1, '\n' );

  for i = 1 : 5
    x = ball01_sample_nd ( dim_num );
    fprintf ( 1, '  %10f  %10f  %10f\n', x(1:dim_num) );
  end

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of sample points = %d\n', n_sample );

  average(1:dim_num) = 0.0;

  for i = 1 : n_sample
    x = ball01_sample_nd ( dim_num );
    average(1:dim_num) = average(1:dim_num) + x(1:dim_num);
  end

  average(1:dim_num) = average(1:dim_num) / n_sample;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Now average the points, which should get a value\n' );
  fprintf ( 1, '  close to zero, and closer as N increases.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Average:        %10f  %10f  %10f\n', average(1:dim_num) );

  average_r = 0.0;

  for i = 1 : n_sample
    x = ball01_sample_nd ( dim_num );
    r = sqrt ( sum ( x(1:dim_num).^2 ) );
    average_r = average_r + r;
  end

  average_r = average_r / n_sample;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Now average the distance of the points from\n' );
  fprintf ( 1, '  the center, which should be the \n' );
  fprintf ( 1, '  1/2^(1/dim_num) = %f\n', 0.5^( 1.0 / dim_num ) );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Average:        %10f\n', average_r );

  average_theta = 0.0;

  for i = 1 : n_sample
    x = ball01_sample_nd ( dim_num );
    theta = atan2 ( x(2), x(1) );
    average_theta = average_theta + theta;
  end

  average_theta = average_theta / n_sample;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Now average the angle THETA, which should be PI.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Average:        %f\n', average_theta );

  average_phi = 0.0;

  for i = 1 : n_sample
    x = ball01_sample_nd ( dim_num );
    r = sqrt ( sum ( x(1:dim_num).^2 ) );
    phi = acos ( x(3) / r );
    average_phi = average_phi + phi;
  end

  average_phi = average_phi / n_sample;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Now average the angle PHI, which should be PI/2.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Average:        %f\n', average_phi );

  return
end
