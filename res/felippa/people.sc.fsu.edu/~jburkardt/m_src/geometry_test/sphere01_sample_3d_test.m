function sphere01_sample_3d_test ( )

%*****************************************************************************80
%
%% sphere01_sample_3d_test() tests sphere01_sample_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 May 2005
%
%  Author:
%
%    John Burkardt
%
  dim_num = 3;
  n_sample = 1000;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere01_sample_3d_test():\n' );
  fprintf ( 1, '  sphere01_sample_3d() samples the unit sphere in 3D;\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  A few sample values: \n' );
  fprintf ( 1, '\n' );

  for i = 1 : 5
    x = sphere01_sample_3d ( );
    fprintf ( 1, '  %10f  %10f  %10f\n', x(1:dim_num) );
  end

  average(1:dim_num) = 0.0;

  for i = 1 : n_sample
    x = sphere01_sample_3d ( );
    average(1:dim_num) = average(1:dim_num) + x(1:dim_num);
  end

  average(1:dim_num) = average(1:dim_num) / n_sample;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Now average the points, which should get a value\n' );
  fprintf ( 1, '  close to zero, and closer as N_SAMPLE increases.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Average:        %10f  %10f  %10f\n', average(1:dim_num) );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Now choose a random direction, sample the same\n' );
  fprintf ( 1, '  number of points, and compute the dot product with\n' );
  fprintf ( 1, '  the direction.\n' );
  fprintf ( 1, '  Take the absolute value of each dot product \n' );
  fprintf ( 1, '  and sum and average.\n' );

  for j = 1 : 5

    v = sphere01_sample_3d ( );

    dot_average = 0.0;

    for i = 1 : n_sample
      x = sphere01_sample_3d ( );
      dot_average = dot_average + abs ( x(1:dim_num) * v(1:dim_num)' );
    end

    dot_average = dot_average / n_sample;

    fprintf ( 1, '\n' );
    fprintf ( 1, '  V:                %10f  %10f  %10f\n', v(1:dim_num) );
    fprintf ( 1, '  Average |(XdotV)| %10f\n', dot_average );

  end

  return
end
