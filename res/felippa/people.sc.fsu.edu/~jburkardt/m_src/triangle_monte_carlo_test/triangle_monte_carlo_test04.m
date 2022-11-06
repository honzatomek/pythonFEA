function triangle_monte_carlo_test04 ( )

%*****************************************************************************80
%
%% triangle_monte_carlo_test04() uses triangle_sample_04() with an increasing number of points.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 August 2009
%
%  Author:
%
%    John Burkardt
%
  f_num = 3;

  t(1:2,1:3) = [ ...
    1.0, 0.0; ...
    0.0, 1.0; ...
    0.0, 0.0]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'TRIANGLE_MONTE_CARLO_TEST04\n' );
  fprintf ( 1, '  Sample using TRIANGLE_UNIT_SAMPLE_04\n' );
  fprintf ( 1, '  Integrate TRIANGLE_UNIT_INTEGRAND_03\n' );
  fprintf ( 1, '  Integration region is the unit triangle.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Use an increasing number of points P_NUM.\n' );
  fprintf ( 1, '  Note that the sample routine is a good" sampler.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '     P_NUM      X^2             X*Y             Y^2\n' );
  fprintf ( 1, '\n' );

  p_num = 1;

  while ( p_num <= 65536 )

    result = triangle_monte_carlo ( t, p_num, f_num, ...
      @triangle_unit_sample_04, @triangle_integrand_03 );

    fprintf ( 1, '  %8d  %14f  %14f  %14f\n', p_num, result(1:f_num) );

    p_num = 2 * p_num;

  end

  return
end
