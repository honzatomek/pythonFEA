function tetrahedron_monte_carlo_test02 ( )

%*****************************************************************************80
%
%% tetrahedron_monte_carlo_test02() samples a general tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 August 2009
%
%  Author:
%
%    John Burkardt
%
  f_num = 6;

  t(1:3,1:4) = [ ...
    1.0, 2.0, 3.0; ...
    4.0, 1.0, 2.0; ...
    2.0, 4.0, 4.0; ...
    3.0, 2.0, 5.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'TETRAHEDRON_MONTE_CARLO_TEST02\n' );
  fprintf ( 1, '  Integrate TETRAHEDRON_UNIT_INTEGRAND_USER\n' );
  fprintf ( 1, '  Integration region is over a general tetrahedron.\n' );
  fprintf ( 1, '  Use an increasing number of points P_NUM.\n' );

  r8mat_transpose_print ( 3, 4, t, '  Tetrahedron vertices:' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '     P_NUM\n' );
  fprintf ( 1, '\n' );

  p_num = 1;

  while ( p_num <= 65536 )

    result = tetrahedron_monte_carlo ( t, p_num, f_num, ...
      @tetrahedron_integrand_user );

    fprintf ( 1, '  %8d  %12f  %12f  %12f  %12f  %12f  %12f\n', ...
      p_num, result(1:f_num) )

    p_num = 2 * p_num;

  end

  return
end
