function triangle_integrals_test ( )

%*****************************************************************************80
%
%% triangle_integrals_test() tests triangle_integrals().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 April 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../triangle_integrals' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_integrals_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test triangle_integrals().\n' );

  i4_to_pascal_test ( );
  i4_to_pascal_degree_test ( );
  pascal_to_i4_test ( );
  r8mat_print_test ( );
  r8mat_print_some_test ( );
  trinomial_test ( );

  rs_to_xy_map_test ( );
  xy_to_rs_map_test ( );

  poly_print_test ( );
  poly_power_linear_test ( );
  poly_power_test ( );
  poly_product_test ( );

  triangle01_monomial_integral_test ( );
  triangle01_poly_integral_test ( );
  triangle_area_test ( );
  triangle_xy_integral_test ( );
  triangle_monomial_integral_test ( );
  triangle_poly_integral_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_integrals_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../triangle_integrals' );

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp() prints the current YMDHMS date as a timestamp.
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
  t = now;
  c = datevec ( t );
  s = datestr ( c, 0 );
  fprintf ( 1, '%s\n', s );

  return
end

