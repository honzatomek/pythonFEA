function quadrule_test ( )

%*****************************************************************************80
%
%% quadrule_test() tests quadrule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    02 March 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../quadrule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'quadrule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test quadrule().\n' );

  chebyshev_set_test ( );
  chebyshev1_compute_test ( );
  chebyshev1_integral_test ( );
  chebyshev1_set_test ( );
  chebyshev2_compute_test ( );
  chebyshev2_compute_test2 ( );
  chebyshev2_integral_test ( );
  chebyshev2_set_test ( );
  chebyshev3_compute_test ( );
  chebyshev3_integral_test ( );
  chebyshev3_set_test ( );
  clenshaw_curtis_compute_test ( );
  clenshaw_curtis_set_test ( );
  fejer1_compute_test ( );
  fejer1_set_test ( );
  fejer2_compute_test ( );
  fejer2_set_test ( );
  gegenbauer_integral_test ( );
  gegenbauer_ss_compute_test ( );
  gen_hermite_ek_compute_test ( );
  gen_hermite_integral_test ( );
  gen_laguerre_ek_compute_test ( );
  gen_laguerre_integral_test ( );
  gen_laguerre_ss_compute_test ( );
  hermite_ek_compute_test ( );
  hermite_integral_test ( );
  hermite_set_test ( );
  hermite_ss_compute_test ( );
  hermite_gk16_set_test ( );
  hermite_gk18_set_test ( );
  hermite_gk22_set_test ( );
  hermite_gk24_set_test ( );
  hermite_1_set_test ( );
  hermite_probabilist_set_test ( );
  imtqlx_test ( );
  jacobi_ek_compute_test ( );
  jacobi_integral_test ( );
  jacobi_ss_compute_test ( );
  kronrod_set_test( );
  laguerre_ek_compute_test ( );
  laguerre_integral_test ( );
  laguerre_set_test ( );
  laguerre_ss_compute_test ( );
  laguerre_1_set_test ( );
  legendre_dr_compute_test ( );
  legendre_ek_compute_test ( );
  legendre_integral_test ( );
  legendre_set_test ( );
  legendre_ss_compute_test ( );
  lobatto_compute_test ( );
  lobatto_set_test ( );
  nc_compute_weights_test ( );
  ncc_compute_test ( );
  ncc_set_test ( );
  nco_compute_test ( );
  nco_set_test ( );
  ncoh_compute_test ( );
  ncoh_set_test ( );
  patterson_set_test ( );
  r8_psi_test ( );
  radau_compute_test ( );
  radau_set_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'quadrule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../quadrule' );

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

