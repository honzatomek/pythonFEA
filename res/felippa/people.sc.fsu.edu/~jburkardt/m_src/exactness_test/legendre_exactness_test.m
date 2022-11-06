function legendre_exactness_test ( )

%*****************************************************************************80
%
%% legendre_exactness_test() tests Gauss-Legendre rules for Legendre integrals.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 January 2016
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'LEGENDRE_EXACTNESS_TEST\n' );
  fprintf ( 1, '  Test Gauss-Legendre rules on Legendre integrals.\n' );
  fprintf ( 1, '  Density function rho(x) = 1.\n' );
  fprintf ( 1, '  Region: -1 <= x <= +1.\n' );
  fprintf ( 1, '  Exactness: 2*N-1.\n' );

  for n = 1 : 5

    [ x, w ] = legendre_set ( n );
    p_max = 2 * n;
    legendre_exactness ( n, x, w, p_max );

  end

  return
end
