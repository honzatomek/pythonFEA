function chebyshev1_exactness_test ( )

%*****************************************************************************80
%
%% chebyshev1_exactness_test() tests rules for the Chebyshev1 integral.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 May 2014
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CHEBYSHEV1_EXACTNESS_TEST\n' );
  fprintf ( 1, '  Test Gauss-Chebyshev1 rules for the Chebyshev1 integral.\n' );
  fprintf ( 1, '  Density function rho(x) = 1/sqrt(1-x^2).\n' );
  fprintf ( 1, '  Region: -1 <= x <= +1.\n' );
  fprintf ( 1, '  Exactness: 2*N-1.\n' );

  for n = 1 : 5

    [ x, w ] = chebyshev1_set ( n );
    p_max = 2 * n;
    chebyshev1_exactness ( n, x, w, p_max );

  end

  return
end
