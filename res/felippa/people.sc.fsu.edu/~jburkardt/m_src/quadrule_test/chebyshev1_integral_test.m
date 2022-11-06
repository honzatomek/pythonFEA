function chebyshev1_integral_test ( )

%*****************************************************************************80
%
%% chebyshev1_integral_test() tests chebyshev1_integral().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    12 June 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CHEBYSHEV1_INTEGRAL_TEST\n' );
  fprintf ( 1, '  CHEBYSHEV1_INTEGRAL evaluates\n' );
  fprintf ( 1, '  Integral ( -1 < x < +1 ) x^n / sqrt(1-x*x) dx\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         N         Value\n' );
  fprintf ( 1, '\n' );

  for n = 0 : 10

    value = chebyshev1_integral ( n );

    fprintf ( 1, '  %8d  %24.16g\n', n, value );

  end

  return
end
