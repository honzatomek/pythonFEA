function gen_laguerre_integral_test ( )

%*****************************************************************************80
%
%% gen_laguerre_integral_test() tests gen_laguerre_integral().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    15 June 2015
%
%  Author:
%
%    John Burkardt
%
  alpha = 0.5;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'GEN_LAGUERRE_INTEGRAL_TEST\n' );
  fprintf ( 1, '  GEN_LAGUERRE_INTEGRAL evaluates\n' );
  fprintf ( 1, '  Integral ( 0 < x < +oo ) exp(-x) x^n x^alpha dx\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Use ALPHA = %g\n', alpha );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         N         Value\n' );
  fprintf ( 1, '\n' );

  for n = 0 : 10

    value = gen_laguerre_integral ( n, alpha );

    fprintf ( 1, '  %8d  %24.16g\n', n, value );

  end

  return
end
