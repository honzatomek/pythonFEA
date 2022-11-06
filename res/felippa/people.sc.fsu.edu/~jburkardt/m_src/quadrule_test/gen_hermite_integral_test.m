function gen_hermite_integral_test ( )

%*****************************************************************************80
%
%% gen_hermite_integral_test() tests gen_hermite_integral().
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
  fprintf ( 1, 'GEN_HERMITE_INTEGRAL_TEST\n' );
  fprintf ( 1, '  GEN_HERMITE_INTEGRAL evaluates\n' );
  fprintf ( 1, '  Integral ( -oo < x < +oo ) exp(-x^2) x^n |x|^alpha dx\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Use ALPHA = %g\n', alpha );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         N         Value\n' );
  fprintf ( 1, '\n' );

  for n = 0 : 10

    value = gen_hermite_integral ( n, alpha );

    fprintf ( 1, '  %8d  %24.16g\n', n, value );

  end

  return
end
