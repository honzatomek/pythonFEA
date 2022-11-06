function hermite_integral_test ( )

%*****************************************************************************80
%
%% hermite_integral_test() tests hermite_integral().
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
  fprintf ( 1, 'HERMITE_INTEGRAL_TEST\n' );
  fprintf ( 1, '  HERMITE_INTEGRAL evaluates\n' );
  fprintf ( 1, '  Integral ( -oo < x < +oo ) exp(-x^2) x^n dx\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         N         Value\n' );
  fprintf ( 1, '\n' );

  for n = 0 : 10

    value = hermite_integral ( n );

    fprintf ( 1, '  %8d  %24.16g\n', n, value );

  end

  return
end
