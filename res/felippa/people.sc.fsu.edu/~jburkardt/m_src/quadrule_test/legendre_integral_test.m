function legendre_integral_test ( )

%*****************************************************************************80
%
%% legendre_integral_test() tests legendre_integral().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    14 June 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'LEGENDRE_INTEGRAL_TEST\n' );
  fprintf ( 1, '  LEGENDRE_INTEGRAL evaluates\n' );
  fprintf ( 1, '  Integral ( -1 < x < +1 ) x^n dx\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         N         Value\n' );
  fprintf ( 1, '\n' );

  for n = 0 : 10

    value = legendre_integral ( n );

    fprintf ( 1, '  %8d  %24.16g\n', n, value );

  end

  return
end
