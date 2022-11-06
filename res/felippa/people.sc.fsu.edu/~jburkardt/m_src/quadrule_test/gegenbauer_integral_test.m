function gegenbauer_integral_test ( )

%*****************************************************************************80
%
%% gegenbauer_integral_test() tests gegenbauer_integral().
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
  alpha = 0.25;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'GEGENBAUER_INTEGRAL_TEST\n' );
  fprintf ( 1, '  GEGENBAUER_INTEGRAL evaluates\n' );
  fprintf ( 1, '  Integral ( -1 < x < +1 ) x^n * (1-x*x)^alpha dx\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         N         Value\n' );
  fprintf ( 1, '\n' );

  for n = 0 : 10

    value = gegenbauer_integral ( n, alpha );

    fprintf ( 1, '  %8d  %24.16g\n', n, value );

  end

  return
end
