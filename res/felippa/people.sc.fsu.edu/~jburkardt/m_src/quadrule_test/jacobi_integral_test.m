function jacobi_integral_test ( )

%*****************************************************************************80
%
%% jacobi_integral_test() tests jacobi_integral().
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
  alpha = 1.5;
  beta = 0.5;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'JACOBI_INTEGRAL_TEST\n' );
  fprintf ( 1, '  JACOBI_INTEGRAL evaluates\n' );
  fprintf ( 1, '  Integral ( -1 < x < +1 ) x^n (1-x)^alpha (1+x)^beta dx\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Use ALPHA = %g\n', alpha );
  fprintf ( 1, '      BETA =  %g\n', beta );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         N         Value\n' );
  fprintf ( 1, '\n' );

  for n = 0 : 10

    value = jacobi_integral ( n, alpha, beta );

    fprintf ( 1, '  %8d  %24.16g\n', n, value );

  end

  return
end
