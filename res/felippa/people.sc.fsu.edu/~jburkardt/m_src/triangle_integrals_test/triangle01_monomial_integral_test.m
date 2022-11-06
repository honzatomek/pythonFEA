function triangle01_monomial_integral_test ( )

%*****************************************************************************80
%
%% triangle01_monomial_integral_test() estimates integrals over the unit triangle.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 April 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'TRIANGLE01_MONOMIAL_INTEGRAL_TEST\n' );
  fprintf ( 1, '  TRIANGLE01_MONOMIAL_INTEGRAL returns the integral Q of\n' );
  fprintf ( 1, '  a monomial X^I Y^J over the interior of the unit triangle.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '   I   J         Q(I,J)\n' );

  for d = 0 : 5
    fprintf ( 1, '\n' );
    for i = 0 : d
      j = d - i;
      q = triangle01_monomial_integral ( i, j );
      fprintf ( 1, '  %2d  %2d  %14.6g\n', i, j, q );
    end
  end

  return
end
