function wedge_integrals_test01 ( )

%*****************************************************************************80
%
%% wedge_integrals_test01() compares exact and estimated monomial integrals.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 August 2014
%
%  Author:
%
%    John Burkardt
%
  m = 3;
  n = 500000;
  e_max = 6;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'WEDGE_INTEGRALS_TEST01()\n' );
  fprintf ( 1, '  Compare exact and estimated integrals\n' );
  fprintf ( 1, '  over the interior of the unit wedge in 3D.\n' );

  x = wedge01_sample ( n );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of sample points used is %d\n', n );
  fprintf ( 1, '\n' );
  fprintf ( 1, '   E1  E2  E3     MC-Estimate      Exact           Error\n' );
  fprintf ( 1, '\n' );
%
%  Check all monomials up to total degree E_MAX.
%
  for e3 = 0 : e_max
    expon(3) = e3;
    for e2 = 0 : e_max - e3
      expon(2) = e2;
      for e1 = 0 : e_max - e3 - e2
        expon(1) = e1;

        value = monomial_value ( m, n, expon, x );

        q = wedge01_volume ( ) * sum ( value(1:n) ) / n;
        exact = wedge01_monomial_integral ( expon );
        error = abs ( q - exact );

        fprintf ( 1, '  %2d  %2d  %2d  %14.6g  %14.6g  %14.6g\n', ...
          expon(1:3), q, exact, error );

      end
    end
  end

  return
end
 
