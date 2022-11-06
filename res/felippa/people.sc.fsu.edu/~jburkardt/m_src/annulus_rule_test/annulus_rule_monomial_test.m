function annulus_rule_monomial_test ( center, r1, r2 )

%*****************************************************************************80
%
%% annulus_rule_monomial_test() estimates monomial integrals using quadrature.
%
%  Discussion:
%
%    If CENTER=(0,0) and R1 = 0 and R2 = 1, then we can compare exact values.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 July 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real CENTER(2), the coordinates of the center.
%
%    real R1, R2, the inner and outer radii of the annulus.
%    0.0 <= R1 <= R2.
%
  e_test = [ ...
    0, 0; ...
    2, 0; ...
    0, 2; ...
    4, 0; ...
    2, 2; ...
    0, 4; ...
    6, 0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'ANNULUS_RULE_MONOMIAL_TEST\n' );
  fprintf ( 1, '  ANNULUS_RULE_COMPUTE can supply a quadrature rule for\n' );
  fprintf ( 1, '  the annulus centered at (%g,%g) with R1 = %g, R2 = %g\n', ...
    center(1), center(2), r1, r2 );
  fprintf ( 1, '  Apply this rule to a variety of monomials.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '    NR    NT           1              X^2             Y^2        ' );
  fprintf ( 1, '     X^4               X^2Y^2           Y^4             X^6\n' );
  fprintf ( 1, '\n' );

  nr = 4;
  
  while ( nr <= 64 )

    nt = 4 * nr;

    [ w, x, y ] = annulus_rule_compute ( center, r1, r2, nr, nt );

    xy = [ x'; y' ];

    fprintf ( 1, '  %4d  %4d', nr, nt );
    n = nr * nt;
    for j = 1 : 7
      e(1:2) = e_test(1:2,j);
      value = monomial_value ( 2, n, e, xy );
      result = w' * value;
      fprintf ( 1, '  %14.6g', result );
    end
    fprintf ( 1, '\n' );

    nr = 2 * nr;

  end

  if ( ...
    center(1) == 0.0 && ...
    center(2) == 0.0 && ...
    r1 == 0.0 && ...
    r2 == 1.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, '     Exact  ' );
    for j = 1 : 7

      e(1:2) = e_test(1:2,j);

      result = disk01_monomial_integral ( e );
      fprintf ( 1, '  %14.6g', result );
    end
    fprintf ( 1, '\n' );

  end

  return
end
 
