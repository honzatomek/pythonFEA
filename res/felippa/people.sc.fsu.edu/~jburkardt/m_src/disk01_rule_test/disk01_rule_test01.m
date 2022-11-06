function disk01_rule_test01 ( )

%*****************************************************************************80
%
%% disk01_rule_test01() tests disk01_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 April 2016
%
%  Author:
%
%    John Burkardt
%
  nr = 4;
  nt = 8;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'DISK01_RULE_TEST01\n' );
  fprintf ( 1, '  DISK01_RULE can compute a rule Q(f) for the unit disk\n' );
  fprintf ( 1, '  using NT equally spaced angles and NR radial distances.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  NT = %d\n', nt );
  fprintf ( 1, '  NR = %d\n', nr );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Estimate integrals I(f) where f = x^e(1) * y^e(2).\n' );
%
%  Compute the quadrature rule.
%
  [ w, r, t ] = disk01_rule ( nr, nt );
%
%  Apply it to integrands.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  E(1)  E(2)    I(f)            Q(f)\n' ); 
  fprintf ( 1, '\n' );
%
%  Specify a monomial.
%
  for e1 = 0 : 2 : 6

    e(1) = e1;

    for e2 = e1 : 2 : 6

      e(2) = e2;

      s = 0.0;
      for j = 1 : nt
        for i = 1 : nr
          x = r(i) * cos ( t(j) );
          y = r(i) * sin ( t(j) );
          s = s + w(i) * x ^ e(1) * y ^ e(2);
        end
      end

      area = pi;

      q = area * s;

      exact = disk01_monomial_integral ( e );

      fprintf ( 1, '   %2d  %2d  %14.6g  %14.6g\n', e(1), e(2), exact, q );

    end

  end

  return
end
