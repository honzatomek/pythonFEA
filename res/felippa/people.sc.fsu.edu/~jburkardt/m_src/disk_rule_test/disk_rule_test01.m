function disk_rule_test01 ( )

%*****************************************************************************80
%
%% disk_rule_test01() tests disk_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 January 2019
%
%  Author:
%
%    John Burkardt
%
  nr = 4;
  nt = 8;
%
%  Tabulated exact integrals of 1, x, y, x^2, xy, y^2, x^3, ..., y^4
%  over circle centered at (1,2) with radius 3.
%
  exact = pi * [ 
       9.0, ...
       9.0,        18.0, ...
     117.0 / 4.0,  18.0,        225.0 / 4.0, ...
     279.0 / 4.0, 117.0 / 2.0,  225.0 / 4.0, 387.0 / 2.0, ...
    1773.0 / 8.0, 279.0 / 2.0, 1341.0 / 8.0, 387.0 / 2.0, 5769.0 / 8.0 ];
 
  fprintf ( 1, '\n' );
  fprintf ( 1, 'DISK_RULE_TEST01\n' );
  fprintf ( 1, '  DISK_RULE can compute a rule Q(f) for a general disk\n' );
  fprintf ( 1, '  centered at (XC,YC) with radius RC,\n' );
  fprintf ( 1, '  using NT equally spaced angles and NR radial distances.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  NT = %d\n', nt );
  fprintf ( 1, '  NR = %d\n', nr );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Estimate integrals I(f) where f = x^e(1) * y^e(2).\n' );
%
%  Define center and radius of non-unit disk.
%
  xc = 1.0;
  yc = 2.0;
  rc = 3.0;
%
%  Compute the quadrature rule.
%
  [ w, x, y ] = disk_rule ( nr, nt, xc, yc, rc );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  E(1)  E(2)    I(f)            Q(f)\n' ); 
  fprintf ( 1, '\n' );
%
%  Specify a monomial F(X,Y) = X^E(1) * Y^E(2).
%
  k = 0;

  for d = 0 : 4

    for e1 = d : -1 : 0

      e2 = d - e1;

      e(1) = e1;
      e(2) = e2;
%
%  Evaluate the function at all the quadrature points.
%
      f = x(1:nr*nt) .^ e(1) .* y(1:nr*nt) .^ e(2);
%
%  Compute the weighted sum of the function values.
%
      s = w' * f;
%
%  Compute the disk area.
%
      area = pi * rc ^ 2;
%
%  Q is the quadrature estimate.
%
      q = area * s;

      k = k + 1;

      fprintf ( 1, '   %2d  %2d  %14.6g  %14.6g\n', e(1), e(2), exact(k), q );

    end

  end

  return
end

