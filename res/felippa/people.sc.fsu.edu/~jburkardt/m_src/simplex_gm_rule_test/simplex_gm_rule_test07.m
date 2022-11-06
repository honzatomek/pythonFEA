function simplex_gm_rule_test07 ( )

%*****************************************************************************80
%
%% simplex_gm_rule_test07() tests gm_rule_set().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 July 2007
%
%  Author:
%
%    John Burkardt
%
  m = 5;
  degree_max = 4;
  rule_max = 3;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'SIMPLEX_GM_RULE_TEST07\n' );
  fprintf ( 1, '  GM_UNIT_RULE_SET determines the weights and abscissas\n' );
  fprintf ( 1, '  of a Grundmann-Moeller quadrature rule for\n' );
  fprintf ( 1, '  the M dimensional unit simplex,\n' );
  fprintf ( 1, '  using a rule of index RULE,\n' );
  fprintf ( 1, '  which will have degree of exactness 2*RULE+1.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  In this test, look at all the monomials up to\n' );
  fprintf ( 1, '  some maximum degree, choose a few low order rules\n' );
  fprintf ( 1, '  and determine the quadrature error for each.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Here we use M = %d\n', m );

  fprintf ( 1, '\n' );
  fprintf ( 1, '      Rule     Order     Quad_Error\n' );
  fprintf ( 1, '\n' );

  for degree = 0 : degree_max

    expon = [];
    more = 0;
    h = 0;
    t = 0;

    while ( true )

      [ expon, more, h, t ] = comp_next ( degree, m, expon, more, h, t );

      fprintf ( 1, '\n' );
      fprintf ( 1, '  F(X) = X1^%d * X2^%d * X3^%d * X4^%d * X5^%d\n', ...
        expon(1:5) );

      fprintf ( 1, '\n' );

      for rule = 0 : rule_max
        n = gm_rule_size ( rule, m );
        [ w, x ] = gm_unit_rule_set ( rule, m, n );
        quad_error = simplex_unit_monomial_quadrature ( m, expon, n, x, w );
        fprintf ( 1, '  %8d  %8d  %14.6e\n', rule, n, quad_error );

      end

      if ( ~ more )
        break
      end

    end

  end

  return
end


