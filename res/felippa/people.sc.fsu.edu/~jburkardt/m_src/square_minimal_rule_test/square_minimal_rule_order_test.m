function square_minimal_rule_order_test ( )

%*****************************************************************************80
%
%% square_minimal_rule_order_test() tests square_minimal_rule_order().
%
%  Licensing:
%
%    This code is distributed under the GNU GPL license.
%
%  Modified:
%
%    07 February 2018
%
%  Author:
%
%    John Burkardt.
%
%  Reference:
%
%    Mattia Festa, Alvise Sommariva,
%    Computing almost minimal formulas on the square,
%    Journal of Computational and Applied Mathematics,
%    Volume 17, Number 236, November 2012, pages 4296-4302.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'SQUARE_MINIMAL_RULE_ORDER_TEST\n' );
  fprintf ( 1, '  Print the order (number of points) for each\n' );
  fprintf ( 1, '  minimal square rule.\n' );

  degree_max = square_minimal_rule_degree_max ( );

  fprintf ( 1, '\n' );
  fprintf ( 1, ' Degree  Order\n' );
  fprintf ( 1, '\n' );
  for degree = 1 : degree_max
    order = square_minimal_rule_order ( degree );
    fprintf ( 1, '  %4d  %4d\n', degree, order );
  end

  return
end
