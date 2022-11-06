function square_minimal_rule_error_max_test ( )

%*****************************************************************************80
%
%% square_minimal_rule_error_max_test() tests square_minimal_rule_error_max().
%
%  Licensing:
%
%    This code is distributed under the GNU GPL license.
%
%  Modified:
%
%    19 February 2018
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
  fprintf ( 1, 'SQUARE_MINIMAL_RULE_ERROR_MAX_TEST\n' );
  fprintf ( 1, '  SQUARE_MINIMAL_RULE_ERROR_MAX computes the maximum\n' );
  fprintf ( 1, '  error for a rule that should be exact for all monomials\n' );
  fprintf ( 1, '  up to a given value of DEGREE.\n' );

  degree_max = square_minimal_rule_degree_max ( );

  fprintf ( 1, '\n' );
  fprintf ( 1, ' Degree  Monomials  Error Max\n' );
  fprintf ( 1, '\n' );
  for degree = 1 : degree_max
    error_max = square_minimal_rule_error_max ( degree );
    m_num = ( ( degree + 1 ) * ( degree + 2 ) ) / 2;
    fprintf ( 1, '   %4d       %4d  %g\n', degree, m_num, error_max );
  end

  return
end
