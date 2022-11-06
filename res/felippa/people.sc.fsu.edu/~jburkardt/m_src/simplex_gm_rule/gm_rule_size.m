function n = gm_rule_size ( rule, m )

%*****************************************************************************80
%
%% gm_rule_size() determines the size of a Grundmann-Moeller rule.
%
%  Discussion:
%
%    This rule returns the value of N, the number of points associated
%    with a GM rule of given index.
%
%    After calling this rule, the user can use the value of N to
%    allocate space for the weight vector as W(N) and the abscissa
%    vector as X(M,N), and then call GM_RULE_SET.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Axel Grundmann, Michael Moeller,
%    Invariant Integration Formulas for the N-Simplex
%    by Combinatorial Methods,
%    SIAM Journal on Numerical Analysis,
%    Volume 15, Number 2, April 1978, pages 282-290.
%
%  Input:
%
%    integer RULE, the index of the rule.
%    0 <= RULE.
%
%    integer M, the spatial dimension.
%    1 <= M.
%
%  Output:
%
%    integer N, the number of points in the rule.
%
  arg1 = m + rule + 1;

  n = nchoosek ( arg1, rule );

  return
end
