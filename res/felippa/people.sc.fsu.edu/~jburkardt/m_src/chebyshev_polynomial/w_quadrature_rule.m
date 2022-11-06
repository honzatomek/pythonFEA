function [ t, w ] = w_quadrature_rule ( n )

%*****************************************************************************80
%
%% w_quadrature_rule(): quadrature rule for W(n,x).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 July 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of the rule.
%
%  Output:
%
%    real T(N,1), W(N,1), the points and weights of the rule.
%
  aj = zeros ( n, 1 );
  aj(1) = - 0.5;

  bj = 0.5 * ones ( n, 1 );

  w = zeros ( n, 1 );
  w(1,1) = sqrt ( pi );

  [ t, w ] = imtqlx ( n, aj, bj, w );

  w(1:n,1) = w(1:n,1).^2;

  return
end

