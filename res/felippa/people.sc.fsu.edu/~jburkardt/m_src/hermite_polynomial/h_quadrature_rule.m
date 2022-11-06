function [ t, wts ] = h_quadrature_rule ( nt )

%*****************************************************************************80
%
%% h_quadrature_rule(): quadrature for H(i,x).
%
%  Discussion:
%
%    H(i,x) is the physicist's Hermite polynomial of degree I.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 February 2012
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NT, the order of the rule.
%
%  Output:
%
%    real T(NT,1), WTS(NT,1), the points and weights of the rule.
%
  aj = zeros ( nt, 1 );
  bj = sqrt ( ( 1 : nt )' / 2.0 );
  wts = zeros ( nt, 1 );
  wts(1,1) = sqrt ( sqrt ( pi ) );

  [ t, wts ] = imtqlx ( nt, aj, bj, wts );

  wts(1:nt,1) = wts(1:nt,1).^2;

  return
end

