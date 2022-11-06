function [ x, w ] = fejer1_rule_compute ( n )

%*****************************************************************************80
%
%% fejer1_rule_compute() computes a Fejer Type 1 rule.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 March 2007
%
%  Author:
%
%    Joerg Waldvogel
%
%  Reference:
%
%    Joerg Waldvogel,
%    Fast Construction of the Fejer and Clenshaw-Curtis Quadrature Rules,
%    BIT Numerical Mathematics
%    Volume 43, Number 1, pages 1-18, 2003.
%
%  Input:
%
%    integer N, the order of the rule.
%
%  Output:
%
%    real X(N), W(N), the abscissas and weights of the rule.
%
  N = [ 1 : 2 : n-1 ]';
  L = length ( N );
  m = n - L;
  K = [ 0 : m-1 ]';

  s = zeros ( L+1, 1 );
  v0 = [ 2 * exp(i*pi*K/n) ./ ( 1 - 4 * K.^2 ); s ];

  v1 = v0(1:end-1) + conj ( v0(end:-1:2) );
  w = ifft ( v1 );
  
  x = cos ( pi * ( (n:-1:1)' - 1 / 2 ) / n );

  return
end
