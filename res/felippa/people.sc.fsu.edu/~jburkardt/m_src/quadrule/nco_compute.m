function [ x, w ] = nco_compute ( n )

%*****************************************************************************80
%
%% nco_compute() computes a Newton-Cotes Open quadrature rule.
%
%  Discussion:
%
%    The integral:
%
%      Integral ( -1 <= X <= +1 ) F(X) dX
%
%    The quadrature rule:
%
%      Sum ( 1 <= I <= N ) W(I) * F ( X(I) ).
%
%    For the OPEN rule, the abscissas do not include the end points.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order.
%
%  Output:
%
%    real X(N), the abscissas.
%
%    real W(N), the weights.
%
  x = zeros ( n, 1 );

  x_min = -1.0;
  x_max =  1.0;

  for i = 1 : n
    x(i) = ( ( n - i + 1 ) * x_min   ...
           + (     i     ) * x_max ) ...
           / ( n     + 1 );
  end

  w = nc_compute_weights ( n, x_min, x_max, x );

  return
end
