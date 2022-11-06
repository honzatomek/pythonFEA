function [ x, w ] = ncc_compute ( n )

%*****************************************************************************80
%
%% ncc_compute() computes a Newton-Cotes Closed quadrature rule.
%
%  Discussion:
%
%    For the interval [-1,+1], the Newton-Cotes Closed quadrature rule
%    estimates
%
%      Integral ( -1 <= X <= +1 ) F(X) dX
%
%    using N abscissas X and weights W:
%
%      sum ( 1 <= I <= N ) W(I) * F ( X(I) ).
%
%    For the CLOSED rule, the abscissas are equally spaced, and include
%    the end points.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 November 2009
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

  if ( n == 1 )

    x(1) = ( x_max + x_min ) / 2.0;

  else

    for i = 1 : n
      x(i) = ( ( n - i     ) * x_min   ...
             + (     i - 1 ) * x_max ) ...
             / ( n     - 1 );
    end

  end

  if ( n == 1 )

    w(1) = x_max - x_min;

  else

    w = nc_compute_weights ( n, x_min, x_max, x );

  end

  return
end
