function value = gegenbauer_cc1 ( n, lambda, f )

%*****************************************************************************80
%
%% gegenbauer_cc1() estimates the Gegenbauer integral of a function.
%
%  Discussion:
%
%     value = integral ( -1 <= x <= + 1 ) ( 1 - x^2 )^(lambda-1/2) * f(x) dx
%
%     The approximation uses the practical abscissas, that is, the extreme
%     points of Tn(x).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    07 January 2016
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    D B Hunter, H V Smith,
%    A quadrature formula of Clenshaw-Curtis type for the Gegenbauer weight function,
%    Journal of Computational and Applied Mathematics,
%    Volume 177, 2005, pages 389-400.
%
%  Input:
%
%    integer N, the number of points to use.
%    1 <= N.
%
%    real LAMBDA, used in the exponent of (1-x^2).
%    -0.5 < LAMBDA.
%
%    real F(x), the handle for the function to be integrated with the
%    Gegenbauer weight.
%
%  Output:
%
%    real WEIGHT, the estimate for the Gegenbauer integral of F.
%
  value = 0.0;

  s = floor ( n / 2 );
  sigma = mod ( n, 2 );

  a2 = chebyshev_even1 ( n, f );

  rh = s;
  u = 0.5 * ( sigma + 1.0 ) * a2(rh+1);
  for rh = s - 1 : -1 : 1
    u = ( rh - lambda ) / ( rh + lambda + 1 ) * u + a2(rh+1);
  end
  u = - lambda * u / ( lambda + 1.0 ) + 0.5 * a2(0+1);

  value = gamma ( lambda + 0.5 ) * sqrt ( pi ) * u / gamma ( lambda + 1.0 );

  return
end

