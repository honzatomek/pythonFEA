function a2 = chebyshev_even1 ( n, f )

%*****************************************************************************80
%
%% chebyshev_even1() returns the even Chebyshev coefficients of F.
%
%  Discussion:
%
%    The coefficients are calculated using the extreme points of Tn(x).
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
%    real F(x), the handle for the function to be integrated with the
%    Gegenbauer weight.
%
%  Output:
%
%    real A2(1+N/2), the even Chebyshev coefficients of F.
%
  s = floor ( n / 2 );
  sigma = mod ( n, 2 );

  a2 = zeros ( s + 1 );

  for r = 0 : 2 : 2 * s
    total = 0.5 * f ( 1.0 );
    for j = 1 : n - 1
      total = total + f ( cos ( j * pi / n ) ) * cos ( r * j * pi / n );
    end
    total = total + 0.5 * r8_mop ( r ) * f ( -1.0 );
    rh = r / 2;
    a2(rh+1) = ( 2.0 / n ) * total;
  end

  return
end
