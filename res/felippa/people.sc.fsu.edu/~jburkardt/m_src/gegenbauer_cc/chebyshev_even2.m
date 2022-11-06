function b2 = chebyshev_even2 ( n, f )

%*****************************************************************************80
%
%% chebyshev_even2() returns the even Chebyshev coefficients of F.
%
%  Discussion:
%
%    The coefficients are calculated using the zeros of Tn(x).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    08 January 2016
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
%    real B2(1+N/2), the even Chebyshev coefficients of F.
%
  s = floor ( n / 2 );
  sigma = mod ( n, 2 );

  b2 = zeros ( s + 1 );

  for r = 0 : 2 : 2 * s
    total = 0.0;
    for j = 0 : n
      total = total + f ( cos ( ( 2 * j + 1 ) * pi / 2 / ( n + 1 ) ) ) ...
        * cos ( r * ( 2 * j + 1 ) * pi / 2 / ( n + 1 ) );
    end
    rh = r / 2;
    b2(rh+1) = ( 2.0 / ( n + 1 ) ) * total;
  end

  return
end
