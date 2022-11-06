function c = cardinal_cos ( j, m, n, t )

%*****************************************************************************80
%
%% cardinal_cos() evaluates the J-th cardinal cosine basis function.
%
%  Discussion:
%
%    The base points are T(I) = pi * I / ( M + 1 ), 0 <= I <= M + 1.
%    Basis function J is 1 at T(J), and 0 at T(I) for I /= J
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    11 January 2015
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    John Boyd,
%    Exponentially convergent Fourier-Chebyshev quadrature schemes on
%    bounded and infinite intervals,
%    Journal of Scientific Computing,
%    Volume 2, Number 2, 1987, pages 99-109.
%
%  Input:
%
%    integer J, the index of the basis function.
%    0 <= J <= M + 1.
%
%    integer M, indicates the size of the basis set.
%
%    integer N, the number of evaluation points.
%
%    real T(N), one or more points in [0,pi] where the
%    basis function is to be evaluated.
%
%  Output:
%
%    real C(N), the value of the function at T.
%
  if ( j == 0 | j == m + 1 )
    cj = 2.0;
  else
    cj = 1.0;
  end

  tj = pi * j / ( m + 1 );

  c = ( -1.0 ) ^ mod ( j + 1, 2 ) ...
    * sin ( t ) ...
    .* sin ( ( m + 1 ) * t ) ...
    / cj ...
    / ( m + 1 ) ...
    ./ ( cos ( t ) - cos ( tj ) );

  i = find ( abs ( t - tj ) < eps );
  c(i) = 1.0;

  return
end
  
