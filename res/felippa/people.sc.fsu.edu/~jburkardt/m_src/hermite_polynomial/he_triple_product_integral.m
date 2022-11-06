function value = he_triple_product_integral ( i, j, k )

%*****************************************************************************80
%
%% he_triple_product_integral(): integral of H(i,x)*H(j,x)*H(k,x)*e^(-x^2/2).
%
%  Discussion:
%
%    He(i,x) represents the probabilist's Hermite polynomial.
%
%    VALUE = integral ( -oo < x < +oo ) H(i,x)*H(j,x)*H(k,x) exp(-x^2/2) dx
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Dongbin Xiu,
%    Numerical Methods for Stochastic Computations: A Spectral Method Approach,
%    Princeton, 2010,
%    ISBN13: 978-0-691-14212-8,
%    LC: QA274.23.X58.
%
%  Input:
%
%    integer I, J, K, the polynomial indices.
%
%  Output:
%
%    real VALUE, the value of the integral.
%
  s = floor ( ( i + j + k ) / 2 );

  if ( s < i || s < j || s < k ) )
    value = 0.0;
  elseif ( mod ( i + j + k, 2 ) ~= 0 )
    value = 0.0;
  else
    value = factorial ( i ) / factorial ( s - i ) ...
          * factorial ( j ) / factorial ( s - j ) ...
          * factorial ( k ) / factorial ( s - k );
  end

  return
end
