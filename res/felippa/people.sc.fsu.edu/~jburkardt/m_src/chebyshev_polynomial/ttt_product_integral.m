function value = ttt_product_integral ( i, j, k )

%*****************************************************************************80
%
%% ttt_product_integral(): integral (-1<=x<=1) T(i,x)*T(j,x)*T(k,x)/sqrt(1-x^2) dx
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 July 2015
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    John Mason, David Handscomb,
%    Chebyshev Polynomials,
%    CRC Press, 2002,
%    ISBN: 0-8493-035509,
%    LC: QA404.5.M37.
%
%  Input:
%
%    integer I, J, K, the polynomial indices.
%    0 <= I, J.
%
%  Output:
%
%    real VALUE, the integral.
%
  if ( i < 0 )
    value = 0.0;
  elseif ( j < 0 )
    value = 0.0;
  elseif ( k < 0 )
    value = 0.0;
  else
    value = 0.5 * ( ...
        tt_product_integral (       i + j,   k ) + ...
      + tt_product_integral ( abs ( i - j ), k ) );
  end

  return
end
