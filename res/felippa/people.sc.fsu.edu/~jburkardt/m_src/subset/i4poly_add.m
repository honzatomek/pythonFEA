function c = i4poly_add ( na, a, nb, b )

%*****************************************************************************80
%
%% i4poly_add() adds two I4POLY's.
%
%  Discussion:
%
%    The polynomials are in power sum form.
%
%    The power sum form is:
%
%      p(x) = a(0) + a(1)*x + ... + a(n-1)*x^(n-1) + a(n)*x^(n)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 November 2013
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NA, the degree of polynomial A.
%
%    integer A(1:NA+1), the coefficients of the first
%    polynomial factor.
%
%    integer NB, the degree of polynomial B.
%
%    integer B(1:NB+1), the coefficients of the
%    second polynomial factor.
%
%  Output:
%
%    integer C(1:max(NA,NB)+1), the coefficients of A + B.
%
  c = zeros ( max ( na, nb ) + 1, 1 );

  if ( nb == na )
    c(1:na+1) = a(1:na+1) + b(1:na+1);
  elseif ( nb < na )
    c(1:nb+1) = a(1:nb+1) + b(1:nb+1);
    c(nb+2:na+1) = a(nb+2:na+1);
  elseif ( na < nb )
    c(1:na+1) = a(1:na+1) + b(1:na+1);
    c(na+2:nb+1) = b(na+2:nb+1);
  end

  return
end
