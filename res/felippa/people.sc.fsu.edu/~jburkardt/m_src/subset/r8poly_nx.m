function [ a, xarray ] = r8poly_nx ( n, a, xarray, x )

%*****************************************************************************80
%
%% r8poly_nx() replaces one of the base points in a polynomial in Newton form.
%
%  Discussion:
%
%    The Newton form of a polynomial is described by an array of N coefficients
%    A and N abscissas X:
%
%      p(x) =   a(1)
%             + a(2) * (x-x(1))
%             + a(3) * (x-x(1)) * (x-x(2))
%             ...
%             + a(n) * (x-x(1)) * (x-x(2)) * ... * (x-x(n-1))
%
%    X(N) does not occur explicitly in the formula for the evaluation of p(x),
%    although it is used in deriving the coefficients A.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the dimension of A.
%
%    real A(N), the polynomial coefficients of the Newton form.
%
%    real XARRAY(N), the set of abscissas that
%    are part of the Newton form of the polynomial.  %
%    Input, real X, the new point to be shifted into XARRAY.
%
%  Output:
%
%    real A(N), the updated polynomial coefficients
%    of the Newton form.
%
%    real XARRAY(N), the shifted abscissas.  The first
%    entry is now equal to X.
%
 for i = n - 1 : -1 : 1
    a(i) = a(i) + ( x - xarray(i) ) * a(i+1);
  end

  for i = n : -1 : 2
    xarray(i) = xarray(i-1);
  end
  xarray(1) = x;

  return
end
