function v = monomial_value_1d ( n, e, x )

%*****************************************************************************80
%
%% monomial_value_1d() evaluates a monomial in 1D.
%
%  Discussion:
%
%    This routine evaluates a monomial of the form
%
%      x^e
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    17 January 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points.
%
%    integer E, the exponent.
%
%    real X(N), the point coordinates.
%
%  Output:
%
%    real V(N), the value of the monomial.
%
  v(1:n) = x(1:n) .^ e;

  return
end
