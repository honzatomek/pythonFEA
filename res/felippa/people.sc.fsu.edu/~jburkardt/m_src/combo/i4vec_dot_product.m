function value = i4vec_dot_product ( n, x, y )

%*****************************************************************************80
%
%% i4vec_dot_product() computes the dot product of two I4VEC's.
%
%  Discussion:
%
%    An I4VEC is a vector of I4's.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 December 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the size of the array.
%
%    integer X(N), Y(N), the arrays.
%
%  Output:
%
%    integer VALUE, the dot product of X and Y.
%

%
%  Destroy all row vectors!
%
  x = x(:);
  y = y(:);

  value = x' * y;

  return
end
