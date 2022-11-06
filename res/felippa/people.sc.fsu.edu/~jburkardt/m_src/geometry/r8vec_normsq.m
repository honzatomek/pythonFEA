function value = r8vec_normsq ( n, a )

%*****************************************************************************80
%
%% r8vec_normsq() returns the squared L2 norm of an R8VEC.
%
%  Discussion:
%
%    The sqaured vector L2 norm is defined as:
%
%      value = sum ( 1 <= I <= N ) A(I)^2.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 October 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the vector dimension.
%
%    real A(N), the vector.
%
%  Output:
%
%    real VALUE, the sqaured L2 norm.
%
  value = sum ( a(1:n).^2 );

  return
end
