function value = r8mat_norm_fro ( m, n, a )

%*****************************************************************************80
%
%% r8mat_norm_fro() returns the Frobenius norm of an R8MAT.
%
%  Discussion:
%
%    The Frobenius norm is defined as
%
%      value = sqrt ( sum ( 1 <= I <= M ) sum ( 1 <= j <= N ) A(I,J)^2 )
%
%    The matrix Frobenius norm is not derived from a vector norm, but
%    is compatible with the vector L2 norm, so that:
%
%      vec_norm_l2 ( A * x ) <= mat_norm_fro ( A ) * vec_norm_l2 ( x ).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 April 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the number of rows in A.
%
%    integer N, the number of columns in A.
%
%    real A(M,N), the matrix whose Frobenius
%    norm is desired.
%
%  Output:
%
%    real VALUE, the Frobenius norm of A.
%
  value = sqrt ( sum ( sum ( a(1:m,1:n).^2 ) ) );

  return
end
