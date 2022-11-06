function a = bernstein_matrix_inverse ( n )

%*****************************************************************************80
%
%% bernstein_matrix_inverse() returns the inverse Bernstein matrix.
%
%  Discussion:
%
%    The inverse Bernstein matrix of order N is an NxN matrix A which can 
%    be used to transform a vector of Bernstein basis coefficients B
%    representing a polynomial P(X) to a corresponding power basis 
%    coefficient vector C:
%
%      C = A * B
%
%    The N power basis vectors are ordered as (1,X,X^2,...X^(N-1)) and the N 
%    Bernstein basis vectors as ((1-X)^(N-1), X*(1_X)^(N-2),...,X^(N-1)).
%
%    For N = 5, the matrix has the form:
%
%      1   1    1    1   1
%      0  1/4  1/2  3/4  1
%      0   0   1/6  1/2  1
%      0   0    0   1/4  1
%      0   0    0    0   1
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of the matrix.
%
%  Output:
%
%    real A(N,N), the inverse Bernstein matrix.
%
  a = zeros ( n, n );

  n0 = n - 1;

  for j0 = 0 : n0
    for i0 = 0 : j0
      a(i0+1,j0+1) = nchoosek ( j0, i0 ) / nchoosek ( n0, i0 );
    end
  end

  return
end
