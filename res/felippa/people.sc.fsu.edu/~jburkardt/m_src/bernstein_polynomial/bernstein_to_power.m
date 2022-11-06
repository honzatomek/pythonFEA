function a = bernstein_to_power ( n )

%*****************************************************************************80
%
%% bernstein_to_power() returns the Bernstein-to-Power matrix.
%
%  Discussion:
%
%    The Bernstein-to-Power matrix of degree N is an N+1xN+1 matrix A which can 
%    be used to transform the N+1 coefficients of a polynomial of degree N
%    from a vector B of Bernstein basis polynomial coefficients ((1-x)^n,...,x^n).
%    to a vector P of coefficients of the power basis (1,x,x^2,...,x^n).
%
%    If we are using N=4-th degree polynomials, the matrix has the form:
%
%      1   0   0   0  0
%     -4   4   0   0  0
%      6 -12   6   0  0
%     -4  12 -12   4  0
%      1  -4   6  -4  1
%
%   and a polynomial with the Bernstein basis representation
%     p(x) = 3/4 * b(4,1) + 1/2 b(4,2)
%   whose Bernstein coefficient vector is
%     B = ( 0, 3/4, 1/2, 0, 0 )
%   will have the Bernstein basis coefficients 
%     P = A * B = ( 0, 3, -6, 3, 0 ).
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
%    integer N, the degree of the polynomials.
%
%  Output:
%
%    real A(N+1,N+1), the Bernstein-to-Power matrix.
%
  a = zeros ( n + 1, n + 1 );

  for j = 0 : n
    for i = 0 : j
      a(n-i+1,n-j+1) = r8_mop ( j - i ) * nchoosek ( n - i, j - i ) ...
        * nchoosek ( n, i );
    end
  end

  return
end
