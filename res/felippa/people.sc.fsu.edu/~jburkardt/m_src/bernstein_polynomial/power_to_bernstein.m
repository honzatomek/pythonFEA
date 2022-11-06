function a = power_to_bernstein ( n )

%*****************************************************************************80
%
%% power_to_bernstein() returns the Power-to-Bernstein matrix.
%
%  Discussion:
%
%    The Power-to-Bernstein matrix of degree N is an N+1xN+1 matrix A which can 
%    be used to transform the N+1 coefficients of a polynomial of degree N
%    from a vector P of coefficients of the power basis (1,x,x^2,...,x^n)
%    to a vector B of Bernstein basis polynomial coefficients ((1-x)^n,...,x^n).
%
%    If we are using N=4-th degree polynomials, the matrix has the form:
%
%          1   0    0    0   0
%          1  1/4   0    0   0
%      A = 1  1/2  1/6   0   0
%          1  3/4  1/2  1/4  1
%          1   1    1    1   1
%
%   and a polynomial 
%     p(x) = 3x - 6x^2 + 3x^3
%   whose power coefficient vector is
%     P = ( 0, 3, -6, 3, 0 )
%   will have the Bernstein basis coefficients 
%     B = A * P = ( 0, 3/4, 1/2, 0, 0 ).
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
%    real A(N+1,N+1), the Power-to-Bernstein matrix.
%
  a = zeros ( n + 1, n + 1 );

  for j = 0 : n
    for i = 0 : j
      a(n-i+1,n-j+1) = nchoosek ( j, i ) / nchoosek ( n, i );
    end
  end

  return
end
