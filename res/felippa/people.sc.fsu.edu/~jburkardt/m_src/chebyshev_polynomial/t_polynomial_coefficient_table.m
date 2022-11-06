function c = t_polynomial_coefficient_table ( n )

%*****************************************************************************80
%
%% t_polynomial_coefficient_table(): coefficients of Chebyshev polynomials T(0:n,x).
%
%  First terms:
%
%    N/K     0     1      2      3       4     5      6    7      8    9   10
%
%     0      1
%     1      0     1
%     2     -1     0      2
%     3      0    -3      0      4
%     4      1     0     -8      0       8
%     5      0     5      0    -20       0    16
%     6     -1     0     18      0     -48     0     32
%     7      0    -7      0     56       0  -112      0    64
%
%  Recursion:
%
%    T(0,X) = 1,
%    T(1,X) = X,
%    T(N,X) = 2 * X * T(N-1,X) - T(N-2,X)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 July 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz, Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%  Input:
%
%    integer N, the highest order polynomial to compute.
%    Note that polynomials 0 through N will be computed.
%
%  Output:
%
%    real C(1:N+1,1:N+1), the coefficient table.
%
  if ( n < 0 )
    c = [];
    return
  end

  c(1:n+1,1:n+1) = 0.0;

  c(1,1) = 1.0;

  if ( n == 0 )
    return
  end

  c(2,2) = 1.0;
 
  for i = 2 : n
    c(i+1,1)     =                  - c(i-1,1);
    c(i+1,2:i-1) = 2.0 * c(i,1:i-2) - c(i-1,2:i-1);
    c(i+1,  i  ) = 2.0 * c(i,  i-1);
    c(i+1,  i+1) = 2.0 * c(i,  i  );
  end
 
  return
end
