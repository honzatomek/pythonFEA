function c = cheby_t_poly_coef ( n )

%*****************************************************************************80
%
%% cheby_t_poly_coef() evaluates coefficients of Chebyshev polynomials T(n,x).
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
%     8      1     0    -32      0     160     0   -256     0   128
%     9      0     9      0   -120       0   432      0   576     0  256
%    10     -1     0     50      0    -400     0   1120     0 -1280    0  512
%
%  Recursion:
%
%    T(0)(X) = 1,
%    T(1)(X) = X,
%    T(N)(X) = 2 * X * T(N-1)(X) - T(N-2)(X)
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
%    Milton Abramowitz and Irene Stegun,
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
%    real C(1:N+1,1:N+1), the coefficients of the Chebyshev T polynomials.
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
