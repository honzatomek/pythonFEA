function c = v_polynomial_coefficients ( n )

%*****************************************************************************80
%
%% v_polynomial_coefficients(): coefficients of the Chebyshev polynomial V(n,x).
%
%  First terms:
%
%    N/K     0     1      2      3       4     5      6    7      8    9   10
%
%     0      1
%     1     -1     2
%     2     -1    -2      4
%     3      1    -4     -4      8
%     4      1    +4    -12     -8      16
%     5     -1     6    +12    -32     -16    32
%     6     -1    -6     24    +32     -80   -32     64
%     7     +1    -8    -24     80     +80  -192    -64   128
%
%  Recursion:
%
%    V(0,X) = 1,
%    V(1,X) = 2 * X - 1,
%    V(N,X) = 2 * X * V(N-1,X) - V(N-2,X)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 July 2015
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
%    real C(1:N+1,1:N+1), the coefficients.
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

  c(2,1) = -1.0;
  c(2,2) = 2.0;
 
  for i = 2 : n
    c(i+1,1)     =                  - c(i-1,1);
    c(i+1,2:i-1) = 2.0 * c(i,1:i-2) - c(i-1,2:i-1);
    c(i+1,  i  ) = 2.0 * c(i,  i-1);
    c(i+1,  i+1) = 2.0 * c(i,  i  );
  end
 
  return
end
