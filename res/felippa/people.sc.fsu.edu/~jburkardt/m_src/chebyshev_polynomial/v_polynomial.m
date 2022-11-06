function v = v_polynomial ( m, n, x )

%*****************************************************************************80
%
%% v_polynomial() evaluates Chebyshev polynomials V(n,x).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 July 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the number of evaluation points.
%
%    integer N, the highest polynomial to compute.
%
%    real X(M,1), the evaluation points.
%
%  Output:
%
%    real V(1:M,1:N+1), the values of the N+1 Chebyshev polynomials.
%
  if ( n < 0 )
    v = [];
    return
  end

  v = zeros(m,n+1);

  v(1:m,1) = 1.0;

  if ( n < 1 )
    return
  end

  x = x(:);

  v(1:m,2) = 2.0 * x(1:m) - 1.0;

  for j = 2 : n
    v(1:m,j+1) = 2.0 * x(1:m) .* v(1:m,j) - v(1:m,j-1);
  end

  return
end
