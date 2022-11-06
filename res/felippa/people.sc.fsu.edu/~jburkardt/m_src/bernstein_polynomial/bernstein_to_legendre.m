function a = bernstein_to_legendre ( n )

%*****************************************************************************80
%
%% bernstein_to_legendre() returns the Bernstein-to-Legendre matrix.
%
%  Discussion:
%
%    The Legendre polynomials are often defined on [-1,+1], while the
%    Bernstein polynomials are defined on [0,1].  For this function,
%    the Legendre polynomials have been shifted to share the [0,1]
%    interval of definition.
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
%    integer N, the maximum degree of the polynomials.
%
%  Output:
%
%    real A(N+1,N+1), the Bernstein-to-Legendre matrix.
%
  a = zeros ( n + 1, n + 1 );

  for i = 0 : n
    for j = 0 : n
      for k = 0 : i
        a(i+1,j+1) = a(i+1,j+1) ...
          + r8_mop ( i + k ) * nchoosek ( i, k ) ^ 2 ...
          / nchoosek ( n + i, j + k );
      end
      a(i+1,j+1) = a(i+1,j+1) * nchoosek ( n, j ) ...
        * ( 2 * i + 1 ) / ( n + i + 1 );
    end
  end

  return
end
