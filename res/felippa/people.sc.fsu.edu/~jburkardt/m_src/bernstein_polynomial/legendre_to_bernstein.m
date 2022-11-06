function a = legendre_to_bernstein ( n )

%*****************************************************************************80
%
%% legendre_to_bernstein() returns the Legendre-to-Bernstein matrix.
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
%    real A(N+1,N+1), the Legendre-to-Bernstein matrix.
%
  a = zeros ( n + 1, n + 1 );

  for i = 0 : n
    for j = 0 : n
      for k = max ( 0, i + j - n ) : min ( i, j )
        a(i+1,j+1) = a(i+1,j+1) ...
          + r8_mop ( j + k ) * nchoosek ( j, k ) ^ 2 ...
          * nchoosek ( n - j, i - k );
      end
      a(i+1,j+1) = a(i+1,j+1) / nchoosek ( n, i );
    end
  end

  return
end
