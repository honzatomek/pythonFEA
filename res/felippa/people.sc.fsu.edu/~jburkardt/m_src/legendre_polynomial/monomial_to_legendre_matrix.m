function a = monomial_to_legendre_matrix ( n )

%*****************************************************************************80
%
%% monomial_to_legendre_matrix(): convert monomial to Legendre basis.
%
%  Discussion:
%
%    If PM(x) is a linear combination of monomials
%    with coefficients CM, then PL(x) is a linear combination of
%    Legendre polynomials with coefficients CL = A * CM.
%    
%    Note that we assume the coefficients are ordered such that
%    the constant term is first.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    17 November 2019
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of A.
%
%  Output:
%
%    real A(N,N), the matrix.
%
  a = zeros ( n, n );

  a(1,1) = 1.0;

  if ( n == 1 )
    return
  end

  a(2,2) = 1.0;

  if ( n == 2 )
    return
  end

  for j = 3 : n
    for i = 1 : n
      if ( i == 1 )

        a(i,j) = (     i     ) * a(i+1,j-1) / ( 2 * i + 1 );

      elseif ( i < n )

        a(i,j) = (     i - 1 ) * a(i-1,j-1) / ( 2 * i - 3 ) ...
               + (     i     ) * a(i+1,j-1) / ( 2 * i + 1 );

      else

        a(i,j) = (     i - 1 ) * a(i-1,j-1) / ( 2 * i - 3 );

      end
    end
  end


  return
end
