function a = legendre_to_monomial_matrix ( n )

%*****************************************************************************80
%
%% legendre_to_monomial_matrix(): Legendre coefficient conversion matrix.
%
%  Discussion:
%
%    If PL(x) is a linear combination of Legendre polynomials
%    with coefficients CL, then PM(x) is a linear combination of
%    monomials with coefficients CM = A * CL.
%    
%    Note that we assume the coefficients are ordered such that
%    the constant term is first.
%
%  Example:
%
%    N = 11
%
%    A (transposed):
%
%     1    .     .     .      .     .      .      .       .     .     . / 1
%     .    1     .     .      .     .      .      .       .     .     . / 1
%    -1    .     3     .      .     .      .      .       .     .     . / 2
%     .   -3     .     5      .     .      .      .       .     .     . / 2
%     3    .   -30     .     35     .      .      .       .     .     . / 8
%     .   15     .   -70      .    63      .      .       .     .     . / 8
%    -5    .   105     .   -315     .    231      .       .     .     . / 16
%     .  -35     .   315      .  -693      .    429       .     .     . / 16
%    35    . -1260     .   6930     . -12012      .    6435     .     . / 128
%     .  315     . -4620      . 18018      . -25740       . 12155     . / 128
%   -63    .  3465     . -30030     .  90090      . -109395     . 46189 / 256
%
%  Properties:
%
%    A is generally not symmetric: A' /= A.
%
%    A is lower triangular.
%
%    The elements of each row sum to 1.
%
%    Because it has a constant row sum of 1,
%    A has an eigenvalue of 1, and
%    a (right) eigenvector of ( 1, 1, 1, ..., 1 ).
%
%    A is reducible.
%
%    The diagonals form a pattern of zero, positive, zero, negative.
%
%    The family of matrices is nested as a function of N.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 June 2019
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
  if ( n <= 0 )
    a = [];
    return
  end

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
        a(i,j) = - ( j - 2 ) * a(i,j-2) ...
                 / ( j - 1 );
      else
        a(i,j) = ( ( 2 * j - 3 ) * a(i-1,j-1) ...
                 + (   - j + 2 ) * a(i,j-2) ) ...
                 / (     j - 1 );
      end
    end
  end

  return
end
