function [ a, degree ] = i4_to_i4poly ( intval, base )

%*****************************************************************************80
%
%% i4_to_i4poly() converts an integer to an integer polynomial in a given base.
%
%  Example:
%
%    INTVAL  BASE  Degree     A (in reverse order!)
%
%         1     2       0     1
%         6     2       2     1  1  0
%        23     2       4     1  0  1  1  1
%        23     3       2     2  1  2
%        23     4       2     1  1  3
%        23     5       1     4  3
%        23     6       1     3  5
%        23    23       1     1  0
%        23    24       0    23
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 May 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer INTVAL, an integer to be converted.
%
%    integer BASE, the base, which should be greater than 1.
%
%  Output:
%
%    integer A(1:DEGREE+1), contains the coefficients
%    of the polynomial expansion of INTVAL in base BASE.
%
%    integer DEGREE, the degree of the polynomial.
%
  j = abs ( floor ( intval ) );

  degree = 0;

  a(degree+1) = mod ( j, base );

  j = j - a(degree+1);
  j = floor ( j / base );

  while ( 0 < j )

    degree = degree + 1;

    a(degree+1) = mod ( j, base );

    j = j - a(degree+1);
    j = floor ( j / base );

  end

  if ( intval < 0 )
    a(1:degree+1) = -a(1:degree+1);
  end

  return
end
