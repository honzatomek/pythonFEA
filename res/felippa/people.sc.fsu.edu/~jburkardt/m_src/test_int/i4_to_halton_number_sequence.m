function r = i4_to_halton_number_sequence ( key, base, n )

%*****************************************************************************80
%
%% i4_to_halton_number_sequence() computes the next N elements of a scalar Halton sequence.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 November 2009
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    John Halton,
%    On the efficiency of certain quasi-random sequences of points
%    in evaluating multi-dimensional integrals,
%    Numerische Mathematik,
%    Volume 2, pages 84-90, 1960.
%
%  Input:
%
%    integer key, the index of the desired element.
%    Only the absolute value of key is considered.
%    key = 0 is allowed, and returns R = 0.
%
%    integer BASE, the Halton base, which should be a prime number.
%    This routine only checks that BASE is greater than 1.
%
%    integer N, the number of elements desired.
%
%    real R(N), the key through key + N - 1
%    elements of the Halton sequence for base BASE.
%

%
%  Set key2 = ( key, key + 1, key + 2, ..., key + N - 1 )
%
  key2(1:n) = ( 1 : n ) + abs ( key ) - 1;

  if ( base <= 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'I4_TO_HALTON_NUMBER_SEQUENCE - Fatal error!\n' );
    fprintf ( 1, '  The input base BASE is <= 1!\n' );
    fprintf ( 1, '  BASE = %d\n', base );
    error ( 'I4_TO_HALTON_NUMBER_SEQUENCE - Fatal error!' );
  end

  base_inv = 1.0 / base;

  r(1:n) = 0.0;

  while ( any ( key2(1:n) ~= 0 ) )
    digit(1:n) = mod ( key2(1:n), base );
    r(1:n) = r(1:n) + digit(1:n) * base_inv;
    base_inv = base_inv / base;
    key2(1:n) = floor ( key2(1:n) / base );
  end

  return
end
