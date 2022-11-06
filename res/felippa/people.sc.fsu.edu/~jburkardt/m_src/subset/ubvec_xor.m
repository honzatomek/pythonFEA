function bvec3 = ubvec_xor ( n, bvec1, bvec2 )

%*****************************************************************************80
%
%% ubvec_xor() computes the exclusive OR of two UBVEC's.
%
%  Discussion:
%
%    A UBVEC is an integer vector of binary digits, intended to
%    represent a nonnegative integer.  BVEC(1) is the units digit, BVEC(N)
%    is the coefficient of 2^(N-1).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 November 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the length of the vectors.
%
%    integer BVEC1(N), BVEC2(N), the binary vectors to be XOR'ed.
%
%  Output:
%
%    integer BVEC3(N), the exclusive OR of the two vectors.
%
  bvec3(1:n) = mod ( bvec1(1:n) + bvec2(1:n), 2 );

  return
end
