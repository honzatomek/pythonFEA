function [ a, b, c ] = pythag_triple_ijk ( i, j, k )

%*****************************************************************************80
%
%% pythag_triple_ijk() computes any primitive Pythagorean triple.
%
%  Example:
%
%     I       J       K       A       B       C    A^2+B^2 = C^2
%
%     0       0       0       3       4       5        25
%     0       0       1       5      12      13       169
%     0       1       0      15       8      17       289
%     1       0       0      20      21      29       841
%     0       2       1     117      44     125     15625
%     1       1       1     207     224     305     93025
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    17 October 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    John D Cook,
%    Generating all primitive Pythagorean triples using linear algebra,
%    https://www.johndcook.com/blog/2020/10/16/primitive-pythagorean-triples/
%
%  Input:
%
%    integer I, J, K, the matrix powers.
%    These should be nonnegative.  Otherwise, some entries of A, B, C may
%    be negative.
%
%  Output:
%
%    integer A, B, C, the corresponding primitive Pythagorean triple.
%    A, B, and C are positive integers which have no common factors,
%    and A^2 + B^2 = C^2.
%
  M1 = [  1,  2,  2; ...
          2,  1,  2; ...
          2,  2,  3 ];

  M2 = [ -1,  2,  2; ...
         -2,  1,  2; ...
         -2,  2,  3 ];

  M3 = [  1, -2,  2; ...
          2, -1,  2; ...
          2, -2,  3 ];

  abc = [ 3; 4; 5 ];

  ABC = M1^i * M2^j * M3^k * abc;

  a = ABC(1);
  b = ABC(2);
  c = ABC(3);

  return
end
