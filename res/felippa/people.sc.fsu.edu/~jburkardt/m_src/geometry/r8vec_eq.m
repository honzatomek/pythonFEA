function value = r8vec_eq ( n, a1, a2 )

%*****************************************************************************80
%
%% r8vec_eq() is true if two R8VEC's are equal.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 August 2017
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of entries in the vectors.
%
%    real A1(N), A2(N), two vectors to compare.
%
%  Output:
%
%    logical VALUE, is true if every pair of elements A1(I)
%    and A2(I) are equal, and false otherwise.
%
  a1 = a1(:);
  a2 = a2(:);

  value = all ( a1(1:n) == a2(1:n) );

  return
end
