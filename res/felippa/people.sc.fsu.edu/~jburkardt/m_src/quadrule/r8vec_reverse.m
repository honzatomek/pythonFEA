function a = r8vec_reverse ( n, a )

%*****************************************************************************80
%
%% r8vec_reverse() reverses the elements of an R8VEC.
%
%  Example:
%
%    Input:
%
%      N = 5, A = ( 11.0, 12.0, 13.0, 14.0, 15.0 ).
%
%    Output:
%
%      A = ( 15.0, 14.0, 13.0, 12.0, 11.0 ).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of entries in the array.
%
%    real A(N), the array to be reversed.
%
%  Output:
%
%    real A(N), the reversed array.
%
  a(1:n) = a(n:-1:1);

  return
end
