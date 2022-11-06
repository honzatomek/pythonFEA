function a = r8vec_indicator1 ( n )

%*****************************************************************************80
%
%% r8vec_indicator1() sets an R8VEC to the indicator vector (1,...,N).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of entries in the vector.
%
%  Output:
%
%    real A(N), the vector.
%
  a = ( 1 : n )';

  return
end

