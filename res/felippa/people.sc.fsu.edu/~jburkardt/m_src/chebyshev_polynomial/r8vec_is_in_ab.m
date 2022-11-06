function value = r8vec_is_in_ab ( n, x, a, b )

%*****************************************************************************80
%
%% r8vec_is_in_ab() is true if the entries of an R8VEC are in the range [A,B].
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 March 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of entries in the vector.
%
%    real X(N), the vector.
%
%    real A, B, the limits, with A <= B.
%
%  Output:
%
%    logical VALUE, is true if every entry is between A and B.
%
  if ( any ( x(1:n) < a | b < x(1:n) ) )
    value = false;
  else
    value = true;
  end

  return
end
