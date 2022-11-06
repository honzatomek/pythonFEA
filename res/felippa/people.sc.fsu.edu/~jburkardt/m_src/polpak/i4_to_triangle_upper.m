function [ i, j ] = i4_to_triangle_upper ( k )

%*****************************************************************************80
%
%% i4_to_triangle_upper() converts an integer to upper triangular coordinates.
%
%  Discussion:
%
%    Triangular coordinates are handy when storing a naturally triangular
%    array (such as the upper half of a matrix) in a linear array.
%
%    Thus, for example, we might consider storing
%
%    (1,1) (1,2) (1,3) (1,4)
%          (2,2) (2,3) (2,4)
%                (3,3) (3,4)
%                      (4,4)
%
%    as the linear array
%
%    (1,1) (1,2) (2,2) (1,3) (2,3) (3,3) (1,4) (2,4) (3,4) (4,4)
%
%    Here, the quantities in parenthesis represent the natural row and
%    column indices of a single number when stored in a rectangular array.
%
%    In this routine, we are given the location K of an item in the
%    linear array, and wish to determine the row I and column J
%    of the item when stored in the triangular array.
%
%  First Values:
%
%     K  I  J
%
%     0  0  0
%     1  1  1
%     2  1  2
%     3  2  2
%     4  1  3
%     5  2  3
%     6  3  3
%     7  1  4
%     8  2  4
%     9  3  4
%    10  4  4
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 March 2017
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer K, the linear index of the (I,J) element, which
%    must be nonnegative.
%
%  Output:
%
%    integer I, J, the row and column indices.
%
  if ( k < 0 )

    fprintf ( 1, '\n' );
    fprintf ( 1, 'I4_TO_TRIANGLE_UPPER - Fatal error!\n' );
    fprintf ( 1, '  K < 0.\n' );
    fprintf ( 1, '  K = %d\n', k );
    error ( 'I4_TO_TRIANGLE_UPPER - Fatal error!' );

  elseif ( k == 0 )

    i = 0;
    j = 0;
    return

  end

  j = floor ( sqrt ( 2 * k ) );

  if ( j * j + j < 2 * k )
    j = j + 1;
  end

  i = k - ( j * ( j - 1 ) ) / 2;

  return
end
