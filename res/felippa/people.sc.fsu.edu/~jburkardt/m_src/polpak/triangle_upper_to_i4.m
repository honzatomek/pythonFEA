function k = triangle_upper_to_i4 ( i, j )

%*****************************************************************************80
%
%% triangle_upper_to_i4() converts an upper triangular coordinate to an integer.
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
%    (1,1) (1,2) (2,2) (1,3) (2,3) (3,3) (1,4) (2,4) (2,4) (4,4)
%
%    Here, the quantities in parenthesis represent the natural row and
%    column indices of a single number when stored in a rectangular array.
%
%    Thus, our goal is, given the row I and column J of the data,
%    to produce the value K which indicates its position in the linear
%    array.
%
%    The triangular numbers are the indices associated with the
%    diagonal elements of the original array, T(1,1), T(2,2), T(3,3)
%    and so on.
%
%  Formula:
%
%    K = I + ( (J-1) * J ) / 2
%
%  First Values:
%
%     I  J  K
%
%     0  0  0
%     1  1  1
%     1  2  2
%     2  2  3
%     1  3  4
%     2  3  5
%     3  3  6
%     1  4  7
%     2  4  8
%     3  4  9
%     4  4 10
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
%    integer I, J, the row and column indices.  I and J must
%    be nonnegative, and I must not be greater than J.
%
%  Output:
%
%    integer K, the linear index of the (I,J) element.
%
  if ( i < 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TRIANGLE_UPPER_TO_I4 - Fatal error!\n' );
    fprintf ( 1, '  I < 0.\n' );
    fprintf ( 1, '  I = %d\n', i );
    error ( 'TRIANGLE_UPPER_TO_I4 - Fatal error!' );
  elseif ( j < 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TRIANGLE_UPPER_TO_I4 - Fatal error!\n' );
    fprintf ( 1, '  J < 0.\n' );
    fprintf ( 1, '  J = %d\n', j );
    error ( 'TRIANGLE_UPPER_TO_I4 - Fatal error!' );
  elseif ( j < i )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TRIANGLE_UPPER_TO_I4 - Fatal error!\n' );
    fprintf ( 1, '  J < I.\n' );
    fprintf ( 1, '  I = %d\n', i );
    fprintf ( 1, '  J = %d\n', j );
    error ( 'TRIANGLE_UPPER_TO_I4 - Fatal error!' );
  end

  k = i + ( ( j - 1 ) * j ) / 2;

  return
end
