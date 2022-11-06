function l4vec = l4vec_next ( n, l4vec )

%*****************************************************************************80
%
%% l4vec_next() generates the next logical vector.
%
%  Discussion:
%
%    In the following discussion, we will let '0' stand for FALSE and
%    '1' for TRUE.
%
%    The vectors have the order
%
%      (0,0,...,0),
%      (0,0,...,1),
%      ...
%      (1,1,...,1)
%
%    and the "next" vector after (1,1,...,1) is (0,0,...,0).  That is,
%    we allow wrap around.
%
%    Since MATLAB does not support a LOGICAL type, we end up actually
%    simply manipulating integers.
%
%  Example:
%
%    N = 3
%
%    Input      Output
%    -----      ------
%    0 0 0  =>  0 0 1
%    0 0 1  =>  0 1 0
%    0 1 0  =>  0 1 1
%    0 1 1  =>  1 0 0
%    1 0 0  =>  1 0 1
%    1 0 1  =>  1 1 0
%    1 1 0  =>  1 1 1
%    1 1 1  =>  0 0 0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 May 2008
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the dimension of the vectors.
%
%    logical L4VEC(N), the vector whose successor is desired.
%
%  Output:
%
%    logical L4VEC(N), the successor to the input vector.
%
  for i = n : -1 : 1

    if ( l4vec(i) == 0 )
      l4vec(i) = 1;
      return
    end

    l4vec(i) = 0;

  end

  return
end

