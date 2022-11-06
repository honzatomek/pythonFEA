function [ a, more ] = vec_colex_next2 ( dim_num, base, a, more )

%*****************************************************************************80
%
%% vec_colex_next2() generates vectors in colex order.
%
%  Discussion:
%
%    The vectors are produced in colexical order, starting with
%    (0,0,...,0),
%    (1,0,...,0),
%    ...
%    (BASE(1)-1,BASE(2)-1,...,BASE(DIM_NUM)-1).
%
%  Example:
%
%    DIM_NUM = 2, 
%    BASE = [ 3, 3]
%
%    0   0
%    1   0
%    2   0
%    0   1
%    1   1
%    2   1
%    0   2
%    1   2
%    2   2
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 November 2007
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Dennis Stanton, Dennis White,
%    Constructive Combinatorics,
%    Springer, 1986,
%    ISBN: 0387963472,
%    LC: QA164.S79.
%
%  Input:
%
%    integer DIM_NUM, the spatial dimension.
%
%    integer BASE(DIM_NUM), the base to be used in each dimension.
%
%    integer A(DIM_NUM), except on the first call, this should
%    be the output value of A on the last call.
%
%    logical MORE, should be FALSE on the first call, and
%    thereafter should be the output value of MORE from the previous call.  
%
%  Output:
%
%    integer A(DIM_NUM), the next vector.
%
%    logical MORE, is TRUE if another vector was computed.
%    If MORE is FALSE on return, then ignore the output value A, and
%    stop calling the routine.
%
  if ( ~ more )

    a(1:dim_num) = 0;
    more = true;

  else
      
    for i = 1 : dim_num

      a(i) = a(i) + 1;

      if ( a(i) < base(i) )
        return
      end

      a(i) = 0;

    end

    more = false;

  end

  return
end
