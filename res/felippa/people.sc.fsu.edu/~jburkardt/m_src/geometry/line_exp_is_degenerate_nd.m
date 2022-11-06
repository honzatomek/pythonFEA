function value = line_exp_is_degenerate_nd ( dim_num, p1, p2 )

%*****************************************************************************80
%
%% line_exp_is_degenerate_nd() finds if an explicit line is degenerate in ND.
%
%  Discussion:
%
%    The explicit form of a line in ND is:
%
%      the line through the points P1, P2.
%
%    An explicit line is degenerate if the two defining points are equal.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 November 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the spatial dimension.
%
%    real P1(DIM_NUM,1), P2(DIM_NUM,1), two points on the line.
%
%  Output:
%
%    logical VALUE, is TRUE if the line is degenerate.
%
  p1 = p1(:);
  p2 = p2(:);

  value = ( p1(1:dim_num,1) == p2(1:dim_num,1) );

  return
end
