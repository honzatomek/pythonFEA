function value = triangle_is_degenerate_nd ( dim_num, t )

%*****************************************************************************80
%
%% triangle_is_degenerate_nd() finds if a triangle is degenerate in ND.
%
%  Discussion:
%
%    A triangle in ND is described by the coordinates of its 3 vertices.
%
%    A triangle in ND is degenerate if any two vertices are equal.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the spatial dimension.
%
%    real T(DIM_NUM,3), the triangle vertices.
%
%  Output:
%
%    logical VALUE, is TRUE if the triangle is degenerate.
%
  value = ( ( t(1:dim_num,1) == t(1:dim_num,2) ) | ...
            ( t(1:dim_num,2) == t(1:dim_num,3) ) | ...
            ( t(1:dim_num,3) == t(1:dim_num,1) ) );

  return
end
