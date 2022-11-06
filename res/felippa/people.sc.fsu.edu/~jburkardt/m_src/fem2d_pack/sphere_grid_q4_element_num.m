function element_num = sphere_grid_q4_element_num ( nelemx, nelemy )

%*****************************************************************************80
%
%% sphere_grid_q4_element_num() counts the elements in a Q4 sphere grid.
%
%  Example:
%
%    Input:
%
%      NELEMX = 3, NELEMY = 2
%
%    Output:
%
%      ELEMENT_NUM = NELEMX * NELEMY = 6
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 September 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NELEMX, NELEMY, the number of elements along the
%    X and Y directions. 
%
%  Output:
%
%    integer ELEMENT_NUM, the number of elements in the grid.
%
  element_num = nelemx * nelemy;

  return
end
