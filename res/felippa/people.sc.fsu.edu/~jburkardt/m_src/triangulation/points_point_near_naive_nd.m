function [ i_min, d_min ] = points_point_near_naive_nd ( dim_num, nset, ...
  pset, p )

%*****************************************************************************80
%
%% points_point_near_naive_2d() finds the nearest point to a given point in ND.
%
%  Discussion:
%
%    A naive algorithm is used.  The distance to every point is calculated,
%    in order to determine the smallest.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the spatial dimension.
%
%    integer NSET, the number of points in the set.
%
%    real PSET(DIM_NUM,NSET), the points in the set.
%
%    real P(DIM_NUM), the point whose nearest neighbor
%    is sought.
%
%  Output:
%
%    integer I_MIN, the index of the nearest point in PSET to P.
%
%    real D_MIN, the distance between P(*) and PSET(*,I_MIN).
%
  d_min = Inf;
  i_min = -1;

  for i = 1 : nset
    d = sum ( ( p(1:dim_num) - pset(1:dim_num,i)' ).^2 );
    if ( d < d_min )
      d_min = d;
      i_min = i;
    end
  end

  d_min = sqrt ( d_min );

  return
end
