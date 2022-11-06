function boundary = p00_boundary_nearest ( test, dim_num, n, point );

%*****************************************************************************80
%
%% p00_boundary_nearest() returns a nearest boundary point for any problem.
%
%  Discussion:
%
%    The given input point need not be inside the region.
%
%    In some cases, more than one boundary point may be "nearest",
%    but only one will be returned.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Per-Olof Persson, Gilbert Strang,
%    A Simple Mesh Generator in MATLAB,
%    SIAM Review,
%    Volume 46, Number 2, June 2004, pages 329-345.
%
%  Input:
%
%    integer TEST, the index of the test problem
%
%    integer DIM_num, the spatial dimension.
%
%    integer N, the number of points.
%
%    real POINT(DIM_num,N), the coordinates 
%    of the points.
%
%  Output:
%
%    real BOUNDARY(DIM_num,N), points on the boundary
%    that are nearest to each point.
%
  if ( test == 1 )
    boundary = p01_boundary_nearest ( dim_num, n, point );
  elseif ( test == 2 )
    boundary = p02_boundary_nearest ( dim_num, n, point );
  elseif ( test == 3 )
    boundary = p03_boundary_nearest ( dim_num, n, point );
  elseif ( test == 4 )
    boundary = p04_boundary_nearest ( dim_num, n, point );
  elseif ( test == 5 )
    boundary = p05_boundary_nearest ( dim_num, n, point );
  elseif ( test == 6 )
    boundary = p06_boundary_nearest ( dim_num, n, point );
  elseif ( test == 7 )
    boundary = p07_boundary_nearest ( dim_num, n, point );
  elseif ( test == 8 )
    boundary = p08_boundary_nearest ( dim_num, n, point );
  elseif ( test == 9 )
    boundary = p09_boundary_nearest ( dim_num, n, point );
  elseif ( test == 10 )
    boundary = p10_boundary_nearest ( dim_num, n, point );
  elseif ( test == 11 )
    boundary = p11_boundary_nearest ( dim_num, n, point );
  elseif ( test == 12 )
    boundary = p12_boundary_nearest ( dim_num, n, point );
  elseif ( test == 13 )
    boundary = p13_boundary_nearest ( dim_num, n, point );
  elseif ( test == 14 )
    boundary = p14_boundary_nearest ( dim_num, n, point );
  elseif ( test == 15 )
    boundary = p15_boundary_nearest ( dim_num, n, point );
  else
    fprintf ( 1, '\n' );
    fprintf ( 1, 'P00_boundary_nearest - Fatal error!\n' );
    fprintf ( 1, '  Input value of TEST is out of range.\n' );
    error ( 'P00_boundary_nearest - Fatal error!' )
  end

  return
end
