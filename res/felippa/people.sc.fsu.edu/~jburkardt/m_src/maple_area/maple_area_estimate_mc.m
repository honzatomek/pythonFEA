function estimate = maple_area_estimate_mc ( b, p_m, p_n, sample_num )

%*****************************************************************************80
%
%% maple_area_estimate_mc() estimates area using a Monte Carlo sample.
%
%  Discussion:
%
%    As a set of MATLAB coordinates, the leaf lies inside the box
%    [0,P_M] x [0,P_N].
%
%    Generate SAMPLE_NUM random points (X,Y) within this region.
%
%    For each point (X,Y), determine if it is inside or outside the polygon.
%
%    The percentage of inside points gives the relative area of the leaf.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 August 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer B(*,2), a list of pixels that form the boundary.
%
%    integer P_M, P_N, the number of rows and columns of pixels.
%
%    integer SAMPLE_NUM, the number of sample points.
%
%  Output:
%
%    real ESTIMATE, the relative area estimate.
%
  x = p_m * rand ( sample_num, 1 );
  y = p_n * rand ( sample_num, 1 );
  inside = inpolygon ( x, y, b(:,1), b(:,2) ); 
  estimate = sum ( inside ) / sample_num;

  return
end

