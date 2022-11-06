function [ cost_mu, cost_std, theta_mu, theta_std ] = hypersphere_angle_stats ( m, n )

%*****************************************************************************80
%
%% hypersphere_angle_stats() estimates unit hypersphere angle statistics.
%
%  Discussion:
%
%    Select two points at random on the unit hypersphere.
%    Compute the absolute value of the cosine of the angle between them.
%    Compute the angle corresponding to this nonnegative cosine.
%
%    Return mean and standard deviation of the cosine and angle statistics.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 January 2019
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the spatial dimension.
%
%    integer N, the number of sample points to use.
%
%  Output:
%
%    real COST_MU, COST_STD, the estimated mean and standard
%    deviation of the absolute value of the cosine of the angle between 
%    two random points on the unit hypersphere.
%
%    real THETA_MU, THETA_STD, the estimated mean and standard
%    deviation of the (nonnegative) angle between two random points on 
%    the unit hypersphere.  
%
  cost = zeros ( n, 1 );

  for i = 1 : n
    p = hypersphere_unit_sample ( m );
    q = hypersphere_unit_sample ( m );
    cost(i) = abs ( p' * q );
  end

  cost_mu = mean ( cost );
  cost_std = std ( cost );

  theta_mu = mean ( acos ( cost ) );
  theta_std = std ( acos ( cost ) );
  
  return
end
