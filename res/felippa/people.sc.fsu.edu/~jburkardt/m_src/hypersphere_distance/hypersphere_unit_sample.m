function x = hypersphere_unit_sample ( m )

%*****************************************************************************80
%
%% hypersphere_unit_sample returns sample points on the unit hypersphere.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    29 September 2018
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Russell Cheng,
%    Random Variate Generation,
%    in Handbook of Simulation,
%    edited by Jerry Banks,
%    Wiley, 1998, pages 168.
%
%    Reuven Rubinstein,
%    Monte Carlo Optimization, Simulation, and Sensitivity
%    of Queueing Networks,
%    Wiley, 1986, page 232.
%
%  Input:
%
%    integer M, the spatial dimension.
%
%  Output:
%
%    real X(M,1), the point.
%
  x = randn ( m, 1 );
%
%  Normalize the vector.
%
  x = x / norm ( x );

  return
end
