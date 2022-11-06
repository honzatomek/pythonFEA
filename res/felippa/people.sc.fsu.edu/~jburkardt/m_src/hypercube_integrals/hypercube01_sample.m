function x = hypercube01_sample ( m, n )

%*****************************************************************************80
%
%% hypercube01_sample() samples points in the unit hypercube in M dimensions.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    18 January 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the spatial dimension.
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(M,N), the points.
%
  x = rand ( m, n );

  return
end
