function x = hypersphere01_sample ( m, n )

%*****************************************************************************80
%
%% hypersphere01_sample() uniformly samples the surface of the unit hypersphere.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    04 January 2014
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
  x = randn ( m, n );
  norm = ones ( 1, m ) * ( x.^2 );
  norm = sqrt ( norm );
  for i = 1 : m
    x(i,1:n) = x(i,1:n) ./ norm(1:n);
  end

  return
end
