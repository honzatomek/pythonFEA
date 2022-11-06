function x = hypersphere_01_surface_uniform ( m, n )

%*****************************************************************************80
%
%% hypersphere_01_surface_uniform(): uniform unit hypersphere surface samples.
%
%  Discussion:
%
%    The sphere has center 0 and radius 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 May 2013
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    George Marsaglia,
%    Choosing a point from the surface of a sphere,
%    Annals of Mathematical Statistics,
%    Volume 43, Number 2, April 1972, pages 645-646.
%
%  Input:
%
%    integer M, the dimension of the space.
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(M,N), the points.
%
  x = randn ( m, n );
  v(1,1:n) = sqrt ( sum ( x.^2, 1 ) );
  vv = repmat ( v, m, 1 );
  x = x ./ vv;

  return
end
