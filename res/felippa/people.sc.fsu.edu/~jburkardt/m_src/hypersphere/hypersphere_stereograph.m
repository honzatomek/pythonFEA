function x2 = hypersphere_stereograph ( m, n, x )

%*****************************************************************************80
%
%% hypersphere_stereograph() applies a stereographic map to points on a hypersphere.
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
%  Input:
%
%    integer M, the spatial dimension.
%    M must be at least 2.
%
%    integer N, the number of points.
%
%    real X(M,N), the points to be mapped.
%
%  Output:
%
%    real X2(M-1,N), the stereographically mapped points.
%
  x2 = x(1:m-1,1:n);
  d = 1.0 - x(m,1:n);
  x2 = x2 ./ repmat ( d, m-1, 1 );
  
  return
end

