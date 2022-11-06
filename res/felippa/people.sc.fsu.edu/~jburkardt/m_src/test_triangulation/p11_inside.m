function inside = p11_inside ( m, n, point )

%*****************************************************************************80
%
%% p11_inside reports if a point is inside the region in problem 11.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 April 2009
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
%    real POINT(M,N), the coordinates of the points.
%
%  Output:
%
%    logical INSIDE(N), is TRUE if the point is in the region.
%
  inside(1:n) =                                          ...
       ( 0.0 <= point(1,1:n) && point(1,1:n) <= 0.5 &&     ...
         0.0 <= point(2,1:n) && point(2,1:n) <= 1.0    )  ...
    |                                                    ...
       ( 0.5 <= point(1,1:n) && point(1,1:n) <= 1.0 &&     ...
         0.0 <= point(2,1:n) && point(2,1:n) <= 0.5 );       

  return
end
