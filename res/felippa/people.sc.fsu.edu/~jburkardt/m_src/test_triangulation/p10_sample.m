function point = p10_sample ( m, n )

%*****************************************************************************80
%
%% p10_sample() samples points from the region in problem 10.
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
%  Output:
%
%    real POINT(M,N), the coordinates of the points.
%
  x1 =  0.0;
  x2 = +1.0;
  y1 =  0.0;
  y2 = +1.0;
%
%  Generate a batch of points in [0,1]x[0,1].
%
  point = rand ( m, n );
%
%  Remap the points to the box [X1,X2] x [Y1,Y2].
%
  point(1,1:n) = x1 + point(1,1:n) * ( x2 - x1 );
  point(2,1:n) = y1 + point(2,1:n) * ( y2 - y1 );

  return
end
