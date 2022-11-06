function p2 = polygon_average ( n, p )

%*****************************************************************************80
%
%% polygon_average() takes one step of polygon averaging.
%
%  Discussion:
%
%    A polygon is represented by a list of N vertices.
%
%    An averaging step involves:
%    * replacing each vertex by the average of itself and its neighbor, 
%    * shifting these vertices to have centroid 0;
%    * scaling the X and Y coordinates separately so that the max-norms
%      of the X and Y coordinate vectors are both 1.
%
%    If the averaging process is carried out recursively, the resulting
%    polygon rapidly converges to an ellipse at a 45 degree tilt.
%
%    The direction of the tilt, and the ratio between the major and minor
%    axis lengths, vary depending on the initial polygon.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 April 2018
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Charles Van Loan, Daisy Fan,
%    Insight Through Computing,
%    SIAM Publishing, 2010,
%    ISBN: 978-0-898716-91-7
%
%    Adam Elmachtoub, Charles Van Loan,
%    From Random Polygon to Ellipse: An Eigenanalysis,
%    SIAM Review,
%    Volume 52, Number 1, March 2010, pages 151-170.
%
%  Input:
%
%    integer N, the number of vertices.
%
%    real P(N,2), the vertices of the polygon.
%
%  Output:
%
%    real P2(N,2), the vertices of the averaged polygon.
%

%
%  Average the vertices.
%
  p2 = ( p + [ p(2:n,1:2); p(1,1:2) ] ) / 2.0;
%
%  Compute the centroid.
%
  c = sum ( p ) / n;
%
%  Shift P2 so the centroid is zero.
%
  c2 = repmat ( c, n, 1 );
  p2 = p2 - c2;
%
%  Compute the max-norms of X and Y.
%
  x_norm2 = max ( abs ( p2(:,1) ) );
  y_norm2 = max ( abs ( p2(:,2) ) );
%
%  Scale P2 so X and Y have max-norm 1.
%
  p2(:,1) = p2(:,1) / x_norm2;
  p2(:,2) = p2(:,2) / y_norm2;

  return
end
 
