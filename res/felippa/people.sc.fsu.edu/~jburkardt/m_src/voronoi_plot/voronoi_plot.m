function voronoi_plot ( xy, m, n, p, filename )

%*****************************************************************************80
%
%% voronoi_plot() computes a pixel plot of a Voronoi diagram.
%
%  Discussion:
%
%    Rather than doing the difficult geometry, we simply discretize the
%    region into pixels, assign a color to each generator, and color a pixel
%    with the color of the nearest generator.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 September 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real XY(2,NC), (default=rand(2,25)), coordinates of center points.
%
%    integer M, (default=200), the number of rows of pixels
%    to use in the plot.  M = 100 might be reasonable for a start.
%
%    integer N, (default=M), the number of columns of pixels
%    to use in the plot.  N = 100 might be reasonable for a start.
%
%    real P, (default=2), the norm to be used.
%    P = 2, the default Euclidean or L2 norm.
%    P = Inf, the max or L-Infinity norm.
%    P = 1, the L1 norm.
%    Otherwise Norm(X,Y) = ( |X|^P + |Y|^P ) ^ (1/P)
%
  if ( nargin < 5 )
    filename = 'voronoi.png';
  end

  if ( nargin < 4 )
    p = 2;
  end

  if ( nargin < 3 )
    if ( nargin < 2 )
      n = 200;
    else
      n = m;
    end
  end

  if ( nargin < 2 )
    m = 200;
  end

  if ( nargin < 1 )
    xy = rand ( 2, 25 );
  end
%
%  How many points did we get?
%
  [ ~, nc ] = size ( xy );
%
%  Compute the range of the points.
%
  xmin = min ( xy(1,:) );
  xmax = max ( xy(1,:) );
  ymin = min ( xy(2,:) );
  ymax = max ( xy(2,:) );
%
%  Compute a margin, so the extreme points are not on the boundary.
%
  margin = 0.05 * max ( xmax - xmin, ymax - ymin );
%
%  Extend the region by the margin.
%
  xmin = xmin - margin;
  xmax = xmax + margin;
  ymin = ymin - margin;
  ymax = ymax + margin;
%
%  Randomly choose NC + 1 sets of RGB values.
%  Our extra color is black, just in case something goes wrong.
%
  rgb = uint8 ( floor ( 256 * rand ( nc + 1, 3 ) ) );

  rgb(nc+1,1:3) = 0;
%
%  Our picture A will be stored as an M x N array of RGB values
%  which are a special MATLAB data type of unsignted 8 bit integers.
%
  a = uint8 ( zeros ( m, n, 3 ) );
%
%  For each pixel in A, we calculate its corresponding XY position,
%  find the nearest center, and color the pixel with the corresponding
%  RGB color.  A vectorized calculation would be much faster.
%
  for i = 1 : m

    y = ( ( m - i     ) * ymax ...
        + (     i - 1 ) * ymin ) ...
        / ( m     - 1 );

    for j = 1 : n

      x = ( ( n - j     ) * xmax ...
          + (     j - 1 ) * xmin ) ...
          / ( n     - 1 );

      nearest = nc + 1;
      distsq_min = Inf;

      for k = 1 : nc

        if ( p == Inf )
          distsq = max ( abs ( x - xy(1,k) ), abs ( y - xy(2,k) ) );
        elseif ( p == 2 )
          distsq = ( x - xy(1,k) ) ^ 2 + ( y - xy(2,k) ) ^ 2;
        elseif ( p == 1 )
          distsq = abs ( x - xy(1,k) ) + abs ( y - xy(2,k) );
        else
          dx = abs ( x - xy(1,k) );
          dy = abs ( y - xy(2,k) );
          distsq = ( dx ^ p + dy ^ p ) ^ ( 1.0 / p );
        end

        if ( distsq < distsq_min )
          distsq_min = distsq;
          nearest = k;
        end
      end

      a(i,j,1:3) = rgb(nearest,1:3);

    end

  end
%
%  Mark the generators as black squares.
%
  for k = 1 : nc
    i = round ( ( n * ymax - 1 * ymin - ( n - 1 ) * xy(2,k) ) / ( ymax - ymin ) );
    j = round ( ( n * xmax - 1 * xmin - ( n - 1 ) * xy(1,k) ) / ( xmax - xmin ) );
    a(i-1:i+1,j-1:j+1,1:3) = 0;
  end
%
%  Display the image.
%
  figure ( )
  imagesc ( a )
  axis equal
  axis tight
  title ( sprintf ( 'Distance computed with %f norm', p ) );
  print ( '-dpng', filename );
  fprintf ( 1, '  Saved graphics in file "%s"\n', filename );

  return
end
 
