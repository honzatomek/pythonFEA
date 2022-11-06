function line = sphere_ll_lines ( nlat, nlong, line_num )

%*****************************************************************************80
%
%% sphere_ll_lines() produces lines on a latitude/longitude grid.
%
%  Discussion:
%
%    The point numbering system is the same used in SPHERE_LL_POINTS,
%    and that routine may be used to compute the coordinates of the points.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 October 2012
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NLAT, NLONG, the number of latitude and longitude
%    lines to draw.  The latitudes do not include the North and South
%    poles, which will be included automatically, so NLAT = 5, for instance,
%    will result in points along 7 lines of latitude.
%
%    integer LINE_NUM, the number of grid lines.
%
%  Output:
%
%    integer LINE(2,LINE_NUM), contains pairs of point indices for
%    line segments that make up the grid.
%
  l = 0;
  line = zeros ( 2, line_num );
%
%  "Vertical" lines.
%
  for j = 0 : nlong - 1

    old = 1;
    new = j + 2;
    l = l + 1;
    line(1:2,l) = [ old, new ]';

    for i = 1 : nlat - 1

      old = new;
      new = old + nlong;
      l = l + 1;
      line(1:2,l) = [ old, new ]';

    end

    old = new;
    l = l + 1;
    line(1:2,l) = [ old, 1 + nlat * nlong + 1 ]';

  end
%
%  "Horizontal" lines.
%
  for i = 1 : nlat

    new = 1 + ( i - 1 ) * nlong + 1;

    for j = 0 : nlong - 2
      old = new;
      new = old + 1;
      l = l + 1;
      line(1:2,l) = [ old, new ]';
    end

    old = new;
    new = 1 + ( i - 1 ) * nlong + 1;
    l = l + 1;
    line(1:2,l) = [ old, new ]';

  end
%
%  "Diagonal" lines.
%
  for j = 0 : nlong - 1

    old = 1;
    new = j + 2;
    newcol = j;

    for i = 1 : nlat - 1

      old = new;
      new = old + nlong + 1;
      newcol = newcol + 1;
      if ( nlong - 1 < newcol )
        newcol = 0;
        new = new - nlong;
      end

      l = l + 1;
      line(1:2,l) = [ old, new ]';

    end

  end

  return
end

