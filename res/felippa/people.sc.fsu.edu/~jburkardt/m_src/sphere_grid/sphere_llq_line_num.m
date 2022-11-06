function line_num = sphere_llq_line_num ( lat_num, long_num )

%*****************************************************************************80
%
%% sphere_llq_line_num() counts lines for a latitude/longitude quadrilateral grid.
%
%  Discussion:
%
%    The number returned is the number of pairs of points to be connected.
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
%    integer LAT_NUM, LONG_NUM, the number of latitude and
%    longitude lines to draw.  The latitudes do not include the North and South
%    poles, which will be included automatically, so LAT_NUM = 5, for instance,
%    will result in points along 7 lines of latitude.
%
%  Output:
%
%    integer LINE_NUM, the number of grid lines.
%
  line_num = long_num * ( lat_num + 1 ) ...
           + lat_num * long_num;

  return
end
