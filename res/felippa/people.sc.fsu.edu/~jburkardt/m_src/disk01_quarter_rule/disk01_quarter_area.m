function area = disk01_quarter_area ( )

%*****************************************************************************80
%
%% disk01_quarter_area() returns the area of the unit quarter disk.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    05 May 2016
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real AREA, the area of the unit quarter disk.
%
  r = 1.0;
  area = 0.25 * pi * r * r;

  return
end
