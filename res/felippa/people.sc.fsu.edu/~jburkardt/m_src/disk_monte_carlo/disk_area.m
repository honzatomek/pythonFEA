function area = disk_area ( center, r )

%*****************************************************************************80
%
%% disk_area() returns the area of a disk.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    04 July 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real CENTER(2), the center of the disk. 
%    This information is not needed for the area calculation.
%
%    real R, the radius of the disk.
%
%  Output:
%
%    real AREA, the area of the unit disk.
%
  area = pi * r * r;

  return
end
