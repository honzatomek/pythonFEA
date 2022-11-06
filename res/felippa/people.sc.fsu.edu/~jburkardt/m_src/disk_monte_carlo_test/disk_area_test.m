function disk_area_test ( )

%*****************************************************************************80
%
%% disk_area_test() test disk_area().
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
  fprintf ( 1, '\n' );
  fprintf ( 1, 'disk_area_test():\n' );
  fprintf ( 1, '  DISK_AREA() computes the area of a disk with\n' );
  fprintf ( 1, '  center = (CX,CY) and radius R.\n' );
  
  fprintf ( 1, '\n' );
  fprintf ( 1, '  (   CX        CY     )    R          Area\n' );
  fprintf ( 1, '\n' );

  for i = 1 : 10
    data = rand ( 3, 1 );
    center(1) = 10.0 * data(1) - 5.0;
    center(2) = 10.0 * data(2) - 5.0;
    r = data(3);
    area = disk_area ( center, r );
    fprintf ( 1, '  (%9.6f,%9.6f)  %9.6f  %9.6f\n', center(1:2), r, area );
  end

  return
end
