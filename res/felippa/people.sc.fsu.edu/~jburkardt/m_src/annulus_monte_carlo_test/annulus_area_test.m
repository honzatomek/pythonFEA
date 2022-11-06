function annulus_area_test ( )

%*****************************************************************************80
%
%% annulus_area_test() test annulus_area().
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
  fprintf ( 1, 'annulus_area_test()\n' );
  fprintf ( 1, '  annulus_area() computes the area of an annulus with\n' );
  fprintf ( 1, '  center = (CX,CY), inner radius R1 and outer radius R2.\n' );
  
  fprintf ( 1, '\n' );
  fprintf ( 1, '  (   CX        CY     )    R1         R2         Area\n' );
  fprintf ( 1, '\n' );

  for i = 1 : 10
    data = rand ( 4, 1 );
    center(1) = 10.0 * data(1) - 5.0;
    center(2) = 10.0 * data(2) - 5.0;
    r1 = data(3);
    r2 = r1 + data(4);
    area = annulus_area ( center, r1, r2 );
    fprintf ( 1, '  (%9.6f,%9.6f)  %9.6f  %9.6f  %9.6f\n', center(1:2), r1, r2, area );
  end

  return
end
