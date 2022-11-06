function quadrilateral_area_test ( )

%*****************************************************************************80
%
%% quadrilateral_area_test() tests quadrilateral_area();
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 May 2010
%
%  Author:
%
%    John Burkardt
%
  q = [ ...
    0.0, 0.0; ...
    1.0, 0.0; ...
    1.0, 1.0; ...
    0.0, 1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'quadrilateral_area_test():\n' );
  fprintf ( 1, '  quadrilateral_area  finds the area of a quadrilateral;\n' );
  fprintf ( 1, '  quadrilateral_area2 finds the area of a quadrilateral;\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );

  area = quadrilateral_area ( q );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral_area area is  %f\n', area );
 
  area = quadrilateral_area2 ( q );

  fprintf ( 1, '  quadrilateral_area2 area is %f\n', area );

  return
end
