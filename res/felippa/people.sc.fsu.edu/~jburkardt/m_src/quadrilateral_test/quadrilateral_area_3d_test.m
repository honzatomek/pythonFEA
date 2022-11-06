function quadrilateral_area_3d_test ( )

%*****************************************************************************80
%
%% quadrilateral_area_3d_test() tests quadrilateral_area_3d_test();
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
    2.0, 2.0, 0.0; ...
    0.0, 0.0, 0.0; ...
    1.0, 1.0, 1.0; ...
    3.0, 3.0, 1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'quadrilateral_area_3d_test():\n' );
  fprintf ( 1, '  quadrilateral_area_3d() finds the area of a quadrilateral\n' );
  fprintf ( 1, '  in 3D.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );

  area = quadrilateral_area_3d ( q );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Area is     %f\n', area );

  t(1:3,1:3) = q(1:3,1:3);
  area1 = triangle_area_3d ( t );
  t(1:3,1:2) = q(1:3,3:4);
  t(1:3,  3) = q(1:3,1  );
  area2 = triangle_area_3d ( t );
  fprintf ( 1, '  Area by 2 calls to triangle_area_3d(): %f\n', area1 + area2 );

  return
end
