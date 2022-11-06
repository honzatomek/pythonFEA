function polygon_centroid_3d_test ( )

%*****************************************************************************80
%
%% polygon_centroid_3d_test() tests polygon_centroid_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 February 2009
%
%  Author:
%
%    John Burkardt
%
  n = 4;
  dim_num = 3;

  v = [ ...
    1.0, 0.0, 0.0; ...
    2.0, 1.0, 1.0; ...
    1.0, 2.0, 1.0; ...
    0.0, 1.0, 0.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_centroid_3d_test():\n' );
  fprintf ( 1, '  polygon_centroid_3d() computes the centroid of a polygon\n' );
  fprintf ( 1, '  in 3D.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  centroid = polygon_centroid_3d ( n, v );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon centroid:\n' );
  disp ( centroid );
 
  return
end
