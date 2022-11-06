function polygon_centroid_test ( )

%*****************************************************************************80
%
%% polygon_centroid_test() tests polygon_centroid().
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

  v = [ ...
    1.0, 0.0; ...
    2.0, 1.0; ...
    1.0, 2.0; ...
    0.0, 1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_centroid_test:\n' );
  fprintf ( 1, '  polygon_centroid()   computes the centroid.\n' );
  fprintf ( 1, '  polygon_centroid_2() computes the centroid.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  centroid = polygon_centroid ( n, v );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon_centroid():\n' );
  disp ( centroid );

  centroid = polygon_centroid_2 ( n, v );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon_centroid2():\n' );
  disp ( centroid );

  return
end
