function polygon_solid_angle_3d_test ( )

%*****************************************************************************80
%
%% polygon_solid_angle_3d_test() tests polygon_solid_angle_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 May 2015
%
%  Author:
%
%    John Burkardt
%
  dim_num = 3;
  test_num = 4;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_solid_angle_3d_test():\n' );
  fprintf ( 1, '  polygon_solid_angle_3d() computes the solid angle\n' );
  fprintf ( 1, '  subtended by a planar polygon in 3D as viewed from\n' );
  fprintf ( 1, '  a point P.\n' );

  for test = 1 : test_num
%
%  One eighth of sphere surface, on the unit sphere surface.
%
    if ( test == 1 )

      n = 3;

      v = [ ...
        1.0, 0.0, 0.0; ...
        0.0, 1.0, 0.0; ...
        0.0, 0.0, 1.0 ]';

      p(1:3,1) = [ 0.0; 0.0; 0.0 ];
%
%  Reverse order of vertices.
%
    elseif ( test == 2 )

      n = 3;

      v = [ ...
        1.0, 0.0, 0.0; ...
        0.0, 0.0, 1.0; ...
        0.0, 1.0, 0.0 ]';

      p(1:3,1) = [ 0.0; 0.0; 0.0 ];
%
%  One eighth of sphere surface, on the unit sphere surface, 
%  translated by (1,2,3).
%
    elseif ( test == 3 )

      n = 3;

      v = [ ...
        2.0, 2.0, 3.0; ...
        1.0, 3.0, 3.0; ...
        1.0, 2.0, 4.0 ]';

      p(1:3,1) = [ 1.0; 2.0; 3.0 ];
%
%  One eighth of sphere surface, but on sphere of radius 2.
%
    elseif ( test == 4 )

      n = 3;

      v = [ ...
        2.0, 0.0, 0.0; ...
        0.0, 2.0, 0.0; ...
        0.0, 0.0, 2.0 ]';

      p(1:3,1) = [ 0.0; 0.0; 0.0 ];

    end

    fprintf ( 1, '\n' );
    fprintf ( 1, '  TEST # %d\n', test );
    fprintf ( 1, '\n' );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  viewing point P:\n' );
    disp ( p );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  polygon vertices:\n' );
    disp ( v );

    solid_angle = polygon_solid_angle_3d ( n, v, p );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Solid angle subtended: %f\n', solid_angle );

  end

  return
end
