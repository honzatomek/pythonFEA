function tetrahedron_face_areas_test ( )

%*****************************************************************************80
%
%% tetrahedron_face_areas_test() tests tetrahedron_face_areas().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 April 2022
%
%  Author:
%
%    John Burkardt
%
  t1 = [ ...
     0.000000,  0.942809, -0.333333; ...
    -0.816496, -0.816496, -0.333333; ...
     0.816496, -0.816496, -0.333333; ...
     0.000000,  0.000000,  1.000000 ]';
  t2 = [ ...
     0.000000,  0.000000,  0.000000; ...
     1.000000,  0.000000,  0.000000; ...
     0.000000,  1.000000,  0.000000; ...
     0.000000,  0.000000,  1.000000 ]';
  t3 = [ ...
     0.000000,  0.000000,  0.000000; ...
     1.000000,  0.000000,  0.000000; ...
     0.000000,  2.000000,  0.000000; ...
     0.000000,  0.000000,  4.000000 ]';
  t4 = [ ...
     0.000000,  0.000000,  0.000000; ...
     1.000000,  0.000000,  0.000000; ...
     0.000000,  1.000000,  0.000000; ...
     1.000000,  1.000000,  1.000000 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_face_areas_test():\n' );
  fprintf ( 1, '  tetrahedron_face_areas() computes the areas of the\n' );
  fprintf ( 1, '  4 faces of a tetrahedron.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron1 vertices:\n' );
  disp ( t1 );
  areas = tetrahedron_face_areas ( t1 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Face areas:\n' );
  disp ( areas );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron2 vertices:\n' );
  disp ( t2 );
  areas = tetrahedron_face_areas ( t2 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Face areas:\n' );
  disp ( areas );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron3 vertices:\n' );
  disp ( t3 );
  areas = tetrahedron_face_areas ( t3 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Face areas:\n' );
  disp ( areas );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron4 vertices:\n' );
  disp ( t4 );
  areas = tetrahedron_face_areas ( t4 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Face areas:\n' );
  disp ( areas );

  return
end
