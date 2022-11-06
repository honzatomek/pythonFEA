function tetrahedron_solid_angles_test ( )

%*****************************************************************************80
%
%% tetrahedron_solid_angles_test() tests tetrahedron_solid_angles();
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
  fprintf ( 1, 'tetrahedron_solid_angles_test():\n' );
  fprintf ( 1, '  tetrahedron_solid_angles() computes the solid angles\n' );
  fprintf ( 1, '  associated with the vertices of a tetrahedron.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron1 vertices:\n' );
  disp ( t1 );
  angle = tetrahedron_solid_angles ( t1 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  solid angles:\n' );
  disp ( angle );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron2 vertices:\n' );
  disp ( t2 );
  angle = tetrahedron_solid_angles ( t2 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  solid angles:\n' );
  disp ( angle );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron3 vertices:\n' );
  disp ( t3 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  solid angles:\n' );
  disp ( angle );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron4 vertices:\n' );
  disp ( t4 );
  angle = tetrahedron_solid_angles ( t4 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  solid angles:\n' );
  disp ( angle );

  return
end
