function tetrahedron_volume_test ( )

%*****************************************************************************80
%
%% tetrahedron_volume_test() tests tetrahedron_volume().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  dim_num = 3;

  tetra = [ ...
     0.000000,  0.942809, -0.333333; ...
    -0.816496, -0.816496, -0.333333; ...
     0.816496, -0.816496, -0.333333; ...
     0.000000,  0.000000,  1.000000 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_volume_test():\n' );
  fprintf ( 1, '  tetrahedron_volume() computes the volume of a tetrahedron;\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron vertices:\n' );
  disp ( tetra );

  volume = tetrahedron_volume ( tetra );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Volume = %f\n', volume );

  return
end
