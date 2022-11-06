function tetrahedron_insphere_test ( )

%*****************************************************************************80
%
%% tetrahedron_insphere_test() tests tetrahedron_insphere().
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
     0.577350269189626,  0.0, 0.0; ...
    -0.288675134594813,  0.5, 0.0; ...
    -0.288675134594813, -0.5, 0.0; ...
     0.0,                0.0, 0.816496580927726 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_insphere_test():\n' );
  fprintf ( 1, '  tetrahedron_insphere() computes the insphere\n' );
  fprintf ( 1, '  of a tetrahedron.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron vertices:\n' );
  disp ( tetra );

  [ r, pc ] = tetrahedron_insphere ( tetra );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  insphere center:\n' );
  disp ( pc );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Insphere radius is %f\n', r );

  return
end
