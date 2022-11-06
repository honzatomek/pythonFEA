function tetrahedron_circumsphere_test ( )

%*****************************************************************************80
%
%% tetrahedron_circumsphere_test() tests tetrahedron_circumsphere();
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
  fprintf ( 1, 'tetrahedron_circumsphere_test():\n' );
  fprintf ( 1, '  tetrahedron_circumsphere() computes the circumsphere\n' );
  fprintf ( 1, '  of a tetrahedron.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron vertices:\n' );
  disp ( tetra );

  [ r, center ] = tetrahedron_circumsphere ( tetra  );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron circumsphere center:\n' );
  disp ( center );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Circumsphere radius is %f\n', r );
 
  return
end
