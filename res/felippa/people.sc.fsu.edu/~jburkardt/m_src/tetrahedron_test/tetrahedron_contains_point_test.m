function tetrahedron_contains_point_test ( )

%*****************************************************************************80
%
%% tetrahedron_contains_point_test() tests tetrahedron_contains_point().
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
  fprintf ( 1, 'tetrahedron_contains_point_test():\n' );
  fprintf ( 1, '  tetrahedron_contains_point() finds if a point\n' );
  fprintf ( 1, '  is inside a tetrahderon;\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron vertices:\n' );
  disp ( tetra );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  P, Inside_Tetra?\n' );
  fprintf ( 1, '\n' );
%
%  Test 1
%
  c(1:4) = [ 0.0, 0.1, 0.2, 0.7 ];

  p(1:dim_num) = ( tetra(1:dim_num,1:4) * c(1:4)' )';

  inside = tetrahedron_contains_point ( tetra, p );

  fprintf ( 1, '  %12f  %12f  %12f  %1d\n', p(1:dim_num), inside );
%
%  Test 2
%
  c(1:4) = [ -1.3, 2.0, 0.2, 0.1 ];

  p(1:dim_num) = ( tetra(1:dim_num,1:4) * c(1:4)' )';

  inside = tetrahedron_contains_point ( tetra, p );

  fprintf ( 1, '  %12f  %12f  %12f  %1d\n', p(1:dim_num), inside );
%
%  Test 3
%
  c(1:4) = [ 0.8, 0.6, -0.5, 0.1 ];

  p(1:dim_num) = ( tetra(1:dim_num,1:4) * c(1:4)' )';

  inside = tetrahedron_contains_point ( tetra, p );

  fprintf ( 1, '  %12f  %12f  %12f  %1d\n', p(1:dim_num), inside );

  return
end
