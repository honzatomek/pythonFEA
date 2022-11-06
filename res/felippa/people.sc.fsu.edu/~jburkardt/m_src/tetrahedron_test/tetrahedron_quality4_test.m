function tetrahedron_quality4_test ( )

%*****************************************************************************80
%
%% tetrahedron_quality4_test() tests tetrahedron_quality4();
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    17 October 2005
%
%  Author:
%
%    John Burkardt
%
  dim_num = 3;
  test_num = 2;

  tetra_test = reshape ( [ ...
     0.577350269189626,  0.0, 0.0, ...
    -0.288675134594813,  0.5, 0.0, ...
    -0.288675134594813, -0.5, 0.816496580927726, ...
     0.0,                0.0, 0.816496580927726, ...
     0.577350269189626,  0.0, 0.0, ...
    -0.288675134594813,  0.5, 0.0, ...
    -0.288675134594813, -0.5, 0.0, ...
     0.0,                0.0, 0.408248290463863 ], ...
    dim_num, 4, test_num );

  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_quality4_test():\n' );
  fprintf ( 1, '  tetrahedron_quality4() computes quality measure #4\n' );
  fprintf ( 1, '  for a tetrahedron.\n' );


  for test = 1 : test_num

    tetra(1:dim_num,1:4) = tetra_test(1:dim_num,1:4,test);

    fprintf ( 1, '\n' );
    fprintf ( 1, '  tetrahedron vertices:\n' );
    disp ( tetra );

    quality4 = tetrahedron_quality4 ( tetra );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Tetrahedron quality is %f\n', quality4 );

  end

  return
end
