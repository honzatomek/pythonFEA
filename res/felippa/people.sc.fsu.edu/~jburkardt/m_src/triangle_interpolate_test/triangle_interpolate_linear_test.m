function triangle_interpolate_linear_test ( )

%*****************************************************************************80
%
%% triangle_interpolate_linear_test() tests triangle_interpolate_linear().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    19 January 2016
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_interpolate_linear_test():\n' );
  fprintf ( 1, '  triangle_interpolate_linear() uses linear interpolation\n' );
  fprintf ( 1, '  on vertex data to estimate values in the interior.\n' );
%
%  Define the triangle corners.
%
  p1 = [ 0.0; 1.0 ];
  p2 = [ 5.0; 0.0 ];
  p3 = [ 4.0; 4.0 ];
%
%  Set the corner colors to R, G and B.
%
  v1 = [ 1.0; 0.0; 0.0 ];
  v2 = [ 0.0; 1.0; 0.0 ];
  v3 = [ 0.0; 0.0; 1.0 ];
%
%  Display the vertices with their colors.
%
  clf ();
  hold ( 'on' );
  grid ( 'on' );
  patch ( [ p1(1), p2(1), p3(1) ], [ p1(2), p2(2), p3(2) ], 'w' );

  n = 3;
  p = [ p1, p2, p3 ];
  v = [ v1, v2, v3 ];

  for j = 1 : n
    plot ( p(1,j), p(2,j), 'o', 'markersize', 15, 'markerfacecolor', v(1:3,j) );
  end

  axis ( 'equal' );

  hold ( 'off' );
%
%  Save the image in a file.
%
  filename = 'vertex_colors.png';
  print ( '-dpng', filename );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  pause ( 5 )
%
%  Get N sample points inside the triangle.
%
  n = 50;
  p = uniform_in_triangle_map1 ( p1, p2, p3, n );
%
%  Add the three corner points to the list.
%
  n = n + 3;
  p = [ p, p1, p2, p3 ];
%
%  Request an interpolated value for R, G and B at each point.
%
  m = 3;
  v = triangle_interpolate_linear ( m, n, p1, p2, p3, p, v1, v2, v3 );
%
%  Display the sample points with their interpolated colors.
%
  clf ( );
  hold ( 'on' );
  grid ( 'on' );
  patch ( [ p1(1), p2(1), p3(1) ], [ p1(2), p2(2), p3(2) ], 'w' );

  for j = 1 : n
    plot ( p(1,j), p(2,j), 'o', 'markersize', 15, 'markerfacecolor', v(1:3,j) );
  end

  axis ( 'equal' );

  hold ( 'off' );
%
%  Save the image in a file.
%
  filename = 'sample_colors.png';
  print ( '-dpng', filename );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Image saved in file "%s"\n', filename );

  pause ( 5 )
%
%  Now do the triangle with the 'patch' command.
%
  clf;
  hold on
  grid on
  v = [ 0, 1; 5, 0; 4, 4 ];
  f = [ 1, 2, 3 ];
  c = [ 6; 3; 0 ];
  patch('Faces',f,'Vertices',v,'FaceVertexCData',c,'FaceColor','interp');
  colormap ( 'jet' )
% colorbar
  axis equal

  hold ( 'off' );
%
%  Save the image in a file.
%
  filename = 'triangle_colors.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  pause ( 5 )

  return
end
