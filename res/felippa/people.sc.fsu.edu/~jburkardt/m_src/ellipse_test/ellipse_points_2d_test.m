function ellipse_points_2d_test ( )

%*****************************************************************************80
%
%% ellipse_points_2d_test() tests ellipse_points_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 July 2018
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_points_2d_test():\n' );
  fprintf ( 1, '  ellipse_points_2d() returns points along an ellipse.\n' );

  center = [ 2.0, 1.0 ];
  r1 = 2.0;
  r2 = 1.0;
  psi = pi / 6.0;
  n = 20;

  p = ellipse_points_2d ( center, r1, r2, psi, n );

  hold ( 'on' );
    plot ( p(1,:), p(2,:), 'r.', 'Markersize', 25 );
    p(1,n+1) = p(1,1);
    p(2,n+1) = p(2,1);
    plot ( p(1,:), p(2,:), 'b-' );
    grid ( 'on' );
    axis ( 'equal' );
    xlabel ( '<--- X --->' );
    ylabel ( '<--- Y --->' );
    title ( '  Points on an ellipse' );
  hold ( 'off' );

  filename = 'ellipse_points_2d_test.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  return
end
