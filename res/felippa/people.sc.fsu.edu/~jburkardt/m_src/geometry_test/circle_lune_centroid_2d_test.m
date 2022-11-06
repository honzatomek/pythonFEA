function circle_lune_centroid_2d_test ( )

%*****************************************************************************80
%
%% circle_lune_centroid_2d_test() tests circle_lune_centroid_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 July 2018
%
%  Author:
%
%    John Burkardt
%
  n_test = 12;

  r = 2.0;
  center(1:2,1) = [ 5.0; 3.0 ];
  theta1 = 0.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_lune_centroid_2d_test():\n' );
  fprintf ( 1, '  circle_lune_centroid_2d() computes the centroid of a\n' );
  fprintf ( 1, '  circular lune, defined by joining the endpoints\n' );
  fprintf ( 1, '  of a circular arc.\n' );

  circle_imp_print_2d ( r, center, '  The implicit circle:' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  The first angle of our lune is always 0.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  THETA2           X             Y\n' );
  fprintf ( 1, '\n' );

  for i = 0 : n_test

    theta2 = i * 2.0 * pi / n_test;

    centroid(1:2,1) = circle_lune_centroid_2d ( r, center, theta1, theta2 );

    fprintf ( 1, '  %12.8f  %12.8f  %12.8f\n', theta2, centroid(1:2,1) );

  end

  return
end
