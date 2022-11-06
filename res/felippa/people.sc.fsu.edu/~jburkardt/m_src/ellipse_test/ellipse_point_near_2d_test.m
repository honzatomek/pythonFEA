function ellipse_point_near_2d_test ( )

%*****************************************************************************80
%
%% ellipse_point_near_2d_test() tests ellipse_point_near_2d().
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
  a = 3.0;
  b = 2.0;
  n = 10;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_point_near_2d_test():\n' );
  fprintf ( 1, '  ellipse_point_near_2d() is given a point P, and\n' );
  fprintf ( 1, '  finds the nearest point PN on an ellipse in 2D.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  The ellipse is (X/A)^2 + (Y/B)^2 = 1\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  A = %f\n', a );
  fprintf ( 1, '  B = %f\n', b );
  fprintf ( 1, '\n' );
  fprintf ( 1, '           P                PN\n' );
  fprintf ( 1, '\n' );

  for i = -3 : n + 3

    p(1) = ( ( n - i ) * 0.0 + i * 4.0 ) / n;

    p(2) = ( ( n - i ) * 3.0 + i * 0.0 )  / n;

    pn = ellipse_point_near_2d ( a, b, p );

    fprintf ( 1, '  %10f  %10f    %10f  %10f\n', p(1:2), pn(1:2) );

  end

  return
end
