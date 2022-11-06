function ellipse_area1_test ( )

%*****************************************************************************80
%
%% ellipse_area1_test() tests ellipse_area1().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 April 2022
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_area1_test():\n' );
  fprintf ( 1, '  ellipse_area1() computes the area of an ellipse.\n' );

  r = 10.0;
  A = [ 5.0, 1.0; ...
        1.0, 2.0 ];
  area = ellipse_area1 ( A, r );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  R = %g\n', r );
  fprintf ( 1, '  Matrix A in ellipse definition x*A*x=r^2:\n' );
  disp ( A );
  fprintf ( 1, '  Area = %g\n', area );

  return
end
