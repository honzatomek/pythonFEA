function ellipse_area2_test ( )

%*****************************************************************************80
%
%% ellipse_area2_test() tests ellipse_area2().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 November 2016
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_area2_test():\n' );
  fprintf ( 1, '  ellipse_area2() computes the area of an ellipse.\n' );

  a = 5.0;
  b = 2.0;
  c = 2.0;
  d = 10.0;

  area = ellipse_area2 ( a, b, c, d );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Ellipse: %g * x^2 + %g * xy + %g * y^2 = %g\n', a, b, c, d );
  fprintf ( 1, '  Area = %g\n', area );

  return
end
