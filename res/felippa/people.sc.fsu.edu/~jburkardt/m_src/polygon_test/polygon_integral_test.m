function polygon_integral_test ( )

%*****************************************************************************80
%
%% polygon_integral_test() tests polygon_integral_*().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 March 2022
%
%  Author:
%
%    John Burkardt
%
  n = 4;
  dim_num = 2;

  v = [ ...
    0.0, 0.0; ...
    1.0, 0.0; ...
    1.0, 1.0; ...
    0.0, 1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_integral_test():\n' );
  fprintf ( 1, '  polygon_integral_1  integrates 1 over a polygon.\n' );
  fprintf ( 1, '  polygon_integral_x  integrates x over a polygon.\n' );
  fprintf ( 1, '  polygon_integral_y  integrates y over a polygon.\n' );
  fprintf ( 1, '  polygon_integral_xx integrates x*x over a polygon.\n' );
  fprintf ( 1, '  polygon_integral_xy integrates x*y over a polygon.\n' );
  fprintf ( 1, '  polygon_integral_yy integrates y*y over a polygon.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  F(X,Y)    Integral\n' );
  fprintf ( 1, '\n' );

  result = polygon_integral_1 ( n, v );
  fprintf ( 1, '    1  %f\n', result );

  result = polygon_integral_x ( n, v );
  fprintf ( 1, '    X  %f\n', result );

  result = polygon_integral_y ( n, v );
  fprintf ( 1, '    Y  %f\n', result );

  result = polygon_integral_xx ( n, v );
  fprintf ( 1, '  X*X  %f\n', result );

  result = polygon_integral_xy ( n, v );
  fprintf ( 1, '  X*Y  %f\n', result );

  result = polygon_integral_yy ( n, v );
  fprintf ( 1, '  Y*Y  %f\n', result );

  return
end
