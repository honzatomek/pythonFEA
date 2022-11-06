function ellipse_quadratic_to_standard_test ( )

%*****************************************************************************80
%
%% ellipse_quadratic_to_standard_test() tests ellipse_quadratic_to_standard().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    06 April 2022
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_quadratic_to_standard_test():\n' );
  fprintf ( 1, '  ellipse_quadratic_to_standard() converts the equation\n' );
  fprintf ( 1, '  of an ellipse from quadratic form:\n' );
  fprintf ( 1, '    Ax^2 + Bxy + Cy^2 + Dx + Ey + F = 0.\n' );
  fprintf ( 1, '  to standard form:\n' );
  fprintf ( 1, '    (   ( x - x0 ) cos ( theta ) + ( y - y0 ) sin ( theta ) ) / a^2\n' );
  fprintf ( 1, '  + ( - ( x - x0 ) sin ( theta ) + ( y - y0 ) cos ( theta ) ) / b^2\n' );
  fprintf ( 1, '  = 1\n' );

  A = 4.0;
  B = 0.0;
  C = 9.0;
  D = 8.0;
  E = - 36.0;
  F = 4.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Quadratic form parameters:\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  A = %g\n', A );
  fprintf ( 1, '  B = %g\n', B );
  fprintf ( 1, '  C = %g\n', C );
  fprintf ( 1, '  D = %g\n', D );
  fprintf ( 1, '  E = %g\n', E );
  fprintf ( 1, '  F = %g\n', F );

  [ a, b, theta, x0, y0 ] = ellipse_quadratic_to_standard ( A, B, C, D, E, F );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Standard form parameters:\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  a     = %g\n', a );
  fprintf ( 1, '  b     = %g\n', b );
  fprintf ( 1, '  theta = %g\n', theta );
  fprintf ( 1, '  x0    = %g\n', x0 );
  fprintf ( 1, '  y0    = %g\n', y0 );

  return
end


