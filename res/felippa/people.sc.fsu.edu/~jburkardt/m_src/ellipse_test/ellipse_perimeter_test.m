function ellipse_perimeter_test ( )

%*****************************************************************************80
%
%% ellipse_perimeter_test() tests ellipse_perimeter().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 March 2021
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_perimeter_test():\n' );
  fprintf ( 1, '  ellipse_perimeter() computes the perimeter of an ellipse.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '      A      B      Ecc\n' );
  fprintf ( 1, '\n' );
  a = 1.0;
  n = 10;
  for i = 0 : n
    b = i / n;
    p = ellipse_perimeter ( a, b );
    fprintf ( 1, '  %5.1f  %5.1f  %10.6f\n', a, b, p );
  end

  return
end
