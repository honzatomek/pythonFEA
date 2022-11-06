function quadrilateral_perimeter_test ( )

%*****************************************************************************80
%
%% quadrilateral_perimeter_test() tests quadrilateral_perimeter().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 April 2022
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'quadrilateral_perimeter_test():\n' );
  fprintf ( 1, '  quadrilateral_perimeter() computes the perimeter of\n' );
  fprintf ( 1, '  a quadrilateral.\n' );

  q = [ ...
    0.0, 1.0, 1.0, 0.0; ...
    0.0, 0.0, 1.0, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral:\n' );
  disp ( q );
  value = quadrilateral_perimeter ( q );
  fprintf ( 1, '  quadrilateral_perimeter = %g\n', value );

  q = [ ...
    0.0, 1.0, 2.0, 0.0; ...
    0.0, 0.0, 1.0, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral:\n' );
  disp ( q );
  value = quadrilateral_perimeter ( q );
  fprintf ( 1, '  quadrilateral_perimeter = %g\n', value );

  q = [ ...
    0.0, 1.0, 0.25, 0.0; ...
    0.0, 0.0, 0.25, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral:\n' );
  disp ( q );
  value = quadrilateral_perimeter ( q );
  fprintf ( 1, '  quadrilateral_perimeter = %g\n', value );

  q = [ ...
    0.0, 1.0, -0.5, 0.0; ...
    0.0, 0.0,  0.5, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral:\n' );
  disp ( q );
  value = quadrilateral_perimeter ( q );
  fprintf ( 1, '  quadrilateral_perimeter = %g\n', value );

  return
end
