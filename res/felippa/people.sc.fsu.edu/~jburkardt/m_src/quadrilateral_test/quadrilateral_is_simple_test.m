function quadrilateral_is_simple_test ( )

%*****************************************************************************80
%
%% quadrilateral_is_simple_test() tests quadrilateral_is_simple().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 August 2018
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'quadrilateral_is_simple_test():\n' );
  fprintf ( 1, '  quadrilateral_is_simple() is true if a quadrilateral is "simple",\n' );
  fprintf ( 1, '  that is, non-degenerate.\n' );

  q = [ ...
    0.0, 1.0, 1.0, 0.0; ...
    0.0, 0.0, 1.0, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );
  value = quadrilateral_is_simple ( q );
  fprintf ( 1, '  quadrilateral_is_simple ( q ) = %g\n', value );

  q = [ ...
    0.0, 1.0, 2.0, 0.0; ...
    0.0, 0.0, 1.0, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );
  value = quadrilateral_is_simple ( q );
  fprintf ( 1, '  quadrilateral_is_simple ( q ) = %g\n', value );

  q = [ ...
    0.0, 1.0, 0.25, 0.0; ...
    0.0, 0.0, 0.25, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );
  value = quadrilateral_is_simple ( q );
  fprintf ( 1, '  quadrilateral_is_simple ( q ) = %g\n', value );

  q = [ ...
    0.0, 1.0, -0.5, 0.0; ...
    0.0, 0.0,  0.5, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );
  value = quadrilateral_is_simple ( q );
  fprintf ( 1, '  quadrilateral_is_simple ( q ) = %g\n', value );

  return
end
