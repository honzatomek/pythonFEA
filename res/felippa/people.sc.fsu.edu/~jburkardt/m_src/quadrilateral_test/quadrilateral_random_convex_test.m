function quadrilateral_random_convex_test ( )

%*****************************************************************************80
%
%% quadrilateral_random_convex_test() tests quadrilateral_random_convex().
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
  fprintf ( 1, 'quadrilateral_random_convex_test():\n' );
  fprintf ( 1, '  quadrilateral_random_convex() returns a random convex quadrilateral\n' );
  fprintf ( 1, '  within the unit square.\n' );
  fprintf ( 1, '\n' );

  q = quadrilateral_random_convex ( );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );

  return
end
