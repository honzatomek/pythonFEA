function quadrilateral_is_convex_test ( )

%*****************************************************************************80
%
%% quadrilateral_is_convex_test() tests quadrilateral_is_convex().
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
  fprintf ( 1, 'quadrilateral_is_convex_test():\n' );
  fprintf ( 1, '  quadrilateral_is_convex() is true if a quadrilateral is convex.\n' );

  for i = 1 : 5

    q = quadrilateral_random_simple ( );
    fprintf ( 1, '\n' );
    fprintf ( 1, '  quadrilateral vertices:\n' );
    disp ( q );
    value = quadrilateral_is_convex ( q );
    fprintf ( 1, '  quadrilateral_is_convex ( q ) = %g\n', value );

  end

  return
end
