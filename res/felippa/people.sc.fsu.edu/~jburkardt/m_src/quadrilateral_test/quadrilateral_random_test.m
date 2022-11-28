function quadrilateral_random_test ( )

%*****************************************************************************80
%
%% quadrilateral_random_test() tests quadrilateral_random().
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
  fprintf ( 1, 'quadrilateral_random_test():\n' );
  fprintf ( 1, '  quadrilateral_random() returns a random quadrilateral\n' );
  fprintf ( 1, '  within the unit square.\n' );

  for i = 1 : 5

    q = quadrilateral_random ( );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  quadrilateral vertices:\n' );
    disp ( q );
    value = quadrilateral_is_simple ( q );
    fprintf ( 1, '  quadrilateral_is_simple ( q ) = %d\n', value );
    value = quadrilateral_is_convex ( q );
    fprintf ( 1, '  quadrilateral_is_convex ( q ) = %d\n', value );
    angles = quadrilateral_angles ( q );
    angles = angles * 180.0 / pi;
    fprintf ( 1, '\n' );
    fprintf ( 1, '  angles:\n' );
    disp ( angles );
    angle_sum = sum ( angles );
    fprintf ( 1, '  Angle sum = %g\n', angle_sum );
    area = quadrilateral_area ( q );
    fprintf ( 1, '  Area = %g\n', area );
    perimeter = quadrilateral_perimeter ( q );
    fprintf ( 1, '  Perimeter = %g\n', perimeter );

  end

  return
end