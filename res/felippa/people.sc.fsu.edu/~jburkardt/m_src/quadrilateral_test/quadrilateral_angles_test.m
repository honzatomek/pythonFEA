function quadrilateral_angles_test ( )

%*****************************************************************************80
%
%% quadrilateral_angles_test() tests quadrilateral_angles().
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
  fprintf ( 1, 'quadrilateral_angles_test():\n' );
  fprintf ( 1, '  quadrilateral_angles() returns the angles of a quadrilateral\n' );
  fprintf ( 1, '  in radians.\n' );

  q = [ ...
    0.0, 1.0, 1.0, 0.0; ...
    0.0, 0.0, 1.0, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );
  angles = quadrilateral_angles ( q );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  angles:\n' );
  disp ( angles );
  fprintf ( 1, '  Angles sum to %g\n',sum ( angles ) );

  q = [ ...
    0.0, 1.0, 2.0, 0.0; ...
    0.0, 0.0, 1.0, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );
  angles = quadrilateral_angles ( q );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  angles:\n' );
  disp ( angles );
  fprintf ( 1, '  Angles sum to %g\n',sum ( angles ) );

  q = [ ...
    0.0, 1.0, 0.25, 0.0; ...
    0.0, 0.0, 0.25, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );
  angles = quadrilateral_angles ( q );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  angles:\n' );
  disp ( angles );
  fprintf ( 1, '  Angles sum to %g\n',sum ( angles ) );

  q = [ ...
    0.0, 1.0, -0.5, 0.0; ...
    0.0, 0.0,  0.5, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );
  angles = quadrilateral_angles ( q );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  angles:\n' );
  disp ( angles );
  fprintf ( 1, '  Angles sum to %g\n',sum ( angles ) );

  return
end
