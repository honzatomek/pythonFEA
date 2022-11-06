function annulus_area_2d_test ( )

%*****************************************************************************80
%
%% annulus_area_2d_test() tests annulus_area_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 July 2018
%
%  Author:
%
%    John Burkardt
%
  pc = [ 5.0, 3.0 ];
  r1 = 2.0;
  r2 = 3.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'annulus_area_2d_test():\n' );
  fprintf ( 1, '  annulus_area_2d() computes the centroid of a\n' );
  fprintf ( 1, '  circular annulus.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  The circle has center        %f  %f\n', pc(1:2) );
  fprintf ( 1, '  The inner radius is R1 =     %f\n', r1 );
  fprintf ( 1, '  The outer radius is R2 =     %f\n', r2 );

  area = annulus_area_2d ( r1, r2 );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Area: %f\n', area );

  return
end
