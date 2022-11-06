function polar_to_xy_test ( )

%*****************************************************************************80
%
%% polar_to_xy_test() tests polar_to_xy().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 April 2009
%
%  Author:
%
%    John Burkardt
%
  test_num = 10;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polar_to_xy_test():\n' );
  fprintf ( 1, '  polar_to_xy() converts (R,Theta) to (X,Y);\n' );
  fprintf ( 1, '  xy_to_polar() converts (X,Y) to (R,Theta).\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, ...
    '         X           Y     ===>  R           T   =>      X           Y\n' );
  fprintf ( 1, '\n' );

  for test = 1 : test_num

    xy1 = -1.0 + 2.0 * rand ( 2, 1 );

    [ r, t ] = xy_to_polar ( xy1 );
    xy2 = polar_to_xy ( r, t );

    fprintf ( 1, '  %9f  %9f  %9f  %9f  %9f  %9f\n', xy1(1:2,1), r, t, xy2(1:2,1) );

  end

  return
end
