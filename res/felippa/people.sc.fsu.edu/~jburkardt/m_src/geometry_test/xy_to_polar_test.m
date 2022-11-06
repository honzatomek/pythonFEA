function xy_to_polar_test ( )

%*****************************************************************************80
%
%% xy_to_polar_test() tests xy_to_polar().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 July 2018
%
%  Author:
%
%    John Burkardt
%
  test_num = 10;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'xy_to_polar_test():\n' );
  fprintf ( 1, '  xy_to_polar() converts (X,Y) to (R,Theta).\n' );
  fprintf ( 1, '  polar_to_xy() converts (R,Theta) to (X,Y);\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, ...
    '         X           Y     ===>  R           T   =>      X           Y\n' );
  fprintf ( 1, '\n' );

  b = -1.0;
  c = +1.0;

  for test = 1 : test_num

    xy1 = b + ( c - b ) * rand ( 2, 1 );

    [ r, t ] = xy_to_polar ( xy1 );
    xy2 = polar_to_xy ( r, t );

    fprintf ( 1, '  %9f  %9f  %9f  %9f  %9f  %9f\n', xy1(1:2,1), r, t, xy2(1:2,1) );

  end

  return
end
