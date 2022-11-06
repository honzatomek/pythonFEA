function vector_rotate_base_2d_test ( )

%*****************************************************************************80
%
%% vector_rotate_base_2d_test() tests vector_rotate_base_2d();
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 December 2010
%
%  Author:
%
%    John Burkardt
%
  ntest = 4;

  atest = [ 30.0, -45.0, 270.0, 20.0 ];
  pb(1:2,1) = [ 10.0; 5.0 ];
  ptest = [
    11.0, 5.0; ...
    10.0, 7.0; ...
    11.0, 6.0; ...
    10.0, 5.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'vector_rotate_base_2d_test():\n' );
  fprintf ( 1, '  vector_rotate_base_2d() rotates a vector (X1,Y1)\n' );
  fprintf ( 1, '  through an angle around a base point (XB,YB).\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '        P1              PB       Angle          P2\n' );
  fprintf ( 1, '\n' );

  for i = 1 : ntest

    p1 = ptest(1:2,i);

    angle = degrees_to_radians ( atest(i) );

    p2 = vector_rotate_base_2d ( p1, pb, angle );

    fprintf ( 1, '  %10f  %10f  %10f  %10f  %10f  %10f  %10f\n', ...
      p1(1:2,1), pb(1:2,1), atest(i), p2(1:2,1) );

  end

  return
end
