function radec_to_xyz_test ( )

%*****************************************************************************80
%
%% radec_to_xyz_test() tests radec_to_xyz().
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
  ntest = 6;

  ptest = [ ...
     1.0,  0.0,  0.0; ...
     0.0,  1.0,  0.0; ...
     0.0,  0.0,  1.0; ...
     1.0,  1.0,  1.0; ...
     5.0, -2.0, -1.0; ...
    -2.0, -2.0, -2.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'radec_to_xyz_test():\n' );
  fprintf ( 1, '  radec_to_xyz() converts XYZ to RADEC coordinates.\n' );
  fprintf ( 1, '  xyz_to_radec() converts RADEC to XYZ coordinates.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '          P1          RA     DEC           P2\n' );
  fprintf ( 1, '\n' );

  for i = 1 : ntest

    p1(1:3,1) = ptest(1:3,i);

    [ ra, dec ] = xyz_to_radec ( p1 );
    p2 = radec_to_xyz ( ra, dec );

    fprintf ( 1, '  %10f  %10f  %10f  %10f  %10f  %10f  %10f  %10f\n', ...
      p1(1:3,1), ra, dec, p2(1:3,1) );

  end

  return
end
