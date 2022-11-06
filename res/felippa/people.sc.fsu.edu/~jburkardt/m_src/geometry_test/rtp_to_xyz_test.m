function rtp_to_xyz_test ( )

%*****************************************************************************80
%
%% rtp_to_xyz_test() tests rtp_to_xyz().
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
  a = -2.0;
  b =  3.0;
  test_num = 5;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'rtp_to_xyz_test():\n' );
  fprintf ( 1, '  rtp_to_xyz() converts XYZ to (R,Theta,Phi) coordinates.\n' );
  fprintf ( 1, '  xyz_to_rtp() converts (R,Theta,Phi) to XYZ coordinates.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, ...
    '      X1     Y1     Z1     R    THETA    PHI    X2     Y2     Z2\n' );
  fprintf ( 1, '\n' );

  for test = 1 : test_num

    xyz1 = a + ( b - a ) * rand ( 3, 1 );

    [ r, theta, phi ] = xyz_to_rtp ( xyz1 );
    xyz2 = rtp_to_xyz ( r, theta, phi );

    fprintf ( 1, '  %7.4f  %7.4f  %7.4f  %7.4f  %7.4f  %7.4f  %7.4f  %7.4f  %7.4f\n', ...
      xyz1(1:3,1), r, theta, phi, xyz2(1:3,1) );

  end

  return
end
