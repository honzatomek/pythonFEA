function geometry_test0616 ( )

%*****************************************************************************80
%
%% geometry_test0616() tests plane_normal_qr_to_xyz() and plane_normal_xyz_to_qr().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 November 2010
%
%  Author:
%
%    John Burkardt
%
  m = 3;
  n = 5;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'TEST0616\n' );
  fprintf ( 1, '  For a normal plane, with point PP and NORMAL vector,\n' );
  fprintf ( 1, '  and in-plane basis vectors PQ and PR,\n' );
  fprintf ( 1, '  PLANE_NORMAL_QR_TO_XYZ converts QR to XYZ coordinates;\n' );
  fprintf ( 1, '  PLANE_NORMAL_XYZ_TO_QR converts XYZ to QR coordinates.\n' );
%
%  Choose PP and NORMAL at random.
%
  pp = rand ( m, 1 );

  normal = rand ( m, 1 );
%
%  Compute in-plane basis vectors PQ and PR.
%
  [ pq, pr ] = plane_normal_basis_3d ( pp, normal );
%
%  Choose random Q, R coordinates.
%
  qr1 = rand ( m - 1, n );
%
%  Convert to XYZ.
%
  xyz = plane_normal_qr_to_xyz ( pp, normal, pq, pr, n, qr1 );
%
%  Convert XYZ to QR.
%
  qr2 = plane_normal_xyz_to_qr ( pp, normal, pq, pr, n, xyz );

  dif = sqrt ( sum ( ( qr1 - qr2 ).^2, 1 ) );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Maximum difference for %d points was %f\n', n, max ( dif ) );

  return
end
