function xyz = plane_normal_qr_to_xyz ( pp, normal, pq, pr, n, qr )

%*****************************************************************************80
%
%% plane_normal_qr_to_xyz(): QR_TO_XYZ coordinates for a normal form plane.
%
%  Discussion:
%
%    The normal form of a plane in 3D is:
%
%      PP is a point on the plane,
%      NORMAL is a normal vector to the plane.
%
%    Two vectors PQ and PR can be computed with the properties that
%    * NORMAL, PQ and PR are pairwise orthogonal;
%    * PQ and PR have unit length;
%    * every point P in the plane has a "QR" representation
%      as P = PP + q * PQ + r * PR.
%
%    This function is given the QR coordinates of a set of points on the
%    plane, and returns the XYZ coordinates.
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
%  Input:
%
%    real PP(3), a point on the plane.
%
%    real NORMAL(3), a normal vector N to the plane.  The
%    vector must not have zero length, but it is not necessary for N
%    to have unit length.
%
%    real PQ(3), a vector of unit length,
%    perpendicular to the vector N and the vector PR.
%
%    real PR(3), a vector of unit length,
%    perpendicular to the vector N and the vector PQ.
%
%    integer N, the number of points on the plane.
%
%    real QR(2,N), the QR coordinates of the points.
%
%  Output:
%
%    real XYZ(3,N), the XYZ coordinates of the points.
%
  xyz = repmat ( pp, 1, n ) + [ pq'; pr' ]' * qr(1:2,1:n);

  return
end
