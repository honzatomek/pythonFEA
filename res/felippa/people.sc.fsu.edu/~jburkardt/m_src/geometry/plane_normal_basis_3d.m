function [ pr, pq ] = plane_normal_basis_3d ( pp, normalvec )

%*****************************************************************************80
%
%% plane_normal_basis_3d() finds two perpendicular vectors in a plane in 3D.
%
%  Discussion:
%
%    The normal form of a plane in 3D is:
%
%      PP is a point on the plane,
%      N is a normal vector to the plane.
%
%    The two vectors to be computed, PQ and PR, can be regarded as
%    the basis of a Cartesian coordinate system for points in the plane.
%    Any point in the plane can be described in terms of the "origin"
%    point PP plus a weighted sum of the two vectors PQ and PR:
%
%      P = PP + a * PQ + b * PR.
%
%    The vectors PQ and PR have unit length, and are perpendicular to N
%    and to each other.
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
%  Input:
%
%    real PP(3,1), a point on the plane.  (Actually,
%    we never need to know these values to do the calculation!)
%
%    real NORMALVEC(3,1), a normal vector N to the plane.  The
%    vector must not have zero length, but it is not necessary for N
%    to have unit length.
%
%  Output:
%
%    real PQ(3,1), a vector of unit length,
%    perpendicular to the vector N and the vector PR.
%
%    real PR(3,1), a vector of unit length,
%    perpendicular to the vector N and the vector PQ.
%
  dim_num = 3;
%
%  Compute the length of NORMALVEC.
%
  normalvec_norm = norm ( normalvec );

  if ( normalvec_norm == 0.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'plane_normal_basis_3d(): Fatal error!\n' );
    fprintf ( 1, '  The normal vector is 0.\n' );
    error ( 'plane_normal_basis_3d(): Fatal error!' );
  end
%
%  Find a vector PQ that is normal to NORMALVEC and has unit length.
%
  pq = r8vec_any_normal ( 3, normalvec );
%
%  Now just take the cross product NORMALVEC x PQ to get the PR vector.
%
  pr = r8vec_cross_product_3d ( normalvec, pq );

  pr_norm = norm ( pr );

  pr(1:dim_num,1) = pr(1:dim_num,1) / pr_norm;

  return
end
