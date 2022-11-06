function [ r, theta, phi ] = xyz_to_rtp ( xyz )

%*****************************************************************************80
%
%% xyz_to_rtp() converts (X,Y,Z) to (R,Theta,Phi) coordinates.
%
%  Discussion:
%
%    Given an XYZ point, compute its distance R from the origin, and
%    regard it as lying on a sphere of radius R, whose axis is the Z
%    axis.
%
%    Theta measures the "longitude" of the point, between 0 and 2 PI.
%
%    PHI measures the angle from the "north pole", between 0 and PI.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real XYZ(3,1), the coordinates of a point in 3D.
%
%  Output:
%
%    real R, THETA, PHI, the radius, longitude and
%    declination of the point.
%

  r = norm ( xyz(1:3,1) );

  if ( r == 0.0 )
    theta = 0.0;
    phi = 0.0;
    return
  end

  phi = acos ( xyz(3,1) / r );

  theta = atan2 ( xyz(2,1), xyz(1,1) );

  return
end
