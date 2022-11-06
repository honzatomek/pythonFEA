function angle = r8vec_angle_3d ( u, v )

%*****************************************************************************80
%
%% r8vec_angle_3d() computes the angle between two vectors in 3D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real U(1:3), V(1:3), the vectors.
%
%  Output:
%
%    real ANGLE, the angle between the two vectors.
%
  uv_dot = u(1) * v(1) + u(2) * v(2) + u(3) * v(3);

  u_norm = sqrt ( u(1) * u(1) + u(2) * u(2) + u(3) * u(3) );

  v_norm = sqrt ( v(1) * v(1) + v(2) * v(2) + v(3) * v(3) );

  angle_cos = uv_dot / u_norm / v_norm;

  angle = acos ( angle_cos );

  return
end
