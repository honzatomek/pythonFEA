function angle = triangle_angles_2d ( t )

%*****************************************************************************80
%
%% triangle_angles_2d() computes the angles of a triangle in 2D.
%
%  Discussion:
%
%    The law of cosines is used:
%
%      C^2 = A^2 + B^2 - 2 * A * B * COS ( GAMMA )
%
%    where GAMMA is the angle opposite side C.
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
%    real T(2,3), the triangle vertices.
%
%  Output:
%
%    real ANGLE(3,1), the angles opposite
%    sides P1-P2, P2-P3 and P3-P1, in radians.
%

%
%  Compute the length of each side.
%
  a = sqrt ( sum ( ( t(1:2,1) - t(1:2,2) ).^2 ) );
  b = sqrt ( sum ( ( t(1:2,2) - t(1:2,3) ).^2 ) );
  c = sqrt ( sum ( ( t(1:2,3) - t(1:2,1) ).^2 ) );

  angle = zeros ( 3, 1 );
%
%  Take care of unlikely special cases.
%
  if ( a == 0.0 & b == 0.0 & c == 0.0 )
    angle(1:3,1) = 2.0 * pi / 3.0;
    return
  end

  if ( c == 0.0 | a == 0.0 )
    angle(1,1) = pi;
  else
    angle(1,1) = acos ( ( c * c + a * a - b * b ) / ( 2.0 * c * a ) );
  end

  if ( a == 0.0 | b == 0.0 )
    angle(2,1) = pi;
  else
    angle(2,1) = acos ( ( a * a + b * b - c * c ) / ( 2.0 * a * b ) );
  end

  if ( b == 0.0 | c == 0.0 )
    angle(3,1) = pi;
  else
    angle(3,1) = acos ( ( b * b + c * c - a * a ) / ( 2.0 * b * c ) );
  end

  return
end
