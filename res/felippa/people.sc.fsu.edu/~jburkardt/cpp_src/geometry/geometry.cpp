# include <cfloat>
# include <cmath>
# include <cstdlib>
# include <cstring>
# include <ctime>
# include <fstream>
# include <iomanip>
# include <iostream>

using namespace std;

# include "geometry.hpp"

//****************************************************************************80

void angle_box_2d ( double dist, double p1[2], double p2[2], double p3[2],
  double p4[2], double p5[2] )

//****************************************************************************80
//
//  Purpose:
//
//    ANGLE_BOX_2D "boxes" an angle defined by three points in 2D.
//
//  Discussion:
//
//    The routine is given points P1, P2 and P3, determining the two lines:
//      P1 to P2
//    and
//      P2 to P3
//    and a nonnegative distance
//      DIST.
//
//    The routine returns a pair of "corner" points
//      P4 and P5
//    both of which are a distance DIST from both lines, and in fact,
//    both of which are a distance DIST from P2.
//
//                         /  P3
//                        /   /   /
//     - - - - - - - - -P4 - / -P6 - - -
//                      /   /   /
//    P1---------------/--P2-----------------
//                    /   /   /
//     - - - - - - -P7 - / -P5 - - - - -
//                  /   /   /
//
//    In the illustration, P1, P2 and P3 represent
//    the points defining the lines.
//
//    P4 and P5 represent the desired "corner points", which
//    are on the positive or negative sides of both lines.
//
//    The numbers P6 and P7 represent the undesired points, which
//    are on the positive side of one line and the negative of the other.
//
//    Special cases:
//
//    if P1 = P2, this is the same as extending the line from
//    P3 through P2 without a bend.
//
//    if P3 = P2, this is the same as extending the line from
//    P1 through P2 without a bend.
//
//    if P1 = P2 = P3 this is an error.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double DIST, the nonnegative distance from P1
//    to the computed points P4 and P5.
//
//    Input, double P1[2], P2[2], P3[2].
//    P1 and P2 are distinct points that define a line.
//    P2 and P3 are distinct points that define a line.
//
//    Output, double P4[2], P5[2], points which lie DIST units from
//    the line between P1 and P2, and from the line between P2 and P3.
//
{
# define DIM_NUM 2

  double stheta;
  double temp;
  double temp1;
  double temp2;
  double u[DIM_NUM];
  double u1[DIM_NUM];
  double u2[DIM_NUM];
//
//  If DIST = 0, assume the user knows best.
//
  if ( dist == 0.0 )
  {
    r8vec_copy ( DIM_NUM, p2, p4 );
    r8vec_copy ( DIM_NUM, p2, p5 );
    return;
  }
//
//  Fail if all three points are equal.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) && r8vec_eq ( DIM_NUM, p2, p3 ) )
  {
    cerr << "\n";
    cerr << "ANGLE_BOX_2D - Fatal error!\n";
    cerr << "  Input points P3 = P2 = P3.\n";
    r8vec_print ( DIM_NUM, p1, "  P1:" );
    exit ( 1 );
  }
//
//  If P1 = P2, extend the line through the doubled point.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) )
  {
    u2[0] = p3[1] - p2[1];
    u2[1] = p2[0] - p3[0];
    temp = r8vec_norm ( DIM_NUM, u2 );
    u2[0] = u2[0] / temp;
    u2[1] = u2[1] / temp;
    p4[0] = p2[0] + dist * u2[0];
    p4[1] = p2[1] + dist * u2[1];
    p5[0] = p2[0] - dist * u2[0];
    p5[1] = p2[1] - dist * u2[1];
    return;
  }
//
//  If P2 = P3, extend the line through the doubled point.
//
  if ( r8vec_eq ( DIM_NUM, p2, p3 ) )
  {
    u1[0] = p1[1] - p2[1];
    u1[1] = p2[0] - p1[0];
    temp = r8vec_norm ( DIM_NUM, u1 );
    u1[0] = u1[0] / temp;
    u1[1] = u1[1] / temp;
    p4[0] = p2[0] + dist * u1[0];
    p4[1] = p2[1] + dist * u1[1];
    p5[0] = p2[0] - dist * u1[0];
    p5[1] = p2[1] - dist * u1[1];
    return;
  }
//
//  Now compute the unit normal vectors to each line.
//  We choose the sign so that the unit normal to line 1 has
//  a positive dot product with line 2.
//
  u1[0] = p1[1] - p2[1];
  u1[1] = p2[0] - p1[0];
  temp = r8vec_norm ( DIM_NUM, u1 );
  u1[0] = u1[0] / temp;
  u1[1] = u1[1] / temp;

  temp1 = u1[0] * ( p3[0] - p2[0] )
        + u1[1] * ( p3[1] - p2[1] );

  if ( temp1 < 0.0 )
  {
    u1[0] = -u1[0];
    u1[1] = -u1[1];
  }

  u2[0] = p3[1] - p2[1];
  u2[1] = p2[0] - p3[0];
  temp = r8vec_norm ( DIM_NUM, u2 );
  u2[0] = u2[0] / temp;
  u2[1] = u2[1] / temp;

  temp2 = u2[0] * ( p1[0] - p2[0] )
        + u2[1] * ( p1[1] - p2[1] );

  if ( temp2 < 0.0 )
  {
    u2[0] = -u2[0];
    u2[1] = -u2[1];
  }
//
//  Try to catch the case where we can't determine the
//  sign of U1, because both U1 and -U1 are perpendicular
//  to (P3-P2), and similarly for U2 and (P1-P2).
//
  if ( temp1 == 0.0 || temp2 == 0.0 )
  {
    if ( u1[0] * u2[0] + u1[1] * u2[1] < 0.0 )
    {
      u1[0] = -u1[0];
      u2[0] = -u2[0];
    }
  }
//
//  Try to catch a line turning back on itself, evidenced by
//    Cos(theta) = (P3-P2) dot (P2-P1) / ( norm(P3-P2) * norm(P2-P1) )
//  being -1, or very close to -1.
//
  temp = ( p3[0] - p2[0] ) * ( p2[0] - p1[0] )
       + ( p3[1] - p2[1] ) * ( p2[1] - p1[1] );

  temp1 = sqrt ( pow ( p3[0] - p2[0], 2 ) + pow ( p3[1] - p2[1], 2 ) );
  temp2 = sqrt ( pow ( p2[0] - p1[0], 2 ) + pow ( p2[1] - p1[1], 2 ) );

  temp = temp / ( temp1 * temp2 );

  if ( temp < -0.99 )
  {
    temp = sqrt ( pow ( p2[0] - p1[0], 2 ) + pow ( p2[1] - p1[1], 2 ) );

    p4[0] = p2[0] + dist * ( p2[0] - p1[0] ) / temp + dist * u1[0];
    p4[1] = p2[1] + dist * ( p2[1] - p1[1] ) / temp + dist * u1[1];
    p5[0] = p2[0] + dist * ( p2[0] - p1[0] ) / temp - dist * u1[0];
    p5[1] = p2[1] + dist * ( p2[1] - p1[1] ) / temp - dist * u1[1];
    return;
  }
//
//  Compute the "average" unit normal vector.
//
//  The average of the unit normals could be zero, but only when
//  the second line has the same direction and opposite sense
//  of the first, and we've already checked for that case.
//
//  Well, check again!  This problem "bit" me in the case where
//  P1 = P2, which I now treat specially just to guarantee I
//  avoid this problem!
//
  if ( u1[0] * u2[0] + u1[1] * u2[1] < 0.0 )
  {
    u2[0] = -u2[0];
    u2[1] = -u2[1];
  }

  u[0] = 0.5 * ( u1[0] + u2[0] );
  u[1] = 0.5 * ( u1[1] + u2[1] );

  temp = r8vec_norm ( DIM_NUM, u );
  u[0] = u[0] / temp;
  u[1] = u[1] / temp;
//
//  You must go DIST/STHETA units along this unit normal to
//  result in a distance DIST from line1 (and line2).
//
  stheta = u[0] * u1[0] + u[1] * u1[1];

  p4[0] = p2[0] + dist * u[0] / stheta;
  p4[1] = p2[1] + dist * u[1] / stheta;
  p5[0] = p2[0] - dist * u[0] / stheta;
  p5[1] = p2[1] - dist * u[1] / stheta;

  return;
# undef DIM_NUM
}
//****************************************************************************80

bool angle_contains_ray_2d ( double p1[2], double p2[2], double p3[2],
  double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    ANGLE_CONTAINS_RAY_2D determines if an angle contains a ray, in 2D.
//
//  Discussion:
//
//    The angle is defined by the sequence of points P1, P2, P3.
//
//    The ray is defined by the sequence of points P2, P.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], P3[2], the coordinates of points on
//    the angle.
//
//    Input, double P[2], the end point of the ray to be checked.
//    The ray is assumed to have an origin at P2.
//
//    Output, bool ANGLE_CONTAINS_RAY_2D, is true if the ray is inside
//    the angle or on its boundary, and false otherwise.
//
{
  double a1;
  double a2;
  bool value;

  a1 = angle_deg_2d ( p1, p2, p );

  a2 = angle_deg_2d ( p1, p2, p3 );

  if ( a1 <= a2 )
  {
    value = true;
  }
  else
  {
    value = false;
  }
  return value;
}
//****************************************************************************80

double angle_deg_2d ( double p1[2], double p2[2], double p3[2] )

//****************************************************************************80
//
//  Purpose:
//
//    ANGLE_DEG_2D returns the angle in degrees swept out between two rays in 2D.
//
//  Discussion:
//
//    Except for the zero angle case, it should be true that
//
//    ANGLE_DEG_2D(P1,P2,P3) + ANGLE_DEG_2D(P3,P2,P1) = 360.0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], P3[2], define the rays
//    P1 - P2 and P3 - P2 which define the angle.
//
//    Output, double ANGLE_DEG_2D, the angle swept out by the rays, measured
//    in degrees.  0 <= ANGLE_DEG_2D < 360.  If either ray has zero length,
//    then ANGLE_DEG_2D is set to 0.
//
{
# define DIM_NUM 2

  double angle_rad;
  double p[DIM_NUM];
  double r8_pi = 3.141592653589793;
  double value;

  p[0] = ( p1[0] - p2[0] ) * ( p3[0] - p2[0] )
       + ( p1[1] - p2[1] ) * ( p3[1] - p2[1] );

  p[1] = ( p1[0] - p2[0] ) * ( p3[1] - p2[1] )
       - ( p1[1] - p2[1] ) * ( p3[0] - p2[0] );

  if ( p[0] == 0.0 && p[1] == 0.0 )
  {
    value = 0.0;
  }
  else
  {
    angle_rad = atan2 ( p[1], p[0] );

    if ( angle_rad < 0.0 )
    {
      angle_rad = angle_rad + 2.0 * r8_pi;
    }

    value = radians_to_degrees ( angle_rad );

  }

  return value;
# undef DIM_NUM
}
//****************************************************************************80

double *angle_half_2d ( double p1[2], double p2[2], double p3[2] )

//****************************************************************************80
//
//  Purpose:
//
//    ANGLE_HALF_2D finds half an angle in 2D.
//
//  Discussion:
//
//    The original angle is defined by the sequence of points P1, P2 and P3.
//
//    The point P4 is calculated so that:
//
//      (P1,P2,P4) = (P1,P2,P3) / 2
//
//        P1
//        /
//       /   P4
//      /  .
//     / .
//    P2--------->P3
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 May 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], P3[2], points defining the angle.
//
//    Input, double ANGLE_HALF_2D[2], a point P4 defining the half angle.
//    The vector P4 - P2 will have unit norm.
//
{
  int i;
  double norm;
  double *p4;

  p4 = new double[2];

  norm = sqrt ( ( p1[0] - p2[0] ) * ( p1[0] - p2[0] )
              + ( p1[1] - p2[1] ) * ( p1[1] - p2[1] ) );

  for ( i = 0; i < 2; i++ )
  {
    p4[i] = ( p1[i] - p2[i] ) / norm;
  }

  norm = sqrt ( ( p3[0] - p2[0] ) * ( p3[0] - p2[0] )
              + ( p3[1] - p2[1] ) * ( p3[1] - p2[1] ) );

  for ( i = 0; i < 2; i++ )
  {
    p4[i] = p4[i] + ( p3[i] - p2[i] ) / norm;
  }

  for ( i = 0; i < 2; i++ )
  {
    p4[i] = 0.5 * p4[i];
  }

  norm = r8vec_norm ( 2, p4 );

  for ( i = 0; i < 2; i++ )
  {
    p4[i] = p2[i] + p4[i] / norm;
  }

  return p4;
}
//****************************************************************************80

double angle_rad_2d ( double p1[2], double p2[2], double p3[2] )

//****************************************************************************80
//
//  Purpose:
//
//    ANGLE_RAD_2D returns the angle in radians swept out between two rays in 2D.
//
//  Discussion:
//
//      ANGLE_RAD_2D ( P1, P2, P3 ) + ANGLE_RAD_2D ( P3, P2, P1 ) = 2 * PI
//
//        P1
//        /
//       /
//      /
//     /
//    P2--------->P3
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    24 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], P3[2], define the rays
//    P1 - P2 and P3 - P2 which define the angle.
//
//    Output, double ANGLE_RAD_2D, the angle between the two rays,
//    in radians.  This value will always be between 0 and 2*PI.  If either 
//    ray has zero length, then the angle is returned as zero.
//
{
  double p[2];
  const double r8_pi = 3.141592653589793;
  double value;

  p[0] = ( p3[0] - p2[0] ) * ( p1[0] - p2[0] )
       + ( p3[1] - p2[1] ) * ( p1[1] - p2[1] );


  p[1] = ( p3[0] - p2[0] ) * ( p1[1] - p2[1] )
       - ( p3[1] - p2[1] ) * ( p1[0] - p2[0] );

  if ( p[0] == 0.0 && p[1] == 0.0 )
  {
    value = 0.0;
    return value;
  }

  value = atan2 ( p[1], p[0] );

  if ( value < 0.0 )
  {
    value = value + 2.0 * r8_pi;
  }

  return value;
}
//****************************************************************************80

double angle_rad_3d ( double p1[3], double p2[3], double p3[3] )

//****************************************************************************80
//
//  Purpose:
//
//    ANGLE_RAD_3D returns the angle between two vectors in 3D.
//
//  Discussion:
//
//    The routine always computes the SMALLER of the two angles between
//    two vectors.  Thus, if the vectors make an (exterior) angle of 200
//    degrees, the (interior) angle of 160 is reported.
//
//    X dot Y = Norm(X) * Norm(Y) * Cos ( Angle(X,Y) )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    20 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], points defining an angle.
//    The rays are P1 - P2 and P3 - P2.
//
//    Output, double ANGLE_RAD_3D, the angle between the two vectors, in radians.
//    This value will always be between 0 and PI.  If either vector has
//    zero length, then the angle is returned as zero.
//
{
# define DIM_NUM 3

  double dot;
  int i;
  double v1norm;
  double v2norm;
  double value;

  v1norm = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v1norm = v1norm + pow ( p1[i] - p2[i], 2 );
  }
  v1norm = sqrt ( v1norm );

  if ( v1norm == 0.0 )
  {
    value = 0.0;
    return value;
  }

  v2norm = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v2norm = v2norm + pow ( p3[i] - p2[i], 2 );
  }
  v2norm = sqrt ( v2norm );

  if ( v2norm == 0.0 )
  {
    value = 0.0;
    return value;
  }

  dot = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    dot = dot + ( p1[i] - p2[i] ) * ( p3[i] - p2[i] );
  }

  value = r8_acos ( dot / ( v1norm * v2norm ) );

  return value;
# undef DIM_NUM
}
//****************************************************************************80

double angle_rad_nd ( int dim_num, double vec1[], double vec2[] )

//****************************************************************************80
//
//  Purpose:
//
//    ANGLE_RAD_ND returns the angle between two vectors in ND.
//
//  Discussion:
//
//    ANGLE_RAD_ND always computes the SMALLER of the two angles between
//    two vectors.  Thus, if the vectors make an (exterior) angle of
//    1.5 PI radians, then the (interior) angle of 0.5 radians is returned.
//
//    X dot Y = Norm(X) * Norm(Y) * Cos( Angle(X,Y) )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, double VEC1[DIM_NUM], VEC2[DIM_NUM], the two vectors to be considered.
//
//    Output, double ANGLE_RAD_ND, the angle between the vectors, in radians.
//    This value will always be between 0 and PI.
//
{
  double dot;
  double v1norm;
  double v2norm;
  double value;

  dot = r8vec_dot_product ( dim_num, vec1, vec2 );

  v1norm = r8vec_norm ( dim_num, vec1 );
  v2norm = r8vec_norm ( dim_num, vec2 );

  if ( v1norm == 0.0 || v2norm == 0.0 )
  {
    value = 0.0;
  }
  else
  {
    value = acos ( dot / ( v1norm * v2norm ) );
  }

  return value;
}
//****************************************************************************80

double angle_turn_2d ( double p1[2], double p2[2], double p3[2] )

//****************************************************************************80
//
//  Purpose:
//
//    ANGLE_TURN_2D computes a turning angle in 2D.
//
//  Discussion:
//
//    This routine is most useful when considering the vertices of a
//    polygonal shape.  We wish to distinguish between angles that "turn
//    in" to the shape, (between 0 and 180 degrees) and angles that
//    "turn out" (between 180 and 360 degrees), as we traverse the boundary.
//
//    If we compute the interior angle and subtract 180 degrees, we get the
//    supplementary angle, which has the nice property that it is
//    negative for "in" angles and positive for "out" angles, and is zero if
//    the three points actually lie along a line.
//
//    Assuming P1, P2 and P3 define an angle, the TURN can be
//    defined to be either:
//
//    * the supplementary angle to the angle formed by P1=P2=P3, or
//
//    * the angle between the vector ( P3-P2) and the vector -(P1-P2),
//      where -(P1-P2) can be understood as the vector that continues
//      through P2 from the direction P1.
//
//    The turning will be zero if P1, P2 and P3 lie along a straight line.
//
//    It will be a positive angle if the turn from the previous direction
//    is counter clockwise, and negative if it is clockwise.
//
//    The turn is given in radians, and will lie between -PI and PI.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    14 March 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], P3[2], the points that form
//    the angle.
//
//    Output, double ANGLE_TURN_2D, the turn angle, between -PI and PI.
//
{
# define DIM_NUM 2

  double p[DIM_NUM];
  double r8_pi = 3.141592653589793;
  double turn;

  p[0] = ( p3[0] - p2[0] ) * ( p1[0] - p2[0] )
       + ( p3[1] - p2[1] ) * ( p1[1] - p2[1] );

  p[1] = ( p3[0] - p2[0] ) * ( p1[1] - p2[1] )
       - ( p3[1] - p2[1] ) * ( p1[0] - p2[0] );

  if ( p[0] == 0.0 && p[1] == 0.0 )
  {
    turn = 0.0;
  }
  else
  {
    turn = r8_pi - r8_atan ( p[1], p[0] );
  }

  return turn;
# undef DIM_NUM
}
//****************************************************************************80

double anglei_deg_2d ( double p1[2], double p2[2], double p3[2] )

//****************************************************************************80
//
//  Purpose:
//
//    ANGLEI_DEG_2D returns the interior angle in degrees between two rays in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], points defining an angle.
//    The rays are P1 - P2 and P3 - P2.
//
//    Output, double ANGLEI_DEG_2D, the angle swept out by the rays, measured
//    in degrees.  This value satisfies 0 <= ANGLEI_DEG_2D < 180.0.  If either
//    ray is of zero length, then ANGLEI_deg_2D is returned as 0.
//
{
# define DIM_NUM 2

  double p[DIM_NUM];
  double r8_pi = 3.141592653589793;
  double value;

  p[0] = ( p1[0] - p2[0] ) * ( p3[0] - p2[0] )
       + ( p1[1] - p2[1] ) * ( p3[1] - p2[1] );

  p[1] = ( p1[0] - p2[0] ) * ( p3[1] - p2[1] )
       - ( p1[1] - p2[1] ) * ( p3[0] - p2[0] );

  if ( p[0] == 0.0 && p[1] == 0.0 )
  {
    value = 0.0;
  }
  else
  {
    value = atan2 ( p[1], p[0] );

    if ( value < 0.0 )
    {
      value = value + 2.0 * r8_pi;
    }

    value = radians_to_degrees ( value );

    if ( 180.0 < value )
    {
      value = 360.0 - value;
    }

  }
  return value;
# undef DIM_NUM
}
//****************************************************************************80

double anglei_rad_2d ( double p1[2], double p2[2], double p3[2] )

//****************************************************************************80
//
//  Purpose:
//
//    ANGLEI_RAD_2D returns the interior angle in radians between two rays in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], points defining an angle.
//    The rays are P1 - P2 and P3 - P2.
//
//    Output, double ANGLEI_RAD_2D, the angle swept out by the rays, measured
//    in degrees.  This value satisfies 0 <= ANGLEI_RAD_2D < PI.  If either
//    ray is of zero length, then ANGLEI_RAD_2D is returned as 0.
//
{
# define DIM_NUM 2

  double p[DIM_NUM];
  double r8_pi = 3.141592653589793;
  double value;

  p[0] = ( p1[0] - p2[0] ) * ( p3[0] - p2[0] )
       + ( p1[1] - p2[1] ) * ( p3[1] - p2[1] );

  p[1] = ( p1[0] - p2[0] ) * ( p3[1] - p2[1] )
       - ( p1[1] - p2[1] ) * ( p3[0] - p2[0] );

  if ( p[0] == 0.0 && p[1] == 0.0 )
  {
    value = 0.0;
  }
  else
  {
    value = atan2 ( p[1], p[0] );

    if ( value < 0.0 )
    {
      value = value + 2.0 * r8_pi;
    }

    if ( r8_pi < value )
    {
      value = 2.0 * r8_pi - value;
    }

  }
  return value;
# undef DIM_NUM
}
//****************************************************************************80

double annulus_area_2d ( double r1, double r2 )

//****************************************************************************80
//
//  Purpose:
//
//    ANNULUS_AREA_2D computes the area of a circular annulus in 2D.
//
//  Discussion:
//
//    A circular annulus with center (XC,YC), inner radius R1 and
//    outer radius R2, is the set of points (X,Y) so that
//
//      R1^2 <= (X-XC)^2 + (Y-YC)^2 <= R2^2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R1, R2, the inner and outer radii.
//
//    Output, double ANNULUS_AREA_2D, the area.
//
{
  double area;
  double r8_pi = 3.141592653589793;

  area = r8_pi * ( r2 + r1 ) * ( r2 - r1 );

  return area;
}
//****************************************************************************80

double annulus_sector_area_2d ( double r1, double r2, double theta1,
  double theta2 )

//****************************************************************************80
//
//  Purpose:
//
//    ANNULUS_SECTOR_AREA_2D computes the area of an annular sector in 2D.
//
//  Discussion:
//
//    An annular sector with center PC, inner radius R1 and
//    outer radius R2, and angles THETA1, THETA2, is the set of points
//    P so that
//
//      R1^2 <= (P(1)-PC(1))^2 + (P(2)-PC(2))^2 <= R2^2
//
//    and
//
//      THETA1 <= THETA ( P - PC ) <= THETA2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R1, R2, the inner and outer radii.
//
//    Input, double THETA1, THETA2, the angles.
//
//    Output, double ANNULUS_SECTOR_AREA_2D, the area.
//
{
  double area;

  area = 0.5 * ( theta2 - theta1 ) * ( r2 + r1 ) * ( r2 - r1 );

  return area;
}
//****************************************************************************80

double *annulus_sector_centroid_2d ( double pc[2], double r1, double r2,
  double theta1, double theta2 )

//****************************************************************************80
//
//  Purpose:
//
//    ANNULUS_SECTOR_CENTROID_2D computes the centroid of an annular sector in 2D.
//
//  Discussion:
//
//    An annular sector with center PC, inner radius R1 and
//    outer radius R2, and angles THETA1, THETA2, is the set of points
//    P so that
//
//      R1^2 <= (P(1)-PC(1))^2 + (P(2)-PC(2))^2 <= R2^2
//
//    and
//
//      THETA1 <= THETA ( P - PC ) <= THETA2
//
//    Thanks to Ed Segall for pointing out a mistake in the computation
//    of the angle THETA assciated with the centroid.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 December 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    John Harris, Horst Stocker,
//    Handbook of Mathematics and Computational Science,
//    Springer, 1998, QA40.S76
//
//  Parameters:
//
//    Input, double PC[2], the center.
//
//    Input, double R1, R2, the inner and outer radii.
//
//    Input, double THETA1, THETA2, the angles.
//
//    Output, double ANNULUS_SECTOR_CENTROID_2D[2], the centroid.
//
{
  double *centroid;
  double r;
  double theta;

  theta = theta2 - theta1;

  r = 4.0 * sin ( theta / 2.0 ) / ( 3.0 * theta )
    * ( r1 * r1 + r1 * r2 + r2 * r2 ) / ( r1 + r2 );

  centroid = new double[2];

  centroid[0] = pc[0] + r * cos ( theta1 + theta / 2.0 );
  centroid[1] = pc[1] + r * sin ( theta1 + theta / 2.0 );

  return centroid;
}
//****************************************************************************80

double *ball01_sample_2d ( int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    BALL01_SAMPLE_2D picks a random point in the unit ball in 2D.
//
//  Discussion:
//
//    The unit ball is the set of points such that
//
//      X * X + Y * Y <= 1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    25 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double BALL01_SAMPLE_2D[2], a random point in the unit ball.
//
{
  const double r8_pi = 3.141592653589793;
  double r;
  double theta;
  double *x;

  r = r8_uniform_01 ( seed );
  r = sqrt ( r );

  theta = r8_uniform_01 ( seed );
  theta = 2.0 * r8_pi * theta;

  x = new double[2];

  x[0] = r * cos ( theta );
  x[1] = r * sin ( theta );

  return x;
}
//****************************************************************************80

double *ball01_sample_3d ( int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    BALL01_SAMPLE_3D picks a random point in the unit ball in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    25 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double BALL01_SAMPLE_3D[3], the sample point.
//
{
# define DIM_NUM 3

  double phi;
  const double r8_pi = 3.141592653589793;
  double r;
  double theta;
  double vdot;
  double *x;
//
//  Pick a uniformly random VDOT, which must be between -1 and 1.
//  This represents the dot product of the random vector with the Z unit vector.
//
//  This works because the surface area of the sphere between
//  Z and Z + dZ is independent of Z.  So choosing Z uniformly chooses
//  a patch of area uniformly.
//
  vdot = r8_uniform_01 ( seed );
  vdot = 2.0 * vdot - 1.0;

  phi = r8_acos ( vdot );
//
//  Pick a uniformly random rotation between 0 and 2 Pi around the
//  axis of the Z vector.
//
  theta = r8_uniform_01 ( seed );
  theta = 2.0 * r8_pi * theta;
//
//  Pick a random radius R.
//
  r = r8_uniform_01 ( seed );
  r = pow ( ( double ) r, (double ) ( 1.0 / 3.0 ) );

  x = new double[DIM_NUM];

  x[0] = r * cos ( theta ) * sin ( phi );
  x[1] = r * sin ( theta ) * sin ( phi );
  x[2] = r * cos ( phi );

  return x;
# undef DIM_NUM
}
//****************************************************************************80

double *ball01_sample_nd ( int dim_num, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    BALL01_SAMPLE_ND picks a random point in the unit ball in ND.
//
//  Discussion:
//
//    DIM_NUM-1 random Givens rotations are applied to the point ( 1, 0, 0, ..., 0 ).
//
//    The I-th Givens rotation is in the plane of coordinate axes I and I+1,
//    and has the form:
//
//     [ cos ( theta )  - sin ( theta ) ] * x(i)      = x'(i)
//     [ sin ( theta )    cos ( theta ) ]   x(i+1)      x'(i+1)
//
//    Finally, a scaling is applied to set the point at a distance R
//    from the center, in a way that results in a uniform distribution.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    25 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double BALL01_SAMPLE_ND[DIM_NUM], the random point.
//
{
  int i;
  double r;
  double random_cosine;
  double random_sign;
  double random_sine;
  double *x;
  double xi;

  x = new double[dim_num];

  x[0] = 1.0;
  for ( i = 1; i < dim_num; i++ )
  {
    x[i] = 0.0;
  }

  for ( i = 0; i < dim_num-1; i++ )
  {
    random_cosine = r8_uniform_01 ( seed );
    random_cosine = 2.0 * random_cosine - 1.0;
    random_sign = r8_uniform_01 ( seed );
    random_sign = ( double )
      ( 2 * ( int ) ( 2.0 * random_sign - 1.0 ) );
    random_sine = random_sign
      * sqrt ( 1.0 - random_cosine * random_cosine );
    xi = x[i];
    x[i  ] = random_cosine * xi;
    x[i+1] = random_sine   * xi;
  }

  r = r8_uniform_01 ( seed );

  r = pow ( ( double ) r, 1.0 / ( double ) dim_num );

  for ( i = 0; i < dim_num; i++ )
  {
    x[i] = r * x[i];
  }

  return x;
}
//****************************************************************************80

double ball01_volume ( )

//****************************************************************************80
//
//  Purpose:
//
//    BALL01_VOLUME returns the volume of the unit ball in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    17 January 2018
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, double BALL01_VOLUME, the volume of the unit ball.
//
{
  double r;
  const double r8_pi = 3.141592653589793;
  double volume;
  
  r = 1.0;
  volume = 4.0 * r8_pi * r * r * r / 3.0;

  return volume;
}
//****************************************************************************80

double *basis_map_3d ( double u[3*3], double v[3*3] )

//****************************************************************************80
//
//  Purpose:
//
//    BASIS_MAP_3D computes the matrix which maps one basis to another.
//
//  Discussion:
//
//    As long as the vectors U1, U2 and U3 are linearly independent,
//    a matrix A will be computed that maps U1 to V1, U2 to V2, and
//    U3 to V3.
//
//    Depending on the values of the vectors, A may represent a
//    rotation, reflection, dilation, project, or a combination of these
//    basic linear transformations.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double U[3*3], the matrix whose columns are three "domain" or "preimage"
//    vectors, which should be linearly independent.
//
//    Input, double V[3*3], the matrix whose columns are the three "range" or
//    "image" vectors.
//
//    Output, double BASIS_MAP_3D[3*3], a matrix with the property that A * U1 = V1,
//    A * U2 = V2 and A * U3 = V3.
//
{
  double *a;
  double *c;
  int i;
  int j;
  int k;
//
//  Compute C = the inverse of U.
//
  c = r8mat_inverse_3d ( u );

  if ( c == NULL )
  {
    return NULL;
  }
//
//  A = V * inverse ( U ).
//
  a = new double[3*3];

  for ( j = 0; j < 3; j++ )
  {
    for ( i = 0; i < 3; i++ )
    {
      a[i+j*3] = 0.0;
      for ( k = 0; k < 3; k++ )
      {
        a[i+j*3] = a[i+j*3] + v[i+k*3] * c[k+j*3];
      }
    }
  }

  delete [] c;

  return a;
}
//****************************************************************************80

bool box_contains_point_2d ( double p1[2], double p2[2], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    BOX_CONTAINS_POINT_2D reports if a point is contained in a box in 2D.
//
//  Discussion:
//
//    A box in 2D is a rectangle with sides aligned on coordinate
//    axes.  It can be described by its low and high corners, P1 and P2
//    as the set of points P satisfying:
//
//      P1(1:2) <= P(1:2) <= P2(1:2).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], the minimum and maximum X and Y
//    values, which define the box.
//
//    Input, double P[2], the coordinates of the point.
//
//    Output, bool BOX_CONTAINS_POINT_2D, is TRUE if the box contains
//    the point.
//
{
  if ( p1[0] <= p[0] && p[0] <= p2[0] && p1[1] <= p[1] && p[1] <= p2[1] )
  {
    return true;
  }
  else
  {
    return false;
  }
}
//****************************************************************************80

bool box_contains_point_nd ( int dim_num, double p1[], double p2[], double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    BOX_CONTAINS_POINT_ND reports if a point is contained in a box in ND.
//
//  Discussion:
//
//    A box in ND is a rectangle with sides aligned on coordinate
//    axes.  It can be described by its low and high corners, P1 and P2
//    as the set of points P satisfying:
//
//      P1(1:DIM_NUM) <= P(1:DIM_NUM) <= P2(1:DIM_NUM).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, double P1[DIM_NUM], P2[DIM_NUM], the minimum and maximum X and Y
//    values, which define the box.
//
//    Input, double P[DIM_NUM], the coordinates of the point.
//
//    Output, bool BOX_CONTAINS_POINT_ND, is TRUE if the box contains
//    the point.
//
{
  int i;

  for ( i = 0; i < dim_num; i++ )
  {
    if ( p[i] < p1[i] || p2[i] < p[i] )
    {
      return false;
    }
  }

  return true;
}
//****************************************************************************80

void box_ray_int_2d ( double p1[2], double p2[2], double pa[2],
  double pb[2], double pint[2] )

//****************************************************************************80
//
//  Purpose:
//
//    BOX_RAY_INT_2D: intersection ( box, ray ) in 2D.
//
//  Discussion:
//
//    A box in 2D is a rectangle with sides aligned on coordinate
//    axes.  It can be described by its low and high corners, P1 and P2
//    as the set of points P satisfying:
//
//      P1(1:2) <= P(1:2) <= P2(1:2).
//
//    The origin of the ray is assumed to be inside the box.  This
//    guarantees that the ray will intersect the box in exactly one point.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], the lower left corner of the box.
//
//    Input, double P2[2], the upper right corner of the box.
//
//    Input, double PA[2], the origin of the ray, which should be
//    inside the box.
//
//    Input, double PB[2], a second point on the ray.
//
//    Output, double PINT[2], the point on the box intersected by the ray.
//
{
# define DIM_NUM 2

  bool inside;
  int ival;
  double pc[DIM_NUM];
  double pd[DIM_NUM];
  int side;

  for ( side = 1; side <= 4; side++ )
  {
    if ( side == 1 )
    {
      pc[0] = p1[0];
      pc[1] = p1[1];
      pd[0] = p2[0];
      pd[1] = p1[1];
    }
    else if ( side == 2 )
    {
      pc[0] = p2[0];
      pc[1] = p1[1];
      pd[0] = p2[0];
      pd[1] = p2[1];
    }
    else if ( side == 3 )
    {
      pc[0] = p2[0];
      pc[1] = p2[1];
      pd[0] = p1[0];
      pd[1] = p2[1];
    }
    else if ( side == 4 )
    {
      pc[0] = p1[0];
      pc[1] = p2[1];
      pd[0] = p1[0];
      pd[1] = p1[1];
    }

    inside = angle_contains_ray_2d ( pc, pa, pd, pb );

    if ( inside )
    {
      break;
    }

    if ( side == 4 )
    {
      cerr << "\n";
      cerr << "BOX_RAY_INT_2D - Fatal error!\n";
      cerr << "  No intersection could be found.\n";
      exit ( 1 );
    }

  }

  lines_exp_int_2d ( pa, pb, pc, pd, &ival, pint );

  return;
# undef DIM_NUM
}
//****************************************************************************80

int box_segment_clip_2d ( double p1[2], double p2[2], double pa[2],
  double pb[2] )

//****************************************************************************80
//
//  Purpose:
//
//    BOX_SEGMENT_CLIP_2D uses a box to clip a line segment in 2D.
//
//  Discussion:
//
//    A box in 2D is a rectangle with sides aligned on coordinate
//    axes.  It can be described by its low and high corners, P1 and P2
//    as the set of points P satisfying:
//
//      P1(1:2) <= P(1:2) <= P2(1:2).
//
//    A line segment is the finite portion of a line that lies between
//    two points.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], the minimum and maximum X and Y
//    values, which define the box.
//
//    Input/output, double PA[2], PB[2]; on input, the endpoints
//    of a line segment.  On output, the endpoints of the portion of the
//    line segment that lies inside the box.  However, if no part of the
//    initial line segment lies inside the box, the output value is the
//    same as the input value.
//
//    Output, int BOX_SEGMENT_CLIP_LINE_2D:
//    -1, no part of the line segment is within the box.
//     0, no clipping was necessary.
//     1, P1 was clipped.
//     2, P2 was clipped.
//     3, P1 and P2 were clipped.
//
{
  bool clip_a;
  bool clip_b;
  int ival;
  double x;
  double y;

  clip_a = false;
  clip_b = false;
//
//  Require that XMIN <= X.
//
  if ( pa[0] < p1[0] && pb[0] < p1[0] )
  {
    ival = -1;
    return ival;
  }

  if ( pa[0] < p1[0] && p1[0] <= pb[0] )
  {
    x = p1[0];
    y = pa[1] + ( pb[1] - pa[1] ) * ( x - pa[0] ) / ( pb[0] - pa[0] );
    pa[0] = x;
    pa[1] = y;
    clip_a = true;
  }
  else if ( p1[0] <= pa[0] && pb[0] < p1[0] )
  {
    x = p1[0];
    y = pa[1] + ( pb[1] - pa[1] ) * ( x - pa[0] ) / ( pb[0] - pa[0] );
    pb[0] = x;
    pb[1] = y;
    clip_b = true;
  }
//
//  Require that X <= XMAX.
//
  if ( p2[0] < pa[0] && p2[0] < pb[0] )
  {
    ival = -1;
    return ival;
  }

  if ( p2[0] < pa[0] && pb[0] <= p2[0] )
  {
    x = p2[0];
    y = pa[1] + ( pb[1] - pa[1] ) * ( x - pa[0] ) / ( pb[0] - pa[0] );
    pa[0] = x;
    pa[1] = y;
    clip_a = true;
  }
  else if ( pa[0] <= p2[0] && p2[0] < pb[0] )
  {
    x = p2[0];
    y = pa[1] + ( pb[1] - pa[1] ) * ( x - pa[0] ) / ( pb[0] - pa[0] );
    pb[0] = x;
    pb[1] = y;
    clip_b = true;
  }
//
//  Require that YMIN <= Y.
//
  if ( pa[1] < p1[1] && pb[1] < p1[1] )
  {
    ival = -1;
    return ival;
  }

  if ( pa[1] < p1[1] && p1[1] <= pb[1] )
  {
    y = p1[1];
    x = pa[0] + ( pb[0] - pa[0] ) * ( y - pa[1] ) / ( pb[1] - pa[1] );

    pa[0] = x;
    pa[1] = y;
    clip_a = true;
  }
  else if ( p1[1] <= pa[1] && pb[1] < p1[1] )
  {
    y = p1[1];
    x = pa[0] + ( pb[0] - pa[0] ) * ( y - pa[1] ) / ( pb[1] - pa[1] );
    pb[0] = x;
    pb[1] = y;
    clip_b = true;
  }
//
//  Require that Y <= YMAX.
//
  if ( p2[1] < pa[1] && p2[1] < pb[1] )
  {
    ival = -1;
    return ival;
  }

  if ( p2[1] < pa[1] && pb[1] <= p2[1] )
  {
    y = p2[1];
    x = pa[0] + ( pb[0] - pa[0] ) * ( y - pa[1] ) / ( pb[1] - pa[1] );
    pa[0] = x;
    pa[1] = y;
    clip_a = true;
  }
  else if ( pa[1] <= p2[1] && p2[1] < pb[1] )
  {
    y = p2[1];
    x = pa[0] + ( pb[0] - pa[0] ) * ( y - pa[1] ) / ( pb[1] - pa[1] );
    pb[0] = x;
    pb[1] = y;
    clip_b = true;
  }

  ival = 0;

  if ( clip_a )
  {
    ival = ival + 1;
  }

  if ( clip_b )
  {
    ival = ival + 2;
  }

  return ival;
}
//****************************************************************************80

bool box01_contains_point_2d ( double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    BOX01_CONTAINS_POINT_2D reports if a point is contained in the unit box in 2D.
//
//  Discussion:
//
//    A unit box in 2D is a rectangle with sides aligned on coordinate
//    axes.  It can be described as the set of points P satisfying:
//
//      0 <= P(1:2) <= 1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P[2], the coordinates of the point.
//
//    Output, bool BOX01_CONTAINS_POINT_2D, is TRUE if the box contains
//    the point.
//
{
  if ( 0.0 <= p[0] && p[0] <= 1.0 && 0.0 <= p[1] && p[1] <= 1.0 )
  {
    return true;
  }
  else
  {
    return false;
  }
}
//****************************************************************************80

bool box01_contains_point_nd ( int dim_num, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    BOX01_CONTAINS_POINT_ND reports if a point is contained in the unit box in ND.
//
//  Discussion:
//
//    A unit box is assumed to be a rectangle with sides aligned on coordinate
//    axes.  It can be described as the set of points P satisfying:
//
//      0.0 <= P(1:DIM_NUM) <= 1.0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, double P[DIM_NUM], the coordinates of the point.
//
//    Output, bool BOX01_CONTAINS_POINT_ND, is TRUE if the box contains
//    the point.
//
{
  int i;

  for ( i = 0; i < dim_num; i++ )
  {
    if ( p[i] < 0.0 || 1.0 < p[i] )
    {
      return false;
    }
  }

  return true;
}
//****************************************************************************80

void circle_arc_point_near_2d ( double r, double pc[2], double theta1,
  double theta2, double p[2], double pn[2], double *dist )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_ARC_POINT_NEAR_2D : nearest point on a circular arc.
//
//  Discussion:
//
//    A circular arc is defined by the portion of a circle (R,PC)
//    between two angles (THETA1,THETA2).
//
//    A point P on a circular arc satisfies
//
//      ( X - PC(1) ) * ( X - PC(1) ) + ( Y - PC(2) ) * ( Y - PC(2) ) = R * R
//
//    and
//
//      Theta1 <= Theta <= Theta2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the center of the circle.
//
//    Input, double THETA1, THETA2, the angles defining the arc,
//    in radians.  Normally, THETA1 < THETA2.
//
//    Input, double P[2], the point to be checked.
//
//    Output, double PN[2], a point on the circular arc which is
//    nearest to the point.
//
//    Output, double *DIST, the distance to the nearest point.
//
{
# define DIM_NUM 2

  int i;
  double r8_pi = 3.141592653589793;
  double r2;
  double theta;
//
//  Special case, the zero circle.
//
  if ( r == 0.0 )
  {
    r8vec_copy ( DIM_NUM, pc, pn );

    *dist = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      *dist = *dist + pow ( p[i] - pn[i], 2 );
    }
    *dist = sqrt ( *dist );
    return;
  }
//
//  Determine the angle made by the point.
//
  theta = r8_atan ( p[1] - pc[1], p[0] - pc[0] );
//
//  If the angle is between THETA1 and THETA2, then you can
//  simply project the point onto the arc.
//
  if ( r8_modp ( theta  - theta1,  2.0 * r8_pi ) <=
       r8_modp ( theta2 - theta1,  2.0 * r8_pi ) )
  {
    r2 = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      r2 = r2 + pow ( p[i] - pc[i], 2 );
    }
    r2 = sqrt ( r2 );
    for ( i = 0; i < DIM_NUM; i++ )
    {
      pn[i] = pc[i] + ( p[i] - pc[i] ) * r / r2;
    }
  }
//
//  Otherwise, if the angle is less than the negative of the
//  average of THETA1 and THETA2, it's on the side of the arc
//  where the endpoint associated with THETA2 is closest.
//
  else if ( r8_modp ( theta - 0.5 * ( theta1 + theta2 ), 2.0 * r8_pi )
    <= r8_pi )
  {
    pn[0] = pc[0] + r * cos ( theta2 );
    pn[1] = pc[1] + r * sin ( theta2 );
  }
//
//  Otherwise, the endpoint associated with THETA1 is closest.
//
  else
  {
    pn[0] = pc[0] + r * cos ( theta1 );
    pn[1] = pc[1] + r * sin ( theta1 );
  }

  *dist = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    *dist = *dist + pow ( p[i] - pn[i], 2 );
  }
  *dist = sqrt ( *dist );

  return;
# undef DIM_NUM
}
//****************************************************************************80

double circle_area_2d ( double r )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_AREA_2D computes the area of a circle in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    14 February 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Output, double CIRCLE_AREA_2D, the area of the circle.
//
{
  double area;
  double r8_pi = 3.141592653589793;

  area = r8_pi * r * r;

  return area;
}
//****************************************************************************80

void circle_dia2imp_2d ( double p1[2], double p2[2], double *r, double pc[2] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_DIA2IMP_2D converts a diameter to an implicit circle in 2D.
//
//  Discussion:
//
//    The diameter form of a circle is:
//
//      P1 and P2 are endpoints of a diameter.
//
//    The implicit form of a circle in 2D is:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    21 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], are the X and Y coordinates
//    of two points which form a diameter of the circle.
//
//    Output, double *R, the computed radius of the circle.
//
//    Output, double PC[2], the computed center of the circle.
//
{
  *r = 0.5 * sqrt ( pow ( p1[0] - p2[0], 2 )
                  + pow ( p1[1] - p2[1], 2 ) );

  pc[0] = 0.5 * ( p1[0] + p2[0] );
  pc[1] = 0.5 * ( p1[1] + p2[1] );

  return;
}
//****************************************************************************80

int circle_exp_contains_point_2d ( double p1[2], double p2[2], double p3[2],
  double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_EXP_CONTAINS_POINT_2D determines if an explicit circle contains a point in 2D.
//
//  Discussion:
//
//    The explicit form of a circle in 2D is:
//
//      The circle passing through points P1, P2 and P3.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], P3[2], the coordinates of three
//    points that lie on a circle.
//
//    Input, double P[2], the coordinates of a point, whose position
//    relative to the circle is desired.
//
//    Output, int CIRCLE_EXP_CONTAINS_POINT_2D:
//   -1, the three points are distinct and noncolinear,
//      and the point lies inside the circle.
//    0, the three points are distinct and noncolinear,
//      and the point lies on the circle.
//    1, the three points are distinct and noncolinear,
//      and the point lies outside the circle.
//    2, the three points are distinct and colinear,
//      and the point lies on the line.
//    3, the three points are distinct and colinear,
//      and the point does not lie on the line.
//    4, two points are distinct, and the point lies on the line.
//    5, two points are distinct, and the point does not lie on the line.
//    6, all three points are equal, and the point is equal to them,
//    7, all three points are equal, and the point is not equal to them.
//
{
  double a[4*4];
  double det;
  int inside;
//
//  P1 = P2?
//
  if ( r8vec_eq ( 2, p1, p2 ) )
  {
    if ( r8vec_eq ( 2, p1, p3 ) )
    {
      if ( r8vec_eq ( 2, p1, p ) )
      {
        inside = 6;
      }
      else
      {
        inside = 7;
      }
    }
    else
    {

      det = ( p1[0] - p3[0] ) * ( p[1]  - p3[1] )
          - ( p[0]  - p3[0] ) * ( p1[1] - p3[1] );

      if ( det == 0.0 )
      {
        inside = 4;
      }
      else
      {
        inside = 5;
      }
    }
    return inside;
  }
//
//  P1 does not equal P2.  Does P1 = P3?
//
  if ( r8vec_eq ( 2, p1, p3 ) )
  {
    det = ( p1[0] - p2[0] ) * ( p[1] - p2[1] )
        - ( p[0] - p2[0] ) * ( p1[1] - p2[1] );

    if ( det == 0.0 )
    {
      inside = 4;
    }
    else
    {
      inside = 5;
    }
    return inside;
  }
//
//  The points are distinct.  Are they colinear?
//
  det = ( p1[0] - p2[0] ) * ( p3[1] - p2[1] )
      - ( p3[0] - p2[0] ) * ( p1[1] - p2[1] );

  if ( det == 0.0 )
  {
    det = ( p1[0] - p2[0] ) * ( p[1] - p2[1] )
        - ( p[0] - p2[0] ) * ( p1[1] - p2[1] );

    if ( det == 0.0 )
    {
      inside = 2;
    }
    else
    {
      inside = 3;
    }

    return inside;

  }
//
//  The points are distinct and non-colinear.
//
//  Compute the determinant
//
  a[0+0*4] = p1[0];
  a[1+0*4] = p2[0];
  a[2+0*4] = p3[0];
  a[3+0*4] = p[0];

  a[0+1*4] = p1[1];
  a[1+1*4] = p2[1];
  a[2+1*4] = p3[1];
  a[3+1*4] = p[1];

  a[0+2*4] = p1[0] * p1[0] + p1[1] * p1[1];
  a[1+2*4] = p2[0] * p2[0] + p2[1] * p2[1];
  a[2+2*4] = p3[0] * p3[0] + p3[1] * p3[1];
  a[3+2*4] = p[0]  * p[0]  + p[1]  * p[1];

  a[0+3*4] = 1.0;
  a[1+3*4] = 1.0;
  a[2+3*4] = 1.0;
  a[3+3*4] = 1.0;

  det = r8mat_det_4d ( a );

  if ( det < 0.0 )
  {
    inside = 1;
  }
  else if ( det == 0.0 )
  {
    inside = 0;
  }
  else
  {
    inside = -1;
  }

  return inside;
}
//****************************************************************************80

void circle_exp2imp_2d ( double p1[2], double p2[2], double p3[2], double *r,
  double pc[2] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_EXP2IMP_2D converts a circle from explicit to implicit form in 2D.
//
//  Discussion:
//
//    The explicit form of a circle in 2D is:
//
//      The circle passing through points P1, P2 and P3.
//
//    Points P on an implicit circle in 2D satisfy the equation:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//    Any three points define a circle, as long as they don't lie on a straight
//    line.  (If the points do lie on a straight line, we could stretch the
//    definition of a circle to allow an infinite radius and a center at
//    some infinite point.)
//
//    Instead of the formulas used here, you can use the linear system
//    approach in the routine TRIANGLE_OUTCIRCLE_2D.
//
//    The diameter of the circle can be found by solving a 2 by 2 linear system.
//    This is because the vectors P2 - P1 and P3 - P1 are secants of the circle,
//    and each forms a right triangle with the diameter.  Hence, the dot product
//    of P2 - P1 with the diameter is equal to the square of the length
//    of P2 - P1, and similarly for P3 - P1.  These two equations determine the
//    diameter vector originating at P1.
//
//    If all three points are equal, return a circle of radius 0 and
//    the obvious center.
//
//    If two points are equal, return a circle of radius half the distance
//    between the two distinct points, and center their average.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 March 2006
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Joseph ORourke,
//    Computational Geometry,
//    Second Edition,
//    Cambridge, 1998,
//    ISBN: 0521649765,
//    LC: QA448.D38.
//
//  Parameters:
//
//    Input, double P1[2], P2[2], P3[2], are the coordinates
//    of three points that lie on the circle.  These points should be
//    distinct, and not collinear.
//
//    Output, double *R, the radius of the circle.  Normally, R will be positive.
//    R will be (meaningfully) zero if all three points are
//    equal.  If two points are equal, R is returned as the distance between
//    two nonequal points.  R is returned as -1 in the unlikely event that
//    the points are numerically collinear; philosophically speaking, R
//    should actually be "infinity" in this case.
//
//    Output, double PC[2], the center of the circle.
//
{
# define DIM_NUM 2

  double a;
  double b;
  double c;
  double d;
  double e;
  double f;
  double g;
//
//  If all three points are equal, then the
//  circle of radius 0 and center P1 passes through the points.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) && r8vec_eq ( DIM_NUM, p1, p3 ) )
  {
    *r = 0.0;
    r8vec_copy ( DIM_NUM, p1, pc );
    return;
  }
//
//  If exactly two points are equal, then the circle is defined as
//  having the obvious radius and center.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) )
  {
    *r = 0.5 * sqrt ( ( p1[0] - p3[0] ) * ( p1[0] - p3[0] )
                    + ( p1[1] - p3[1] ) * ( p1[1] - p3[1] ) );
    pc[0] = 0.5 * ( p1[0] + p3[0] );
    pc[1] = 0.5 * ( p1[1] + p3[1] );
    return;
  }
  else if ( r8vec_eq ( DIM_NUM, p1, p3 ) )
  {
    *r = 0.5 * sqrt ( ( p1[0] - p2[0] ) * ( p1[0] - p2[0] )
                    + ( p1[1] - p2[1] ) * ( p1[1] - p2[1] ) );
    pc[0] = 0.5 * ( p1[0] + p2[0] );
    pc[1] = 0.5 * ( p1[1] + p2[1] );
    return;
  }
  else if ( r8vec_eq ( DIM_NUM, p2, p3 ) )
  {
    *r = 0.5 * sqrt ( ( p1[0] - p2[0] ) * ( p1[0] - p2[0] )
                    + ( p1[1] - p2[1] ) * ( p1[1] - p2[1] ) );
    pc[0] = 0.5 * ( p1[0] + p2[0] );
    pc[1] = 0.5 * ( p1[1] + p2[1] );
    return;
  }

  a = p2[0] - p1[0];
  b = p2[1] - p1[1];
  c = p3[0] - p1[0];
  d = p3[1] - p1[1];

  e = a * ( p1[0] + p2[0] ) + b * ( p1[1] + p2[1] );
  f = c * ( p1[0] + p3[0] ) + d * ( p1[1] + p3[1] );
//
//  Our formula is:
//
//    G = a * ( d - b ) - b * ( c - a )
//
//  but we get slightly better results using the original data.
//
  g = a * ( p3[1] - p2[1] ) - b * ( p3[0] - p2[0] );
//
//  We check for collinearity.  A more useful check would compare the
//  absolute value of G to a small quantity.
//
  if ( g == 0.0 )
  {
    pc[0] = 0.0;
    pc[1] = 0.0;
    *r = -1.0;
    return;
  }
//
//  The center is halfway along the diameter vector from P1.
//
  pc[0] = 0.5 * ( d * e - b * f ) / g;
  pc[1] = 0.5 * ( a * f - c * e ) / g;
//
//  Knowing the center, the radius is now easy to compute.
//
  *r = sqrt ( ( p1[0] - pc[0] ) * ( p1[0] - pc[0] )
            + ( p1[1] - pc[1] ) * ( p1[1] - pc[1] ) );

  return;
# undef DIM_NUM
}
//****************************************************************************80

bool circle_imp_contains_point_2d ( double r, double pc[2], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_IMP_CONTAINS_POINT_2D determines if an implicit circle contains a point in 2D.
//
//  Discussion:
//
//    An implicit circle in 2D satisfies the equation:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the coordinates of the center of the circle.
//
//    Input, double P[2], the point to be checked.
//
//    Output, bool CIRCLE_IMP_CONTAINS_POINT_2D, is true if the point
//    is inside or on the circle, false otherwise.
//
{
  if ( pow ( p[0] - pc[0], 2 ) + pow ( p[1] - pc[1], 2 ) <= r * r )
  {
    return true;
  }
  else
  {
    return false;
  }
}
//****************************************************************************80

double circle_imp_line_exp_dist_2d ( double r, double pc[], double p1[], 
  double p2[] )

//****************************************************************************80
//
//  Purpose:
//
//    circle_imp_line_exp_dist_2d: distance ( implicit circle, explicit line ) in 2D.
//
//  Discussion:
//
//    The distance is zero if the line intersects the circle.
//
//    Points P on an implicit circle in 2D satisfy the equation:
//
//      ( P(1) - PC(1) )^2 + ( P(2) - PC(2) )^2 = R^2
//
//    The explicit form of a line in 2D is:
//
//      the line through the points P1 and P2.
//
//    The distance between the circle and the line is zero if
//    and only if they intersect.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    16 September 2020
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    double R, the radius of the circle.
//
//    double PC[2], the center of the circle.
//
//    double P1[2], P2[2], two points on the line.
//
//  Output:
//
//    double circle_imp_line_exp_dist_2d, the distance of the line to the circle.
//
{
  double dist;

  dist = line_exp_point_dist_2d ( p1, p2, pc );

  dist = dist - r;

  if ( dist < 0.0 )
  {
    dist = 0.0;
  }

  return dist;
}
//****************************************************************************80

void circle_imp_line_par_int_2d ( double r, double pc[2], double x0, double y0,
  double f, double g, int *int_num, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_IMP_LINE_PAR_INT_2D: ( implicit circle, parametric line ) intersection in 2D.
//
//  Discussion:
//
//    An implicit circle in 2D satisfies the equation:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//    The parametric form of a line in 2D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//
//    For normalization, we choose F*F+G*G = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the coordinates of the center of the circle.
//
//    Input, double F, G, X0, Y0, the parametric parameters of the line.
//
//    Output, int *INT_NUM, the number of intersecting points found.
//    INT_NUM will be 0, 1 or 2.
//
//    Output, double P[2*2], contains the X and Y coordinates of
//    the intersecting points.
//
{
  double root;
  double t;

  root = r * r * ( f * f + g * g )
    - ( f * ( pc[1] - y0 ) - g * ( pc[0] - x0 ) )
    * ( f * ( pc[1] - y0 ) - g * ( pc[0] - x0 ) );

  if ( root < 0.0 )
  {
    *int_num = 0;
  }
  else if ( root == 0.0 )
  {

    *int_num = 1;

    t = ( f * ( pc[0] - x0 ) + g * ( pc[1] - y0 ) ) / ( f * f + g * g );
    p[0+0*2] = x0 + f * t;
    p[1+0*2] = y0 + g * t;

  }
  else if ( 0.0 < root )
  {

    *int_num = 2;

    t = ( ( f * ( pc[0] - x0 ) + g * ( pc[1] - y0 ) ) - sqrt ( root ) )
      / ( f * f + g * g );

    p[0+0*2] = x0 + f * t;
    p[1+0*2] = y0 + g * t;

    t = ( ( f * ( pc[0] - x0 ) + g * ( pc[1] - y0 ) ) + sqrt ( root ) )
      / ( f * f + g * g );

    p[0+1*2] = x0 + f * t;
    p[1+1*2] = y0 + g * t;
  }

  return;
}
//****************************************************************************80

double circle_imp_point_dist_2d ( double r, double pc[2], double p[2]  )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_IMP_POINT_DIST_2D: distance ( implicit circle, point ) in 2D.
//
//  Discussion:
//
//    The distance is zero if the point is on the circle.
//
//    An implicit circle in 2D satisfies the equation:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the coordinates of the center of the circle.
//
//    Input, double P[2], the point to be checked.
//
//    Output, double CIRCLE_IMP_POINT_DIST_2D, the distance of the point
//    to the circle.
//
{
  double value;

  value = sqrt ( fabs ( pow ( p[0] - pc[0], 2 ) + pow ( p[1] - pc[1], 2 )
    - r * r ) );

  return value;
}
//****************************************************************************80

double circle_imp_point_dist_signed_2d ( double r, double pc[2], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_IMP_POINT_DIST_SIGNED_2D: signed distance ( implicit circle, point ) in 2D.
//
//  Discussion:
//
//    The signed distance is zero if the point is on the circle.
//    The signed distance is negative if the point is inside the circle.
//
//    An implicit circle in 2D satisfies the equation:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the coordinates of the center of the circle.
//
//    Input, double P[2], the point to be checked.
//
//    Output, double CIRCLE_IMP_POINT_DIST_SIGNED_2D, the signed distance
//    of the point to the circle.  If the point is inside the circle,
//    the signed distance is negative.
//
{
  double t;
  double value;

  t = pow ( p[0] - pc[0], 2 ) + pow ( p[1] - pc[1], 2 ) - r * r;

  value = r8_sign ( t ) * sqrt ( fabs ( t ) );

  return value;
}
//****************************************************************************80

double circle_imp_point_near_2d ( double r, double pc[2], double p[2],
  double pn[2] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_IMP_POINT_NEAR_2D: nearest ( implicit circle, point ) in 2D.
//
//  Discussion:
//
//    This routine finds the distance from a point to an implicitly
//    defined circle, and returns the point on the circle that is
//    nearest to the given point.
//
//    If the given point is the center of the circle, than any point
//    on the circle is "the" nearest.
//
//    An implicit circle in 2D satisfies the equation:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the center of the circle.
//
//    Input, double P[2], the point to be checked.
//
//    Output, double PN[2], the nearest point on the circle.
//
//    Output, double CIRCLE_IMP_POINT_NEAR_2D, the distance of the point to the circle.
//
{
# define DIM_NUM 2

  double dist;
  int i;
  double r2;

  if ( r8vec_eq ( DIM_NUM, p, pc ) )
  {
    dist = r;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      pn[i] = pc[i] + r / sqrt ( ( double ) ( DIM_NUM ) );
    }
    return dist;
  }

  r2 = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    r2 = r2 + pow ( p[i] - pc[i], 2 );
  }
  r2 = sqrt ( r2 );

  dist = fabs (  r2 - r );

  for ( i = 0; i < DIM_NUM; i++ )
  {
    pn[i] = pc[i] + r * ( p[i] - pc[i] ) / r2;
  }

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

double *circle_imp_points_2d ( double r, double pc[2], int n )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_IMP_POINTS_2D returns N equally spaced points on an implicit circle in 2D.
//
//  Discussion:
//
//    The first point is always ( PC[0] + R, PC[1] ), and subsequent points
//    proceed counter clockwise around the circle.
//
//    An implicit circle in 2D satisfies the equation:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 March 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the coordinates of the center of the circle.
//
//    Input, int N, the number of points desired.  N must be at least 1.
//
//    Output, double CIRCLE_IMP_POINTS_2D[2*N], points on the circle.
//
{
  double angle;
  int j;
  double *p;
  const double r8_pi = 3.141592653589793;

  p = new double[2*n];

  for ( j = 0; j < n; j++ )
  {
    angle = ( 2.0 * r8_pi * ( double ) j ) / ( double ) n ;
    p[0+j*2] = pc[0] + r * cos ( angle );
    p[1+j*2] = pc[1] + r * sin ( angle );
  }

  return p;
}
//****************************************************************************80

double *circle_imp_points_3d ( double r, double pc[3], double nc[3], int n )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_IMP_POINTS_3D returns points on an implicit circle in 3D.
//
//  Discussion:
//
//    Points P on an implicit circle in 3D satisfy the equations:
//
//      ( P(1) - PC(1) )^2
//    + ( P(2) - PC(2) )^2
//    + ( P(3) - PC(3) )^2 = R^2
//
//    and
//
//      ( P(1) - PC(1) ) * NC(1)
//    + ( P(2) - PC(2) ) * NC(2)
//    + ( P(3) - PC(3) ) * NC(3) = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 March 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[3], the center of the circle.
//
//    Input, double NC[3], a nonzero vector that is normal to
//    the plane of the circle.  It is customary, but not necessary,
//    that this vector have unit norm.
//
//    Input, int N, the number of points desired.
//    N must be at least 1.
//
//    Output, double CIRCLE_IMP_POINTS_3D[3*N], the coordinates of points
//    on the circle.
//
{
# define DIM_NUM 3

  int i;
  int j;
  double *n1;
  double *n2;
  double *p;
  const double r8_pi = 3.141592653589793;
  double theta;
//
//  Get two unit vectors N1 and N2 which are orthogonal to each other,
//  and to NC.
//
  n1 = new double[DIM_NUM];
  n2 = new double[DIM_NUM];

  plane_normal_basis_3d ( pc, nc, n1, n2 );
//
//  Rotate R units away from PC in the plane of N1 and N2.
//
  p = new double[DIM_NUM*n];

  for ( j = 0; j < n; j++ )
  {
    theta = ( 2.0 * r8_pi * ( double ) ( j ) ) / ( double ) ( n );

    for ( i = 0; i < DIM_NUM; i++ )
    {
      p[i+j*DIM_NUM] = pc[i] + r * ( cos ( theta ) * n1[i]
                                   + sin ( theta ) * n2[i] );
    }
  }

  delete [] n1;
  delete [] n2;

  return p;
# undef DIM_NUM
}
//****************************************************************************80

void circle_imp_points_arc_2d ( double r, double pc[2], double theta1,
  double theta2, int n, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_IMP_POINTS_ARC_2D returns N points on an arc of an implicit circle in 2D.
//
//  Discussion:
//
//    The first point is ( PC[0] + R * COS ( THETA1 ), PC[1] + R * SIN ( THETA1 ) );
//    The last point is  ( PC[0] + R * COS ( THETA2 ), PC[1] + R * SIN ( THETA2 ) );
//    and the intermediate points are evenly spaced in angle between these,
//    and in counter clockwise order.
//
//    An implicit circle in 2D satisfies the equation:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the coordinates of the center of the circle.
//
//    Input, double THETA1, THETA2, the angular coordinates of the first
//    and last points to be drawn, in radians.
//
//    Input, int N, the number of points desired.  N must be at least 1.
//
//    Output, double P[2*N], the coordinates of points on the circle.
//
{
  int i;
  double r8_pi = 3.141592653589793;
  double theta;
  double theta3;
//
//  THETA3 is the smallest angle, no less than THETA1, which
//  coincides with THETA2.
//
  theta3 = theta1 + r8_modp ( theta2 - theta1, 2.0 * r8_pi );

  for ( i = 0; i < n; i++ )
  {
    if ( 1 < n )
    {
      theta = ( ( double ) ( n - i - 1 ) * theta1
              + ( double ) (     i     ) * theta3 )
              / ( double ) ( n     - 1 );
    }
    else
    {
      theta = 0.5 * ( theta1 + theta3 );
    }

    p[0+i*2] = pc[0] + r * cos ( theta );
    p[1+i*2] = pc[1] + r * sin ( theta );
  }

  return;
}
//****************************************************************************80

void circle_imp_print_2d ( double r, double pc[2], string title )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_IMP_PRINT_2D prints an implicit circle in 2D.
//
//  Discussion:
//
//    An implicit circle in 2D satisfies the equation:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the center of the circle.
//
//    Input, string TITLE, a title.
//
{
# define DIM_NUM 2

  cout << "\n";
  cout << title << "\n";
  cout << "\n";
  cout << "  Radius = " << r << "\n";
  cout << "  Center = (" << pc[0] << ",  " << pc[1] << ")\n";

  return;
# undef DIM_NUM
}
//****************************************************************************80

void circle_imp_print_3d ( double r, double pc[3], double nc[3], string title )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_IMP_PRINT_2D prints an implicit circle in 3D.
//
//  Discussion:
//
//    Points P on an implicit circle in 3D satisfy the equations:
//
//      ( P(1) - PC(1) )^2
//    + ( P(2) - PC(2) )^2
//    + ( P(3) - PC(3) )^2 = R^2
//
//    and
//
//      ( P(1) - PC(1) ) * NC(1)
//    + ( P(2) - PC(2) ) * NC(2)
//    + ( P(3) - PC(3) ) * NC(3) = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 March 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[3], the center of the circle.
//
//    Input, double NC[3], the normal vector to the circle.
//
//    Input, string TITLE, a title.
//
{
# define DIM_NUM 3

  cout << "\n";
  cout << title << "\n";
  cout << "\n";
  cout << "  Radius = " << r << "\n";
  cout << "  Center = (" << pc[0]
                 << ", " << pc[1]
                 << ", " << pc[2] << ")\n";
  cout << "  Normal = (" << nc[0]
                 << ", " << nc[1]
                 << ", " << nc[2] << ")\n";

  return;
# undef DIM_NUM
}
//****************************************************************************80

void circle_imp2exp_2d ( double r, double pc[2], double p1[2], double p2[2],
  double p3[2] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_IMP2EXP_2D converts a circle from implicit to explicit form in 2D.
//
//  Discussion:
//
//    Points P on an implicit circle in 2D satisfy the equation:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//    The explicit form of a circle in 2D is:
//
//      The circle passing through points P1, P2 and P3.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 May 2007
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Joseph ORourke,
//    Computational Geometry,
//    Second Edition,
//    Cambridge, 1998,
//    ISBN: 0521649765,
//    LC: QA448.D38.
//
//  Parameters:
//
//    Input, double R, PC[2], the radius and center of the circle.
//
//    Output, double P1[2], P2[2], P3[2], three points on the circle.
//
{
  const double r8_pi = 3.141592653589793;
  double theta;

  theta = 0.0;
  p1[0] = pc[0] + r * cos ( theta );
  p1[1] = pc[1] + r * sin ( theta );

  theta = 2.0 * r8_pi / 3.0;
  p2[0] = pc[0] + r * cos ( theta );
  p2[1] = pc[1] + r * sin ( theta );

  theta = 4.0 * r8_pi / 3.0;
  p3[0] = pc[0] + r * cos ( theta );
  p3[1] = pc[1] + r * sin ( theta );

  return;
}
//****************************************************************************80

double *circle_llr2imp_2d ( double p1[], double p2[], double q1[], double q2[],
  double r )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_LLR2IMP_2D converts a circle from LLR to implicit form in 2D.
//
//  Discussion:
//
//    The LLR form of a circle in 2D is:
//
//      The circle of radius R tangent to the lines L1 and L2.
//
//    The implicit form of a circle in 2D is:
//
//      ( P(1) - PC(1) )^2 + ( P(2) - PC(2) )^2 = R^2
//
//    Let S be the scaled distance of a point on L1 from P1 to P2,
//    and let N1 be a unit normal vector to L1.  Then a point P that is
//    R units from L1 satisfies:
//
//      P = P1 + s * ( P2 - P1 ) + R * N1.
//
//    Let t be the scaled distance of a point on L2 from Q1 to Q2,
//    and let N2 be a unit normal vector to L2.  Then a point Q that is
//    R units from L2 satisfies:
//
//      Q = Q1 + t * ( Q2 - Q1 ) + R * N2.
//
//    For the center of the circle, then, we have P = Q, that is
//
//      ( P2 - P1 ) * s + ( Q2 - Q1 ) * t = - P1 - Q1 - R * ( N1 + N2 )
//
//    This is a linear system for ( s and t ) from which we can compute
//    the points of tangency, and the center.
//
//    Note that we have four choices for the circle based on the use
//    of plus or minus N1 and plus or minus N2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 November 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], two points on line 1.
//
//    Input, double Q1[2], Q2[2], two points on line 2.
//
//    Input, double R, the radius of the circle.
//
//    Output, double CIRCLE_LLR2IMP_2D[2*4], the centers of the circles.
//
{
  double *a;
  double *b;
  double det;
  double *n1;
  double *n2;
  double *pc;
  double *x;

  a = new double[2*2];
  b = new double[2];
  pc = new double[2*4];
//
//  Compute the normals N1 and N2.
//
  n1 = line_exp_normal_2d ( p1, p2 );

  n2 = line_exp_normal_2d ( q1, q2 );
//
//  Set the linear system.
//
  a[0+0*2] =   p2[0] - p1[0];
  a[1+0*2] =   p2[1] - p1[1];
  a[0+1*2] = - q2[0] + q1[0];
  a[1+1*2] = - q2[1] + q1[1];
//
//  Solve the 4 linear systems, using every combination of
//  signs on the normal vectors.
//
  b[0] = - p1[0] + q1[0] + r * n1[0] + r * n2[0];
  b[1] = - p1[1] + q1[1] + r * n1[1] + r * n2[1];

  x = r8mat_solve_2d ( a, b, &det );

  pc[0+2*0] = p1[0] + ( p2[0] - p1[0] ) * x[0] - r * n1[0];
  pc[1+2*0] = p1[1] + ( p2[1] - p1[1] ) * x[0] - r * n1[1];

  delete [] x;

  b[0] = - p1[0] + q1[0] + r * n1[0] - r * n2[0];
  b[1] = - p1[1] + q1[1] + r * n1[1] - r * n2[1];

  x = r8mat_solve_2d ( a, b, &det );

  pc[0+2*1] = p1[0] + ( p2[0] - p1[0] ) * x[0] - r * n1[0];
  pc[1+2*1] = p1[1] + ( p2[1] - p1[1] ) * x[0] - r * n1[1];

  delete [] x;

  b[0] = - p1[0] + q1[0] - r * n1[0] + r * n2[0];
  b[1] = - p1[1] + q1[1] - r * n1[1] + r * n2[1];

  x = r8mat_solve_2d ( a, b, &det );

  pc[0+2*2] = p1[0] + ( p2[0] - p1[0] ) * x[0] + r * n1[0];
  pc[1+2*2] = p1[1] + ( p2[1] - p1[1] ) * x[0] + r * n1[1];

  delete [] x;

  b[0] = - p1[0] + q1[0] - r * n1[0] - r * n2[0];
  b[1] = - p1[1] + q1[1] - r * n1[1] - r * n2[1];

  x = r8mat_solve_2d ( a, b, &det );

  pc[0+2*3] = p1[0] + ( p2[0] - p1[0] ) * x[0] + r * n1[0];
  pc[1+2*3] = p1[1] + ( p2[1] - p1[1] ) * x[0] + r * n1[1];

  delete [] a;
  delete [] b;
  delete [] n1;
  delete [] n2;
  delete [] x;

  return pc;
}
//****************************************************************************80

double circle_lune_angle_by_height_2d ( double r, double h )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_LUNE_ANGLE_BY_HEIGHT_2D computes the angle of a circular lune.
//
//  Discussion:
//
//    Draw the chord connecting two points on the circumference of a circle.
//    The region between the chord and the circumference is a "lune".
//    We wish to know the angle subtended by the lune.
//
//    The distance from the center of the circle to the midpoint of the chord
//    is the "height" H of the lune.  It is natural to expect 0 <= H <= R.
//    However, if we allow -R <= H < 0 as well, this allows us to include
//    lunes which involve more than half the circle's area.
//
//    If H < -R or R < H, then no lune is formed, and we return a zero angle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    15 January 2018
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double H, the height of the lune.
//
//    Output, double ANGLE, the angle of the lune.
//
{
  double angle;

  if ( -r <= h && h <= r )
  {
    angle = 2.0 * acos ( h / r );
  }
  else
  {
    angle = 0.0;
  }

  return angle;
}
//****************************************************************************80

double circle_lune_area_by_angle_2d ( double r, double pc[2], double theta1,
  double theta2 )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_LUNE_AREA_BY_ANGLE_2D returns the area of a circular lune in 2D.
//
//  Discussion:
//
//    A lune is formed by drawing a circular arc, and joining its endpoints.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the center of the circle.
//
//    Input, double THETA1, THETA2, the angles defining the arc,
//    in radians.  Normally, THETA1 < THETA2.
//
//    Output, double CIRCLE_LUNE_AREA_BY_ANGLE_2D, the area of the lune.
//
{
  double area;
  double area_sector;
  double area_triangle;

  area_sector = circle_sector_area_2d ( r, pc, theta1, theta2 );
  area_triangle = circle_triangle_area_2d ( r, pc, theta1, theta2 );
  area = area_sector - area_triangle;

  return area;
}
//****************************************************************************80

double circle_lune_area_by_height_2d ( double r, double h )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_LUNE_AREA_BY_HEIGHT_2D returns the area of a circular lune in 2D.
//
//  Discussion:
//
//    A lune is formed by drawing a circular arc, and joining its endpoints.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    15 January 2018
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double H, the height of the lune.
//
//    Output, double CIRCLE_LUNE_AREA_BY_HEIGHT_2D, the area of the lune.
//
{
  double area;

  if ( -r <= h && h <= r )
  {
    area = r * r * acos ( h / r ) - h * sqrt ( r * r - h * h );
  }
  else
  {
    area = 0.0;
  }

  return area;
}
//****************************************************************************80

double *circle_lune_centroid_2d ( double r, double pc[2], double theta1,
  double theta2 )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_LUNE_CENTROID_2D returns the centroid of a circular lune in 2D.
//
//  Discussion:
//
//    A lune is formed by drawing a circular arc, and joining its endpoints.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the coordinates of the center of the circle.
//
//    Input, double THETA1, THETA2, the angles of the first and last points
//    on the circular arc.
//
//    Output, double CIRCLE_LUNE_CENTROID_2D[2], the coordinates of the centroid
//    of the lune.
//
{
  double *centroid;
  double d;
  double theta;

  theta = theta2 - theta1;

  if ( theta == 0.0 )
  {
    d = r;
  }
  else
  {
    d = 4.0 * r * pow ( ( sin ( 0.5 * theta ) ), 3 ) /
      ( 3.0 * ( theta - sin ( theta ) ) );
  }

  centroid = new double[2];

  centroid[0] = pc[0] + d * cos ( theta );
  centroid[1] = pc[1] + d * sin ( theta );

  return centroid;
}
//****************************************************************************80

double circle_lune_height_by_angle_2d ( double r, double angle )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_LUNE_HEIGHT_BY_ANGLE_2D computes the height of a circular lune.
//
//  Discussion:
//
//    Draw the chord connecting two points on the circumference of a circle.
//    The region between the chord and the circumference is a "lune".
//    The lune subtends a given angle between 0 and 2 pi.
//
//    The distance from the center of the circle to the midpoint of the chord
//    is the "height" H of the lune and we wish to determine this value.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    14 January 2018
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double ANGLE, the angle subtended by the lune.
//
//    Output, double HEIGHT, the height of the lune
//
{
  double height;

  height = r * cos ( angle / 2.0 );

  return height;
}
//****************************************************************************80

void circle_pppr2imp_3d ( double p1[], double p2[], double p3[], double r,
  double pc[], double normal[] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_PPR2IMP_3D converts a circle from PPR to implicit form in 3D.
//
//  Discussion:
//
//    The PPPR form of a circle in 3D is:
//
//      The circle of radius R passing through points P1 and P2,
//      and lying in the plane of P1, P2 and P3.
//
//    The implicit form of a circle in 3D is:
//
//      ( P(1) - PC(1) )^2 + ( P(2) - PC(2) )^2 + ( P(3) - PC(3) )^2 = R^2
//      and the dot product of P - PC with NORMAL is 0.
//
//    There may be zero, one, or two circles that satisfy the
//    requirements of the PPPR form.
//
//    If there is no such circle, then PC(1:2,1) and PC(1:2,2)
//    are set to the midpoint of (P1,P2).
//
//    If there is one circle, PC(1:2,1) and PC(1:2,2) will be equal.
//
//    If there are two circles, then PC(1:2,1) is the first center,
//    and PC(1:2,2) is the second.
//
//    This calculation is equivalent to finding the intersections of
//    spheres of radius R at points P1 and P2, which lie in the plane
//    defined by P1, P2 and P3.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 November 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], two points on the circle.
//
//    Input, double P3[3], a third point.
//
//    Input, double R, the radius of the circle.
//
//    Output, double PC[3*2], the centers of the two circles.
//
//    Output, double NORMAL[3], the normal to the circles.
//
{
# define DIM_NUM 3

  double dist;
  double dot;
  double h;
  int i;
  int j;
  double length;
  double *v;
//
//  Compute the distance from P1 to P2.
//
  dist = r8vec_distance ( DIM_NUM, p1, p2 );
//
//  If R is smaller than DIST, we don't have a circle.
//
  if ( 2.0 * r < dist )
  {
    for ( j = 0; j < 2; j++ )
    {
      for ( i = 0; i < DIM_NUM; i++ )
      {
        pc[i+j*DIM_NUM] = 0.5 * ( p1[i] + p2[i] );
      }
    }
  }
//
//  H is the distance from the midpoint of (P1,P2) to the center.
//
  h = sqrt ( ( r + 0.5 * dist ) * ( r - 0.5 * dist ) );
//
//  Define a unit direction V that is normal to P2-P1, and lying
//  in the plane (P1,P2,P3).
//
//  To do this, subtract from P3-P1 the component in the direction P2-P1.
//
  v = new double[DIM_NUM];

  for ( i = 0; i < DIM_NUM; i++ )
  {
    v[i] = p3[i] - p1[i];
  }
  dot = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    dot = dot + v[i] * ( p2[i] - p1[i] );
  }
  dot = dot / dist;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v[i] = v[i] - dot * ( p2[i] - p1[i] ) / dist;
  }

  length = r8vec_norm ( DIM_NUM, v );
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v[i] = v[i] / length;
  }
//
//  We can go with or against the given normal direction.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    pc[i+0*DIM_NUM] = 0.5 * ( p2[i] + p1[i] ) + h * v[i];
    pc[i+1*DIM_NUM] = 0.5 * ( p2[i] + p1[i] ) - h * v[i];
  }
  delete [] v;

  plane_exp_normal_3d ( p1, p2, p3, normal );

  return;
# undef DIM_NUM
}
//****************************************************************************80

double *circle_ppr2imp_2d ( double p1[], double p2[], double r )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_PPR2IMP_2D converts a circle from PPR to implicit form in 2D.
//
//  Discussion:
//
//    The PPR form of a circle in 2D is:
//
//      The circle of radius R passing through points P1 and P2.
//
//    The implicit form of a circle in 2D is:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = R * R
//
//    There may be zero, one, or two circles that satisfy the
//    requirements of the PPR form.
//
//    If there is no such circle, then the two "solutions" are set to
//    the midpoint of P1 and P2.
//
//    If there is one circle, then the two solutions will be set equal
//    to the midpoint of P1 and P2.
//
//    If there are two distinct circles, then (PC[0],PC[1]) is the first center,
//    and (PC[2],PC[3]) is the second.
//
//    This calculation is equivalent to finding the intersections of
//    circles of radius R at points P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 November 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], two points on the circle.
//
//    Input, double R, the radius of the circle.
//
//    Output, double PC[2*2], the centers of the two circles.
//
{
# define DIM_NUM 2

  double dist;
  double h;
  int i;
  int j;
  double *pc;

  pc = new double[DIM_NUM*2];
//
//  Compute the distance from P1 to P2.
//
  dist = sqrt ( pow ( p2[0] - p1[0], 2 ) + pow ( p2[1] - p1[1], 2 ) );
//
//  If R is smaller than DIST, we don't have a circle.
//
  if ( 2.0 * r < dist )
  {
    for ( j = 0; j < 2; j++ )
    {
      for ( i = 0; i < DIM_NUM; i++ )
      {
        pc[i+j*DIM_NUM] = 0.5 * ( p1[i] + p2[i] );
      }
    }
  }
//
//  H is the distance from the midpoint of (P1,P2) to the center.
//
  h = sqrt ( ( r + 0.5 * dist ) * ( r - 0.5 * dist ) );
//
//  The center is found by going midway between P1 and P2, and then
//  H units in the unit perpendicular direction.
//
//  We actually have two choices for the normal direction.
//
  pc[0+0*DIM_NUM] = 0.5 * ( p2[0] + p1[0] ) + h * ( p2[1] - p1[1] ) / dist;
  pc[1+0*DIM_NUM] = 0.5 * ( p2[1] + p1[1] ) - h * ( p2[0] - p1[0] ) / dist;

  pc[0+1*DIM_NUM] = 0.5 * ( p2[0] + p1[0] ) - h * ( p2[1] - p1[1] ) / dist;
  pc[1+1*DIM_NUM] = 0.5 * ( p2[1] + p1[1] ) + h * ( p2[0] - p1[0] ) / dist;

  return pc;
# undef DIM_NUM
}
//****************************************************************************80

double circle_sector_area_2d ( double r, double pc[2], double theta1,
  double theta2 )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_SECTOR_AREA_2D computes the area of a circular sector in 2D.
//
//  Discussion:
//
//    A circular sector is formed by a circular arc, and the two straight line
//    segments that join its ends to the center of the circle.
//
//    A circular sector is defined by
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//    and
//
//      Theta1 <= Theta <= Theta2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the coordinates of the center of the circle.
//
//    Input, double THETA1, THETA2, the angles of the first and last points
//    on the circular arc.
//
//    Output, double CIRCLE_SECTOR_AREA_2D, the area of the circle.
//
{
  double area;

  area = 0.5 * r * r * ( theta2 - theta1 );

  return area;
}
//****************************************************************************80

double *circle_sector_centroid_2d ( double r, double pc[2], double theta1,
  double theta2 )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_SECTOR_CENTROID_2D returns the centroid of a circular sector in 2D.
//
//  Discussion:
//
//    A circular sector is formed by a circular arc, and the two straight line
//    segments that join its ends to the center of the circle.
//
//    A circular sector is defined by
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//    and
//
//      Theta1 <= Theta <= Theta2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the coordinates of the center of the circle.
//
//    Input, double THETA1, THETA2, the angles of the first and last points
//    on the circular arc.
//
//    Output, double CIRCLE_SECTOR_CENTROID_2D[2], the coordinates
//    of the centroid of the sector.
//
{
# define DIM_NUM 2

  double *centroid;
  double d;
  double theta;

  theta = theta2 - theta1;

  if ( theta == 0.0 )
  {
    d = 2.0 * r / 3.0;
  }
  else
  {
    d = 4.0 * r * sin ( 0.5 * theta ) / ( 3.0 * theta );
  }

  centroid = new double[DIM_NUM];

  centroid[0] = pc[0] + d * cos ( theta );
  centroid[1] = pc[1] + d * sin ( theta );

  return centroid;
# undef DIM_NUM
}
//****************************************************************************80

bool circle_sector_contains_point_2d ( double r, double pc[2], double theta1,
  double theta2, double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_SECTOR_CONTAINS_POINT_2D : is a point inside a circular sector?
//
//  Discussion:
//
//    A circular sector is formed by a circular arc, and the two straight line
//    segments that join its ends to the center of the circle.
//
//    A circular sector is defined by
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//    and
//
//      Theta1 <= Theta <= Theta2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 October 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the center of the circle.
//
//    Input, double THETA1, THETA2, the angles defining the arc,
//    in radians.  Normally, THETA1 < THETA2.
//
//    Input, double P[2], the point to be checked.
//
//    Output, logical CIRCLE_SECTOR_CONTAINS_POINT_2D, is TRUE if the point is
//    inside or on the circular sector.
//
{
  bool inside;
  double r8_pi = 3.141592653589793;
  double theta;

  inside = false;
//
//  Is the point inside the (full) circle?
//
  if ( pow ( p[0] - pc[0], 2 ) + pow ( p[1] - pc[1], 2 ) <= r * r )
  {
//
//  Is the point's angle within the arc's range?
//  Try to force the angles to lie between 0 and 2 * PI.
//
    theta = r8_atan ( p[1] - pc[1], p[0] - pc[0] );

    if ( r8_modp ( theta - theta1, 2.0 * r8_pi ) <=
         r8_modp ( theta2 - theta1, 2.0 * r8_pi ) )
    {
      inside = true;
    }
  }

  return inside;
}
//****************************************************************************80

void circle_sector_print_2d ( double r, double pc[2], double theta1,
  double theta2 )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_SECTOR_PRINT_2D prints a circular sector in 2D.
//
//  Discussion:
//
//    A circular sector is formed by a circular arc, and the two straight line
//    segments that join its ends to the center of the circle.
//
//    A circular sector is defined by
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//    and
//
//      Theta1 <= Theta <= Theta2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the center of the circle.
//
//    Input, double THETA1, THETA2, the angles defining the arc,
//    in radians.  Normally, THETA1 < THETA2.
//
{
  cout << "\n";
  cout << "  Circular sector definition:\n";
  cout << "\n";
  cout << "    Radius = " << setw(12) << r << "\n";
  cout << "    Center = " << setw(12) << pc[0]
       << "  "            << setw(12) << pc[1] << "\n";
  cout << "    Theta  = " << setw(12) << theta1
       << "  "            << setw(12) << theta2 << "\n";

  return;
}
//****************************************************************************80

double circle_triangle_area_2d ( double r, double pc[2], double theta1,
  double theta2 )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_TRIANGLE_AREA_2D returns the area of a circle triangle in 2D.
//
//  Discussion:
//
//    A circle triangle is formed by drawing a circular arc, and considering
//    the triangle formed by the endpoints of the arc plus the center of
//    the circle.
//
//    Note that for angles greater than PI, the triangle will actually
//    have NEGATIVE area.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the circle.
//
//    Input, double PC[2], the center of the circle.
//
//    Input, double THETA1, THETA2, the angles defining the arc,
//    in radians.  Normally, THETA1 < THETA2.
//
//    Output, double AREA, the (signed) area of the triangle.
//
{
  double area;

  area = 0.5 * r * r * sin ( theta2 - theta1 );

  return area;
}
//****************************************************************************80

void circle_triple_angles_2d ( double r1, double r2, double r3, double *angle1,
  double *angle2, double *angle3 )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_TRIPLE_ANGLE_2D returns an angle formed by three circles in 2D.
//
//  Discussion:
//
//    A circle triple is a set of three tangent circles.  We assume
//    that no circle is contained in another.
//
//    We consider the triangle formed by joining the centers of the circles.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Kenneth Stephenson,
//    Circle Packing, The Theory of Discrete Analytic Functions,
//    Cambridge, 2005.
//
//  Parameters:
//
//    Input, double R1, R2, R3, the radii of the circles.
//
//    Input, double *ANGLE1, *ANGLE2, *ANGLE3, the angles
//    in the triangle.
//
{
  *angle1 = r8_acos (
    pow ( r1 + r2, 2 ) + pow ( r1 + r3, 2 ) - pow ( r2 + r3, 2 ) ) /
    ( 2.0 * ( r1 + r2 ) * ( r1 + r3 ) );

  *angle2 = r8_acos (
    pow ( r2 + r3, 2 ) + pow ( r2 + r1, 2 ) - pow ( r3 + r1, 2 ) ) /
    ( 2.0 * ( r2 + r3 ) * ( r2 + r1 ) );

  *angle3 = r8_acos (
    pow ( r3 + r1, 2 ) + pow ( r3 + r2, 2 ) - pow ( r1 + r2, 2 ) ) /
    ( 2.0 * ( r3 + r1 ) * ( r3 + r2 ) );

  return;
}
//****************************************************************************80

double circles_intersect_area_2d ( double r1, double r2, double d )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLES_INTERSECT_AREA_2D: area of the intersection of two circles.
//
//  Discussion:
//
//    Circles of radius R1 and R2 are D units apart.  What is the area of
//    intersection?
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    15 January 2018
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R1, R2, the radiuses of the circles.
//    R1 and R2 should be positive.
//
//    Input, double D, the distance between the circular centers.
//    D must be positive, and should be no greater than R1 + R2.
//
//    Output, double AREA, the area of the intersection.
//
{
  double area;
  double area1;
  double area2;
  double h1;
  double h2;
  double r8_pi = 3.141592653589793;

  if ( r1 + r2 < d )
  {
    area = 0.0;
  }
  else if ( d == 0.0 )
  {
    area = r8_pi * pow ( fmin ( r1, r2 ), 2 );
  }
  else
  {
    h1 = 0.5 * ( d * d + r1 * r1 - r2 * r2 ) / d;
    area1 = circle_lune_area_by_height_2d ( r1, h1 );
    h2 = 0.5 * ( d * d - r1 * r1 + r2 * r2 ) / d;
    area2 = circle_lune_area_by_height_2d ( r2, h2 );
    area = area1 + area2;
  }

  return area;
}
//****************************************************************************80

void circles_intersect_points_2d ( double r1, double pc1[2], double r2, 
  double pc2[2], int *int_num, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLES_INTERSECT_POINTS_2D: intersection points of two circles in 2D.
//
//  Discussion:
//
//    Two circles can intersect in 0, 1, 2 or infinitely many points.
//
//    The 0 and 2 intersection cases are numerically robust; the 1 and
//    infinite intersection cases are numerically fragile.  The routine
//    uses a tolerance to try to detect the 1 and infinite cases.
//
//    An implicit circle in 2D satisfies the equation:
//
//      pow ( P[0] - PC[0], 2 ) + pow ( P[1] - PC[1], 2 ) = pow ( R, 2 )
//
//    Thanks to Mario Pintaric for pointing out, on 13 March 2006,
//    a place where (R1-R2) had been mistakenly written as (R1-R1).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 March 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R1, the radius of the first circle.
//
//    Input, double PC1[2], the coordinates of the center of the first circle.
//
//    Input, double R2, the radius of the second circle.
//
//    Input, double PC2[2], the coordinates of the center of the second circle.
//
//    Output, int *INT_NUM, the number of intersecting points found.
//    INT_NUM will be 0, 1, 2 or 3.  3 indicates that there are an infinite
//    number of intersection points.
//
//    Output, double P[2*2], if INT_NUM is 1 or 2, the
//    coordinates of the intersecting points.
//
{
  double distsq;
  double root;
  double sc1;
  double sc2;
  double t1;
  double t2;
  double tol;

  tol = DBL_EPSILON;

  p[0+0*2] = 0.0;
  p[1+0*2] = 0.0;
  p[0+1*2] = 0.0;
  p[1+1*2] = 0.0;
//
//  Take care of the case in which the circles have the same center.
//
  t1 = ( fabs ( pc1[0] - pc2[0] ) + fabs ( pc1[1] - pc2[1] ) ) / 2.0;
  t2 = ( fabs ( pc1[0] ) + fabs ( pc2[0] )
       + fabs ( pc1[1] ) + fabs ( pc2[1] ) + 1.0 ) / 5.0;

  if ( t1 <= tol * t2 )
  {
    t1 = fabs ( r1 - r2 );
    t2 = ( fabs ( r1 ) + fabs ( r2 ) + 1.0 ) / 3.0;

    if ( t1 <= tol * t2 )
    {
      *int_num = 3;
    }
    else
    {
      *int_num = 0;
    }
    return;
  }

  distsq = ( pc1[0] - pc2[0] ) * ( pc1[0] - pc2[0] )
         + ( pc1[1] - pc2[1] ) * ( pc1[1] - pc2[1] );

  root = 2.0 * ( r1 * r1 + r2 * r2 ) * distsq - distsq * distsq
    - ( r1 - r2 ) * ( r1 - r2 ) * ( r1 + r2 ) * ( r1 + r2 );

  if ( root < -tol )
  {
    *int_num = 0;
    return;
  }

  sc1 = ( distsq - ( r2 * r2 - r1 * r1 ) ) / distsq;

  if ( root < tol )
  {
    *int_num = 1;
    p[0+0*2] = pc1[0] + 0.5 * sc1 * ( pc2[0] - pc1[0] );
    p[1+0*2] = pc1[1] + 0.5 * sc1 * ( pc2[1] - pc1[1] );
    return;
  }

  sc2 = sqrt ( root ) / distsq;

  *int_num = 2;

  p[0+0*2] = pc1[0] + 0.5 * sc1 * ( pc2[0] - pc1[0] )
                    - 0.5 * sc2 * ( pc2[1] - pc1[1] );
  p[1+0*2] = pc1[1] + 0.5 * sc1 * ( pc2[1] - pc1[1] )
                    + 0.5 * sc2 * ( pc2[0] - pc1[0] );

  p[0+1*2] = pc1[0] + 0.5 * sc1 * ( pc2[0] - pc1[0] )
                    + 0.5 * sc2 * ( pc2[1] - pc1[1] );
  p[1+1*2] = pc1[1] + 0.5 * sc1 * ( pc2[1] - pc1[1] )
                    - 0.5 * sc2 * ( pc2[0] - pc1[0] );

  return;
}
//****************************************************************************80

double cone_area_3d ( double h, double r )

//****************************************************************************80
//
//  Purpose:
//
//    CONE_AREA_3D computes the surface area of a right circular cone in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double H, R, the height of the cone, and the radius of the
//    circle that forms the base of the cone.
//
//    Output, double CONE_AREA_3D, the surface area of the cone.
//
{
  double area;
  double r8_pi = 3.141592653589793;

  area = r8_pi * r * sqrt ( h * h + r * r );

  return area;
}
//****************************************************************************80

double *cone_centroid_3d ( double r, double pc[3], double pt[3] )

//****************************************************************************80
//
//  Purpose:
//
//    CONE_CENTROID_3D returns the centroid of a cone in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double R, the radius of the circle at the base of the cone.
//
//    Input, double PC[3], the coordinates of the center of the circle.
//
//    Input, double PT[3], the coordinates of the tip of the cone.
//
//    Output, double CONE_CENTROID_3D[3], the coordinates of the centroid of the cone.
//
{
  double *centroid;
  int dim_num = 3;
  int i;

  centroid = new double[3];

  for ( i = 0; i < dim_num; i++ )
  {
    centroid[i] = 0.75 * pc[i] + 0.25 * pt[i];
  }

  return centroid;
}
//****************************************************************************80

double cone_volume_3d ( double h, double r )

//****************************************************************************80
//
//  Purpose:
//
//    CONE_VOLUME_3D computes the volume of a right circular cone in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double H, R, the height of the cone, and the radius of the
//    circle that forms the base of the cone.
//
//    Output, double CONE_VOLUME_3D, the volume of the cone.
//
{
  double r8_pi = 3.141592653589793;
  double volume;

  volume = r8_pi * r * r * h / 3.0;

  return volume;
}
//****************************************************************************80

void conv3d ( char axis, double theta, int n, double cor3[], double cor2[] )

//****************************************************************************80
//
//  Purpose:
//
//    CONV3D converts 3D data to a 2D projection.
//
//  Discussion:
//
//    A "presentation angle" THETA is used to project the 3D point
//    (X3D, Y3D, Z3D) to the 2D projection (XVAL,YVAL).
//
//    If COR = 'X':
//
//      X2D = Y3D - sin ( THETA ) * X3D
//      Y2D = Z3D - sin ( THETA ) * X3D
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, char AXIS, the coordinate to be projected.
//    AXIS should be 'X', 'Y', or 'Z'.
//
//    Input, double THETA, the presentation angle in degrees.
//
//    Input, int N, the number of values to be projected.
//
//    Input, double COR3[3*N], the point coordinates.
//
//    Output, double COR2[2*N], the 2D projections.
//
{
  int j;
  double stheta;

  stheta = sin ( degrees_to_radians ( theta ) );

  if ( axis == 'X' || axis == 'x' )
  {
    for ( j = 0; j < n; j++ )
    {
      cor2[0+j*2] = cor3[2+j*2] - stheta * cor3[0+j*2];
      cor2[1+j*2] = cor3[3+j*2] - stheta * cor3[0+j*2];
    }
  }
  else if ( axis == 'Y' || axis == 'y' )
  {
    for ( j = 0; j < n; j++ )
    {
      cor2[0+j*2] = cor3[0+j*2] - stheta * cor3[1+j*2];
      cor2[1+j*2] = cor3[2+j*2] - stheta * cor3[1+j*2];
    }
  }
  else if ( axis == 'Z' || axis == 'z' )
  {
    for ( j = 0; j < n; j++ )
    {
      cor2[0+j*2] = cor3[0+j*2] - stheta * cor3[2+j*2];
      cor2[1+j*2] = cor3[1+j*2] - stheta * cor3[2+j*2];
    }
  }
  else
  {
    cerr << "\n";
    cerr << "CONV3D - Fatal error!\n";
    cerr << "  Illegal coordinate index = " << axis << "\n";
    exit ( 1 );
  }

  return;
}
//****************************************************************************80

double cot_rad ( double angle )

//****************************************************************************80
//
//  Purpose:
//
//    COT_RAD returns the cotangent of an angle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    21 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double ANGLE, the angle, in radians.
//
//    Output, double COT_RAD, the cotangent of the angle.
//
{
  double value;

  value = cos ( angle ) / sin ( angle );

  return value;
}
//****************************************************************************80

void cube_shape_3d ( int point_num, int face_num, int face_order_max,
  double point_coord[], int face_order[], int face_point[] )

//****************************************************************************80
//
//  Purpose:
//
//    CUBE_SHAPE_3D describes a cube in 3D.
//
//  Discussion:
//
//    The vertices lie on the unit sphere.
//
//    The dual of the octahedron is the cube.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int POINT_NUM, the number of points.
//
//    Input, int FACE_NUM, the number of faces.
//
//    Input, int FACE_ORDER_MAX, the maximum number of vertices
//    per face.
//
//    Output, double POINT_COORD[3*POINT_NUM], the point coordinates.
//
//    Output, int FACE_ORDER[FACE_NUM], the number of vertices per face.
//
//    Output, int FACE_POINT[FACE_ORDER_MAX*FACE_NUM]; FACE_POINT(I,J)
//    contains the index of the I-th point in the J-th face.  The
//    points are listed in the counter clockwise direction defined
//    by the outward normal at the face.
//
{
# define DIM_NUM 3

  double a = sqrt ( 1.0 / 3.0 );

  static int face_order_save[6] = {
    4, 4, 4, 4, 4, 4 };
  static int face_point_save[4*6] = {
     1, 4, 3, 2,
     1, 2, 6, 5,
     2, 3, 7, 6,
     3, 4, 8, 7,
     1, 5, 8, 4,
     5, 6, 7, 8 };
  static double point_coord_save[DIM_NUM*8] = {
     -a, -a, -a,
      a, -a, -a,
      a,  a, -a,
     -a,  a, -a,
     -a, -a,  a,
      a, -a,  a,
      a,  a,  a,
     -a,  a,  a };

  i4vec_copy ( face_num, face_order_save, face_order );
  i4vec_copy ( face_order_max*face_num, face_point_save, face_point );
  r8vec_copy ( DIM_NUM*point_num, point_coord_save, point_coord );

  return;
# undef DIM_NUM
}
//****************************************************************************80

void cube_size_3d ( int *point_num, int *edge_num, int *face_num,
  int *face_order_max )

//****************************************************************************80
//
//  Purpose:
//
//    CUBE_SIZE_3D gives "sizes" for a cube in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 July 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, int *POINT_NUM, the number of points.
//
//    Output, int *EDGE_NUM, the number of edges.
//
//    Output, int *FACE_NUM, the number of faces.
//
//    Output, int *FACE_ORDER_MAX, the maximum order of any face.
//
{
  *point_num = 8;
  *edge_num = 12;
  *face_num = 6;
  *face_order_max = 4;

  return;
}
//****************************************************************************80

double cube01_volume ( )

//****************************************************************************80
//
//  Purpose:
//
//    CUBE01_VOLUME returns the volume of the unit cube in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    18 January 2018
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, double CUBE01_VOLUME, the volume.
//
{
  double volume;
  
  volume = 1.0;

  return volume;
}
//****************************************************************************80

double cylinder_point_dist_3d ( double p1[3], double p2[3], double r,
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    CYLINDER_POINT_DIST_3D determines the distance from a cylinder to a point in 3D.
//
//  Discussion:
//
//    We are computing the distance to the SURFACE of the cylinder.
//
//    The surface of a (right) (finite) cylinder in 3D is defined by an axis,
//    which is the line segment from point P1 to P2, and a radius R.  The points
//    on the surface of the cylinder are:
//    * points at a distance R from the line through P1 and P2, and whose nearest
//      point on the line through P1 and P2 is strictly between P1 and P2,
//    PLUS
//    * points at a distance less than or equal to R from the line through P1
//      and P2, whose nearest point on the line through P1 and P2 is either
//      P1 or P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the first and last points
//    on the axis line of the cylinder.
//
//    Input, double R, the radius of the cylinder.
//
//    Input, double P[3], the point.
//
//    Output, double CYLINDER_POINT_DIST_3D, the distance from the point
//    to the cylinder.
//
{
# define DIM_NUM 3

  double axis[DIM_NUM];
  double axis_length = 0.0;
  double distance = 0.0;
  int i;
  double off_axis_component = 0.0;
  double p_dot_axis = 0.0;
  double p_length = 0.0;
  double v1[DIM_NUM];

  for ( i = 0; i < DIM_NUM; i++ )
  {
    axis[i] = p2[i] - p1[i];
  }

  axis_length = r8vec_norm ( DIM_NUM, axis );

  if ( axis_length == 0.0 )
  {
    distance = -HUGE_VAL;
    return distance;
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    axis[i] = axis[i] / axis_length;
  }

  p_dot_axis = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    p_dot_axis = p_dot_axis + ( p[i] - p1[i] ) * axis[i];
  }
//
//  Case 1: Below bottom cap.
//
  if ( p_dot_axis <= 0.0 )
  {
    distance = disk_point_dist_3d ( p1, r, axis, p );
  }
//
//  Case 2: between cylinder planes.
//
  else if ( p_dot_axis <= axis_length )
  {
    for ( i = 0; i < DIM_NUM; i++ )
    {
      v1[i] = p[i] - p1[i];
    }
    p_length = r8vec_norm ( DIM_NUM, v1 );
    off_axis_component = sqrt ( pow ( p_length, 2 ) - pow ( p_dot_axis, 2 ) );

    distance = fabs ( off_axis_component - r );

    if ( off_axis_component < r )
    {
      distance = fmin ( distance, axis_length - p_dot_axis );
      distance = fmin ( distance, p_dot_axis );
    }
  }
//
//  Case 3: Above the top cap.
//
  else if ( axis_length < p_dot_axis )
  {
    distance = disk_point_dist_3d ( p2, r, axis, p );
  }

  return distance;
# undef DIM_NUM
}
//****************************************************************************80

double cylinder_point_dist_signed_3d ( double p1[3], double p2[3], double r,
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    CYLINDER_POINT_DIST_SIGNED_3D: signed distance from cylinder to point in 3D.
//
//  Discussion:
//
//    We are computing the signed distance to the SURFACE of the cylinder.
//
//    The surface of a (right) (finite) cylinder in 3D is defined by an axis,
//    which is the line segment from point P1 to P2, and a radius R.  The points
//    on the surface of the cylinder are:
//    * points at a distance R from the line through P1 and P2, and whose nearest
//      point on the line through P1 and P2 is strictly between P1 and P2,
//    PLUS
//    * points at a distance less than or equal to R from the line through P1
//      and P2, whose nearest point on the line through P1 and P2 is either
//      P1 or P2.
//
//    Points inside the surface have a negative distance.
//    Points on the surface have a zero distance.
//    Points outside the surface have a positive distance.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    26 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the first and last points
//    on the axis line of the cylinder.
//
//    Input, double R, the radius of the cylinder.
//
//    Input, double P[3], the point.
//
//    Output, double CYLINDER_POINT_DIST_SIGNED_3D, the signed distance from the point
//    to the cylinder.
//
{
# define DIM_NUM 3

  double axis[DIM_NUM];
  double axis_length = 0.0;
  double distance = 0.0;
  int i;
  double off_axis_component = 0.0;
  double p_dot_axis = 0.0;
  double p_length = 0.0;
  double v1[DIM_NUM];

  for ( i = 0; i < DIM_NUM; i++ )
  {
    axis[i] = p2[i] - p1[i];
  }

  axis_length = r8vec_norm ( DIM_NUM, axis );

  if ( axis_length == 0.0 )
  {
    distance = -HUGE_VAL;
    return distance;
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    axis[i] = axis[i] / axis_length;
  }

  p_dot_axis = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    p_dot_axis = p_dot_axis + ( p[i] - p1[i] ) * axis[i];
  }
//
//  Case 1: Below bottom cap.
//
  if ( p_dot_axis <= 0.0 )
  {
    distance = disk_point_dist_3d ( p1, r, axis, p );
  }
//
//  Case 2: between cylinder planes.
//
  else if ( p_dot_axis <= axis_length )
  {
    for ( i = 0; i < DIM_NUM; i++ )
    {
      v1[i] = p[i] - p1[i];
    }
    p_length = r8vec_norm ( DIM_NUM, v1 );
    off_axis_component = sqrt ( pow ( p_length, 2 ) - pow ( p_dot_axis, 2 ) );

    distance = off_axis_component - r;

    if ( distance < 0.0 )
    {
      distance = fmax ( distance, p_dot_axis - axis_length);
      distance = fmax ( distance, -p_dot_axis );
    }
  }
//
//  Case 3: Above the top cap.
//
  else if ( axis_length < p_dot_axis )
  {
    distance = disk_point_dist_3d ( p2, r, axis, p );
  }

  return distance;
# undef DIM_NUM
}
//****************************************************************************80

bool cylinder_point_inside_3d ( double p1[3], double p2[3], double r,
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    CYLINDER_POINT_INSIDE_3D determines if a cylinder contains a point in 3D.
//
//  Discussion:
//
//    The surface and interior of a (right) (finite) cylinder in 3D is defined
//    by an axis, which is the line segment from point P1 to P2, and a
//    radius R.  The points contained in the volume include:
//    * points at a distance less than or equal to R from the line through P1
//      and P2, whose nearest point on the line through P1 and P2 is, in fact,
//      P1, P2, or any point between them.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the first and last points
//    on the axis line of the cylinder.
//
//    Input, double R, the radius of the cylinder.
//
//    Input, double P[3], the point.
//
//    Output, bool CYLINDER_POINT_INSIDE_3D, is TRUE if the point is inside
//    the cylinder.
//
{
# define DIM_NUM 3

  double axis[DIM_NUM];
  double axis_length;
  int i;
  bool inside;
  double off_axis_component;
  double p_dot_axis;
  double p_length;
  double v1[DIM_NUM];

  for ( i = 0; i < DIM_NUM; i++ )
  {
    axis[i] = p2[i] - p1[i];
  }

  axis_length = r8vec_norm ( DIM_NUM, axis );

  if ( axis_length == 0.0 )
  {
    inside = false;
    return inside;
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    axis[i] = axis[i] / axis_length;
  }

  p_dot_axis = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    p_dot_axis = p_dot_axis + ( p[i] - p1[i] ) * axis[i];
  }
//
//  If the point lies below or above the "caps" of the cylinder, we're done.
//
  if ( p_dot_axis < 0.0 || axis_length < p_dot_axis )
  {
    inside = false;
  }
//
//  Otherwise, determine the distance from P to the axis.
//
  else
  {
    for ( i = 0; i < DIM_NUM; i++ )
    {
      v1[i] = p[i] - p1[i];
    }
    p_length = r8vec_norm ( DIM_NUM, v1 );

    off_axis_component = sqrt ( pow ( p_length, 2 ) - pow ( p_dot_axis, 2 ) );

    if ( off_axis_component <= r )
    {
      inside = true;
    }
    else
    {
      inside = false;
    }
  }

  return inside;
# undef DIM_NUM
}
//****************************************************************************80

double *cylinder_point_near_3d ( double p1[3], double p2[3], double r,
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    CYLINDER_POINT_NEAR_3D: nearest point on a cylinder to a point in 3D.
//
//  Discussion:
//
//    We are computing the nearest point on the SURFACE of the cylinder.
//
//    The surface of a (right) (finite) cylinder in 3D is defined by an axis,
//    which is the line segment from point P1 to P2, and a radius R.  The points
//    on the surface of the cylinder are:
//    * points at a distance R from the line through P1 and P2, and whose nearest
//      point on the line through P1 and P2 is strictly between P1 and P2,
//    PLUS
//    * points at a distance less than or equal to R from the line through P1
//      and P2, whose nearest point on the line through P1 and P2 is either
//      P1 or P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the first and last points
//    on the axis line of the cylinder.
//
//    Input, double R, the radius of the cylinder.
//
//    Input, double P[3], the point.
//
//    Output, double CYLINDER_POINT_NEAR_3D[3], the nearest point on the cylinder.
//
{
# define DIM_NUM 3

  double axial_component;
  double axis[DIM_NUM];
  double axis_length;
  double distance;
  int i;
  double *normal;
  double off_axis[DIM_NUM];
  double off_axis_component;
  double *pn;

  pn = new double[DIM_NUM];

  for ( i = 0; i < DIM_NUM; i++ )
  {
    axis[i] = p2[i] - p1[i];
  }
  axis_length = r8vec_norm ( DIM_NUM, axis );
  for ( i = 0; i < DIM_NUM; i++ )
  {
    axis[i] = axis[i] / axis_length;
  }

  axial_component = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    axial_component = axial_component + ( p[i] - p1[i] ) * axis[i];
  }
  for ( i = 0; i < DIM_NUM; i++ )
  {
    off_axis[i] = p[i] - p1[i] - axial_component * axis[i];
  }

  off_axis_component = r8vec_norm ( DIM_NUM, off_axis );
//
//  Case 1: Below bottom cap.
//
  if ( axial_component <= 0.0 )
  {
    if ( off_axis_component <= r )
    {
      for ( i = 0; i < DIM_NUM; i++ )
      {
        pn[i] = p1[i] + off_axis[i];
      }
    }
    else
    {
      for ( i = 0; i < DIM_NUM; i++ )
      {
        pn[i] = p1[i] + ( r / off_axis_component ) * off_axis[i];
      }
    }
  }
//
//  Case 2: between cylinder planes.
//
  else if ( axial_component <= axis_length )
  {
    if ( off_axis_component == 0.0 )
    {
      normal = r8vec_any_normal ( DIM_NUM, axis );
      for ( i = 0; i < DIM_NUM; i++ )
      {
        pn[i] = p[i] + r * normal[i];
      }
      delete [] normal;
    }
    else
    {
      distance = fabs ( off_axis_component - r );

      for ( i = 0; i < DIM_NUM; i++ )
      {
        pn[i] = p1[i] + axial_component * axis[i]
              + ( r / off_axis_component ) * off_axis[i];
      }
      if ( off_axis_component < r )
      {
        if ( axis_length - axial_component < distance )
        {
          distance = axis_length - axial_component;
          for ( i = 0; i < DIM_NUM; i++ )
          {
            pn[i] = p2[i] + off_axis[i];
          }
        }
        if ( axial_component < distance )
        {
          distance = axial_component;
          for ( i = 0; i < DIM_NUM; i++ )
          {
            pn[i] = p1[i] + off_axis[i];
          }
        }
      }
    }
  }
//
//  Case 3: Above the top cap.
//
  else if ( axis_length < axial_component )
  {
    if ( off_axis_component <= r )
    {
      for ( i = 0; i < DIM_NUM; i++ )
      {
        pn[i] = p2[i] + off_axis[i];
      }
    }
    else
    {
      for ( i = 0; i < DIM_NUM; i++ )
      {
        pn[i] = p2[i] + ( r / off_axis_component ) * off_axis[i];
      }
    }
  }

  return pn;
# undef DIM_NUM
}
//****************************************************************************80

double *cylinder_sample_3d ( double p1[3], double p2[3], double r, int n,
  int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    CYLINDER_SAMPLE_3D samples a cylinder in 3D.
//
//  Discussion:
//
//    We are sampling the interior of a right finite cylinder in 3D.
//
//    The interior of a (right) (finite) cylinder in 3D is defined by an axis,
//    which is the line segment from point P1 to P2, and a radius R.  The points
//    on or inside the cylinder are:
//    * points whose distance from the line through P1 and P2 is less than
//      or equal to R, and whose nearest point on the line through P1 and P2
//      lies (nonstrictly) between P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the first and last points
//    on the axis line of the cylinder.
//
//    Input, double R, the radius of the cylinder.
//
//    Input, int N, the number of sample points to compute.
//
//    Input/output, int &SEED, the random number seed.
//
//    Input, double CYLINDER_SAMPLE_3D[3*N], the sample points.
//
{
# define DIM_NUM 3

  double axis[DIM_NUM];
  double axis_length;
  int i;
  int j;
  double *p;
  double r8_pi = 3.141592653589793;
  double radius;
  double theta;
  double v2[DIM_NUM];
  double v3[DIM_NUM];
  double z;
//
//  Compute the axis vector.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    axis[i] = p2[i] - p1[i];
  }
  axis_length = r8vec_norm ( DIM_NUM, axis );
  for ( i = 0; i < DIM_NUM; i++ )
  {
    axis[i] = axis[i] / axis_length;
  }
//
//  Compute vectors V2 and V3 that form an orthogonal triple with AXIS.
//
  plane_normal_basis_3d ( p1, axis, v2, v3 );
//
//  Assemble the randomized information.
//
  p = new double[DIM_NUM*n];

  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < DIM_NUM; i++ )
    {
      radius = r * sqrt ( r8_uniform_01 ( seed ) );
      theta = 2.0 * r8_pi * r8_uniform_01 ( seed );
      z = axis_length * r8_uniform_01 ( seed );

      p[i+j*DIM_NUM] = p1[i]
        + z                      * axis[i]
        + radius * cos ( theta ) * v2[i]
        + radius * sin ( theta ) * v3[i];
    }
  }

  return p;
# undef DIM_NUM
}
//****************************************************************************80

double cylinder_volume_3d ( double p1[3], double p2[3], double r )

//****************************************************************************80
//
//  Purpose:
//
//    CYLINDER_VOLUME_3D determines the volume of a cylinder in 3D.
//
//  Discussion:
//
//    A (right) (finite) cylinder in 3D is the set of points
//    contained on or inside a circle of radius R, whose center
//    lies along the line segment from point P1 to P2, and whose
//    plane is perpendicular to that line segment.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the first and last points
//    on the axis line of the cylinder.
//
//    Input, double R, the radius of the cylinder.
//
//    Output, double CYLINDER_VOLUME_3D, the volume of the cylinder.
//
{
  double h;
  double r8_pi = 3.141592653589793;
  double volume;

  h = r8vec_distance ( 3, p1, p2 );

  volume = r8_pi * r * r * h;

  return volume;
}
//****************************************************************************80

double degrees_to_radians ( double angle )

//****************************************************************************80
//
//  Purpose:
//
//    DEGREES_TO_RADIANS converts an angle from degrees to radians.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double ANGLE, an angle in degrees.
//
//    Output, double DEGREES_TO_RADIANS, the equivalent angle
//    in radians.
//
{
  double r8_pi = 3.141592653589793;
  double value;

  value = ( angle / 180.0 ) * r8_pi;

  return value;
}
//****************************************************************************80

double dge_det ( int n, double a[], int pivot[] )

//****************************************************************************80
//
//  Purpose:
//
//    DGE_DET computes the determinant of a matrix factored by SGE_FA.
//
//  Discussion:
//
//    The doubly dimensioned array A is treated as a one dimensional vector,
//    stored by COLUMNS:
//
//      A(0,0), A(1,0), A(2,0), ..., A(N-1,0) // A(1,0), A(1,1), ... A(N-1,1)
//
//    Entry A(I,J) is stored as A[I+J*N]
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Jack Dongarra, James Bunch, Cleve Moler, Pete Stewart,
//    LINPACK User's Guide,
//    SIAM, 1979
//
//  Parameters:
//
//    Input, int N, the order of the matrix.
//    N must be positive.
//
//    Input, double A[N*N], the LU factors computed by DGE_FA.
//
//    Input, int PIVOT[N], as computed by DGE_FA.
//
//    Output, double DGE_DET, the determinant of the matrix.
//
{
  double det;
  int i;

  det = 1.0;

  for ( i = 0; i < n; i++ )
  {
    det = det * a[i+i*n];
    if ( pivot[i] != i + 1 )
    {
      det = -det;
    }
  }

  return det;
}
//****************************************************************************80

int dge_fa ( int n, double a[], int pivot[] )

//****************************************************************************80
//
//  Purpose:
//
//    DGE_FA factors a general matrix.
//
//  Discussion:
//
//    DGE_FA is a simplified version of the LINPACK routine SGEFA.
//
//    The doubly dimensioned array A is treated as a one dimensional vector,
//    stored by COLUMNS:
//
//      A(0,0), A(1,0), A(2,0), ..., A(N-1,0) // A(1,0), A(1,1), ... A(N-1,1)
//
//    Entry A(I,J) is stored as A[I+J*N]
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Jack Dongarra, James Bunch, Cleve Moler, Pete Stewart,
//    LINPACK User's Guide,
//    SIAM, 1979
//
//  Parameters:
//
//    Input, int N, the order of the matrix.
//    N must be positive.
//
//    Input/output, double A[N*N], the matrix to be factored.
//    On output, A contains an upper triangular matrix and the multipliers
//    which were used to obtain it.  The factorization can be written
//    A = L * U, where L is a product of permutation and unit lower
//    triangular matrices and U is upper triangular.
//
//    Output, int PIVOT[N], a vector of pivot indices.
//
//    Output, int DGE_FA, singularity flag.
//    0, no singularity detected.
//    nonzero, the factorization failed on the DGE_FA-th step.
//
{
  int i;
  int ii;
  int info;
  int j;
  int k;
  int l;
  double t;

  info = 0;

  for ( k = 1; k <= n-1; k++ )
  {
//
//  Find L, the index of the pivot row.
//
    l = k;
    for ( i = k+1; i <= n; i++ )
    {
      if ( fabs ( a[l-1+(k-1)*n] ) < fabs ( a[i-1+(k-1)*n] ) )
      {
        l = i;
      }
    }

    pivot[k-1] = l;
//
//  If the pivot index is zero, the algorithm has failed.
//
    if ( a[l-1+(k-1)*n] == 0.0 )
    {
      info = k;
      cout << "\n";
      cout << "DGE_FA - Warning!\n";
      cout << "  Zero pivot on step " << info << "\n";
      return info;
    }
//
//  Interchange rows L and K if necessary.
//
    if ( l != k )
    {
      t              = a[l-1+(k-1)*n];
      a[l-1+(k-1)*n] = a[k-1+(k-1)*n];
      a[k-1+(k-1)*n] = t;
    }
//
//  Normalize the values that lie below the pivot entry A(K,K).
//
    for ( j = k+1; j <= n; j++ )
    {
      a[j-1+(k-1)*n] = -a[j-1+(k-1)*n] / a[k-1+(k-1)*n];
    }
//
//  Row elimination with column indexing.
//
    for ( j = k+1; j <= n; j++ )
    {
      if ( l != k )
      {
        t              = a[l-1+(j-1)*n];
        a[l-1+(j-1)*n] = a[k-1+(j-1)*n];
        a[k-1+(j-1)*n] = t;
      }

      for ( ii = k; ii < n; ii++ )
      {
        a[ii+(j-1)*n] = a[ii+(j-1)*n] + a[ii+(k-1)*n] * a[k-1+(j-1)*n];
      }
    }
  }

  pivot[n-1] = n;

  if ( a[n-1+(n-1)*n] == 0.0 )
  {
    info = n;
    cout << "\n";
    cout << "DGE_FA - Warning!\n";
    cout << "  Zero pivot on step " << info << "\n";
  }

  return info;
}
//****************************************************************************80

void dge_sl ( int n, double a[], int pivot[], double b[], int job )

//****************************************************************************80
//
//  Purpose:
//
//    DGE_SL solves a system factored by SGE_FA.
//
//  Discussion:
//
//    DGE_SL is a simplified version of the LINPACK routine SGESL.
//
//    The doubly dimensioned array A is treated as a one dimensional vector,
//    stored by COLUMNS:
//
//      A(0,0), A(1,0), A(2,0), ..., A(N-1,0) // A(1,0), A(1,1), ... A(N-1,1)
//
//    Entry A(I,J) is stored as A[I+J*N]
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the order of the matrix.
//    N must be positive.
//
//    Input, double A[N*N], the LU factors from DGE_FA.
//
//    Input, int PIVOT[N], the pivot vector from DGE_FA.
//
//    Input/output, double B[N].
//    On input, the right hand side vector.
//    On output, the solution vector.
//
//    Input, int JOB, specifies the operation.
//    0, solve A * x = b.
//    nonzero, solve A' * x = b.
//
{
  int i;
  int k;
  int l;
  double t;
//
//  Solve A * x = b.
//
  if ( job == 0 )
  {
//
//  Solve PL * Y = B.
//
    for ( k = 1; k <= n-1; k++ )
    {
      l = pivot[k-1];

      if ( l != k )
      {
        t      = b[l-1];
        b[l-1] = b[k-1];
        b[k-1] = t;
      }

      for ( i = k+1; i <= n; i++ )
      {
        b[i-1] = b[i-1] + a[i-1+(k-1)*n] * b[k-1];
      }
    }
//
//  Solve U * X = Y.
//
    for ( k = n; 1 <= k; k-- )
    {
      b[k-1] = b[k-1] / a[k-1+(k-1)*n];
      for ( i = 1; i <= k-1; i++ )
      {
        b[i-1] = b[i-1] - a[i-1+(k-1)*n] * b[k-1];
      }
    }
//
//  Solve A' * X = B.
//
  }
  else
  {
//
//  Solve U' * Y = B.
//
    for ( k = 1; k <= n; k++ )
    {
      t = 0.0;
      for ( i = 1; i <= k-1; i++ )
      {
        t = t + b[i-1] * a[i-1+(k-1)*n];
      }
      b[k-1] = ( b[k-1] - t ) / a[k-1+(k-1)*n];
    }
//
//  Solve ( PL )' * X = Y.
//
    for ( k = n-1; 1 <= k; k-- )
    {
      t = 0.0;
      for ( i = k+1; i <= n; i++ )
      {
        t = t + b[i-1] * a[i-1+(k-1)*n];
      }
      b[k-1] = b[k-1] + t;

      l = pivot[k-1];

      if ( l != k )
      {
        t      = b[l-1];
        b[l-1] = b[k-1];
        b[k-1] = t;
      }
    }
  }

  return;
}
//****************************************************************************80

double *direction_pert_3d ( double sigma, double vbase[3], int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    DIRECTION_PERT_3D randomly perturbs a direction vector in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double SIGMA, determines the strength of the perturbation.
//    SIGMA <= 0 results in a completely random direction.
//    1 <= SIGMA results in VBASE.
//    0 < SIGMA < 1 results in a perturbation from VBASE, which is
//    large when SIGMA is near 0, and small when SIGMA is near 1.
//
//    Input, double VBASE[3], the base direction vector, which should have
//    unit norm.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double DIRECTION_PERT_3D[3], the perturbed vector, which will
//    have unit norm.
//
{
# define DIM_NUM 3

  double dphi;
  double p[DIM_NUM];
  double phi;
  double r8_pi = 3.141592653589793;
  double psi;
  double theta;
  double vdot;
  double *vran;
  double x;
//
//  0 <= SIGMA, just use the base vector.
//
  vran = new double[DIM_NUM];

  if ( 1.0 <= sigma )
  {
    r8vec_copy ( DIM_NUM, vbase, vran );
  }
  else if ( sigma <= 0.0 )
  {
    vdot = r8_uniform_01 ( seed );
    vdot = 2.0 * vdot - 1.0;
    phi = acos ( vdot );
    theta = r8_uniform_01 ( seed );
    theta = 2.0 * r8_pi * theta;

    vran[0] = cos ( theta ) * sin ( phi );
    vran[1] = sin ( theta ) * sin ( phi );
    vran[2] = cos ( phi );
  }
  else
  {
    phi = acos ( vbase[2] );
    theta = atan2 ( vbase[1], vbase[0] );
//
//  Pick VDOT, which must be between -1 and 1.  This represents
//  the dot product of the perturbed vector with the base vector.
//
//  r8_uniFORM_01 returns a uniformly random value between 0 and 1.
//  The operations we perform on this quantity tend to bias it
//  out towards 1, as SIGMA grows from 0 to 1.
//
//  VDOT, in turn, is a value between -1 and 1, which, for large
//  SIGMA, we want biased towards 1.
//
    x = r8_uniform_01 ( seed );
    x = exp ( ( 1.0 - sigma ) * log ( x ) );
    vdot = 2.0 * x - 1.0;
    dphi = acos ( vdot );
//
//  Now we know enough to write down a vector that is rotated DPHI
//  from the base vector.
//
    p[0] = cos ( theta ) * sin ( phi + dphi );
    p[1] = sin ( theta ) * sin ( phi + dphi );
    p[2] = cos ( phi + dphi );
//
//  Pick a uniformly random rotation between 0 and 2 Pi around the
//  axis of the base vector.
//
    psi = r8_uniform_01 ( seed );
    psi = 2.0 * r8_pi * psi;

    vector_rotate_3d ( p, vbase, psi, vran );
  }

  return vran;
# undef DIM_NUM
}
//****************************************************************************80

double *direction_uniform_2d ( int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    DIRECTION_UNIFORM_2D picks a random direction vector in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 June 2002
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double DIRECTION_UNIFORM_2D[2], the random direction vector,
//    with unit norm.
//
{
# define DIM_NUM 2

  double r8_pi = 3.141592653589793;
  double theta;
  double *vran;

  theta = r8_uniform_01 ( seed );
  theta = 2.0 * r8_pi * theta;

  vran = new double[DIM_NUM];

  vran[0] = cos ( theta );
  vran[1] = sin ( theta );

  return vran;
# undef DIM_NUM
}
//****************************************************************************80

double *direction_uniform_3d ( int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    DIRECTION_UNIFORM_3D picks a random direction vector in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double DIRECTION_UNIFORM_3D[3], the random direction vector,
//    with unit norm.
//
{
# define DIM_NUM 3

  double phi;
  double r8_pi = 3.141592653589793;
  double theta;
  double vdot;
  double *vran;
//
//  Pick a uniformly random VDOT, which must be between -1 and 1.
//  This represents the dot product of the random vector with the Z unit vector.
//
//  This works because the surface area of the sphere between
//  Z and Z + dZ is independent of Z.  So choosing Z uniformly chooses
//  a patch of area uniformly.
//
  vdot = r8_uniform_01 ( seed );
  vdot = 2.0 * vdot - 1.0;
  phi = acos ( vdot );
//
//  Pick a uniformly random rotation between 0 and 2 Pi around the
//  axis of the Z vector.
//
  theta = r8_uniform_01 ( seed );
  theta = 2.0 * r8_pi * theta;

  vran = new double[DIM_NUM];

  vran[0] = cos ( theta ) * sin ( phi );
  vran[1] = sin ( theta ) * sin ( phi );
  vran[2] = cos ( phi );

  return vran;
# undef DIM_NUM
}
//****************************************************************************80

double *direction_uniform_nd ( int dim_num, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    DIRECTION_UNIFORM_ND generates a random direction vector in ND.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double DIRECTION_UNIFORM_ND[DIM_NUM], a random direction vector, 
//    with unit norm.
//
{
  double *a;
//
//  Take DIM_NUM random samples from the normal distribution.
//
  a = r8vec_normal_01_new ( dim_num, seed );
//
//  Normalize the vector.
//
  vector_unit_nd ( dim_num, a );

  return a;
}
//****************************************************************************80

double disk_point_dist_3d ( double pc[3], double r, double axis[3],
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    DISK_POINT_DIST_3D determines the distance from a disk to a point in 3D.
//
//  Discussion:
//
//    A disk in 3D satisfies the equations:
//
//      ( P(1) - PC(1) )^2 + ( P(2) - PC(2) )^2 + ( P(3) - PC(3) <= R^2
//
//    and
//
//      P(1) * AXIS(1) + P(2) * AXIS(2) + P(3) * AXIS(3) = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PC(3), the center of the disk.
//
//    Input, double R, the radius of the disk.
//
//    Input, double AXIS(3), the axis vector.
//
//    Input, double P(3), the point to be checked.
//
//    Output, double DISK_POINT_DIST_3D, the distance of the
//    point to the disk.
//
{
# define DIM_NUM 3

  double axial_component;
  double axis_length;
  double dist;
  int i;
  double off_axis_component;
  double off_axis[DIM_NUM];
  double v[DIM_NUM];
//
//  Special case: the point is the center.
//
  if ( r8vec_eq ( DIM_NUM, p, pc ) )
  {
    dist = 0.0;
    return dist;
  }

  axis_length = r8vec_norm ( DIM_NUM, axis );

  if ( axis_length <= 0.0 )
  {
    dist = -HUGE_VAL;
    return dist;
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    v[i] = p[i] - pc[i];
  }

  axial_component = r8vec_dot_product ( DIM_NUM, v, axis ) / axis_length;
//
//  Special case: the point satisfies the disk equation exactly.
//
  if ( r8vec_norm ( DIM_NUM, v ) <= r && axial_component == 0.0 )
  {
    dist = 0.0;
    return dist;
  }
//
//  Decompose P-PC into axis component and off-axis component.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    off_axis[i] = p[i] - pc[i] - axial_component * axis[i] / axis_length;
  }
  off_axis_component = r8vec_norm ( DIM_NUM, off_axis );
//
//  If the off-axis component has norm less than R, the nearest point is
//  the projection to the disk along the axial direction, and the distance
//  is just the dot product of P-PC with unit AXIS.
//
  if ( off_axis_component <= r )
  {
    dist = fabs ( axial_component );
    return dist;
  }
//
//  Otherwise, the nearest point is along the perimeter of the disk.
//
  dist = sqrt ( pow ( axial_component, 2 )
              + pow ( off_axis_component - r, 2 ) );

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

double dms_to_radians ( int degrees, int minutes, int seconds )

//****************************************************************************80
//
//  Purpose:
//
//    DMS_TO_RADIANS converts an angle from degrees/minutes/seconds to radians.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DEGREES, MINUTES, SECONDS, an angle in degrees, minutes,
//    and seconds.
//
//    Output, double DMS_TO_RADIANS, the equivalent angle in radians.
//
{
  double angle;
  double r8_pi = 3.141592653589793;
  double radians;

  angle =   ( double ) degrees
        + ( ( ( double ) minutes )
        + ( ( ( double ) seconds ) / 60.0 ) ) / 60.0;

  radians = ( angle / 180.0 ) * r8_pi;

  return radians;
}
//****************************************************************************80

void dodec_shape_3d ( int point_num, int face_num, int face_order_max,
  double point_coord[], int face_order[], int face_point[] )

//****************************************************************************80
//
//  Purpose:
//
//    DODEC_SHAPE_3D describes a dodecahedron in 3D.
//
//  Discussion:
//
//    The vertices lie on the unit sphere.
//
//    The dual of a dodecahedron is the icosahedron.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 October 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int POINT_NUM, the number of points.
//
//    Input, int FACE_NUM, the number of faces.
//
//    Input, int FACE_ORDER_MAX, the maximum number of vertices
//    per face.
//
//    Output, double POINT_COORD[3*POINT_NUM], the point coordinates.
//
//    Output, int FACE_ORDER[FACE_NUM], the number of vertices per face.
//
//    Output, int FACE_POINT[FACE_ORDER_MAX*FACE_NUM]; FACE_POINT(I,J)
//    contains the index of the I-th point in the J-th face.  The
//    points are listed in the counter clockwise direction defined
//    by the outward normal at the face.
//
{
# define DIM_NUM 3

  double phi = 0.5 * ( sqrt ( 5.0 ) + 1.0 );

  double a = 1.0 / sqrt ( 3.0 );
  double b = phi / sqrt ( 3.0 );
  double c = ( phi - 1.0 ) / sqrt ( 3.0 );
  double z = 0.0;

  static int face_order_save[12] = {
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 };
  static int face_point_save[5*12] = {
      2,  9,  1, 13, 14,
      5, 10,  6, 16, 15,
      3, 11,  4, 14, 13,
      8, 12,  7, 15, 16,
      3, 13,  1, 17, 18,
      2, 14,  4, 20, 19,
      5, 15,  7, 18, 17,
      8, 16,  6, 19, 20,
      5, 17,  1,  9, 10,
      3, 18,  7, 12, 11,
      2, 19,  6, 10,  9,
      8, 20,  4, 11, 12 };
  static double point_coord_save[DIM_NUM*20] = {
      a,  a,  a,
      a,  a, -a,
      a, -a,  a,
      a, -a, -a,
     -a,  a,  a,
     -a,  a, -a,
     -a, -a,  a,
     -a, -a, -a,
      c,  b,  z,
     -c,  b,  z,
      c, -b,  z,
     -c, -b,  z,
      b,  z,  c,
      b,  z, -c,
     -b,  z,  c,
     -b,  z, -c,
      z,  c,  b,
      z, -c,  b,
      z,  c, -b,
      z, -c, -b };

  i4vec_copy ( face_num, face_order_save, face_order );
  i4vec_copy ( face_order_max*face_num, face_point_save, face_point );
  r8vec_copy ( DIM_NUM*point_num, point_coord_save, point_coord );

  return;
# undef DIM_NUM
}
//****************************************************************************80

void dodec_size_3d ( int *point_num, int *edge_num, int *face_num,
  int *face_order_max )

//****************************************************************************80
//
//  Purpose:
//
//    DODEC_SIZE_3D gives "sizes" for a dodecahedron in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 July 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, int *POINT_NUM, the number of points.
//
//    Output, int *EDGE_NUM, the number of edges.
//
//    Output, int *FACE_NUM, the number of faces.
//
//    Output, int *FACE_ORDER_MAX, the maximum order of any face.
//
{
  *point_num = 20;
  *edge_num = 30;
  *face_num = 12;
  *face_order_max = 5;

  return;
}
//****************************************************************************80

void dual_shape_3d ( int point_num, int face_num, int face_order_max,
  double point_coord[], int face_order[], int face_point[], int point_num2,
  int face_num2, int face_order_max2, double point_coord2[], int face_order2[],
  int face_point2[] )

//****************************************************************************80
//
//  Purpose:
//
//    DUAL_SHAPE_3D constructs the dual of a shape in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    26 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int POINT_NUM, the number of points.
//
//    Input, int FACE_NUM, the number of faces.
//
//    Input, int FACE_ORDER_MAX, the maximum number of vertices per face.
//
//    Input, double POINT_COORD[3*POINT_NUM], the point coordinates.
//
//    Input, int FACE_ORDER[FACE_NUM], the number of vertices per face.
//
//    Input, int FACE_POINT[FACE_ORDER_MAX*FACE_NUM]; FACE_POINT(I,J)
//    contains the index of the I-th point in the J-th face.  The
//    points are listed in the counter clockwise direction defined
//    by the outward normal at the face.
//
//    Input, int POINT_NUM2, the number of points in the dual.
//
//    Input, int FACE_NUM2, the number of faces in the dual.
//
//    Input, int FACE_ORDER_MAX2, the maximum number of vertices per face
//    in the dual.
//
//    Output, double POINT_COORD2[3*POINT_NUM2], the point coordinates
//    of the dual.
//
//    Input, int FACE_ORDER2[FACE_NUM2], the number of vertices
//    per face.
//
//    Output, int FACE_POINT2[FACE_ORDER_MAX2*FACE_NUM2], the vertices
//    of each face in the dual.
//
{
  int col;
  int face;
  int i;
  int inext;
  int iprev;
  int istop;
  int j;
  int k;
  double norm;
  int row;
  double x;
  double y;
  double z;
//
//  This computation should really compute the center of gravity
//  of the face, in the general case.
//
//  We'll also assume the vertices of the original and the dual
//  are to lie on the unit sphere, so we can normalize the
//  position vector of the vertex.
//
  for ( face = 0; face < face_num; face++ )
  {
    x = 0.0;
    y = 0.0;
    z = 0.0;
    for ( j = 0; j < face_order[face]; j++ )
    {
      k = face_point[j+face*face_order_max];
      x = x + point_coord[0+(k-1)*3];
      y = y + point_coord[1+(k-1)*3];
      z = z + point_coord[2+(k-1)*3];
    }

    norm = sqrt ( x * x + y * y + z * z );

    point_coord2[0+face*face_order_max2] = x / norm;
    point_coord2[1+face*face_order_max2] = y / norm;
    point_coord2[2+face*face_order_max2] = z / norm;
  }
//
//  Now build the face in the dual associated with each node FACE.
//
  for ( face = 1; face <= face_num2; face++ )
  {
//
//  Initialize the order.
//
    face_order2[face-1] = 0;
//
//  Find the first occurrence of FACE in an edge of polyhedron.
//  ROW and COL are 1-based indices.
//
    i4col_find_item ( face_order_max, face_num, face_point, face,
      &row, &col );

    if ( row <= 0 )
    {
      cerr << "\n";
      cerr << "DUAL_SHAPE_3D - Fatal error!\n";
      cerr << "  Could not find an edge using node " << face << "\n";
      exit ( 1 );
    }
//
//  Save the following node as ISTOP.
//  When we encounter ISTOP again, this will mark the end of our search.
//
    i = row + 1;
    if ( face_order[col-1] < i )
    {
      i = 1;
    }

    istop = face_point[i-1+(col-1)*face_order_max];
//
//  Save the previous node as INEXT.
//
    for ( ; ; )
    {
      i = row - 1;
      if ( i < 1 )
      {
        i = i + face_order[col-1];
      }

      inext = face_point[i-1+(col-1)*face_order_max];

      face_order2[face-1] = face_order2[face-1] + 1;
      face_point2[face_order2[face-1]-1+(face-1)*face_order_max2] = col;
//
//  If INEXT != ISTOP, continue.
//
      if ( inext == istop )
      {
        break;
      }
//
//  Set IPREV:= INEXT.
//
      iprev = inext;
//
//  Search for the occurrence of the edge FACE-IPREV.
//  ROW and COL are 1-based indices.
//
      i4col_find_pair_wrap ( face_order_max, face_num, face_point, face,
        iprev, &row, &col );

      if ( row <= 0 )
      {
        cerr << "\n";
        cerr << "DUAL_SHAPE_3D - Fatal error!\n";
        cerr << "  No edge from node " << iprev << "\n";
        cerr << "  to node " << face << "\n";
        exit ( 1 );
      }
    }
  }

  return;
}
//****************************************************************************80

void dual_size_3d ( int point_num, int edge_num, int face_num,
  int face_order_max, double point_coord[], int face_order[], int face_point[],
  int *point_num2, int *edge_num2, int *face_num2, int *face_order_max2 )

//****************************************************************************80
//
//  Purpose:
//
//    DUAL_SIZE_3D determines sizes for a dual of a shape in 3D.
//
//  Discussion:
//
//    We don't actually need FACE_POINT as input here.  But since the
//    three arrays occur together everywhere else, it seems unnecessarily
//    user-confusing to vary the usage here!
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 July 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int POINT_NUM, the number of points.
//
//    Input, int EDGE_NUM, the number of edges.
//
//    Input, int FACE_NUM, the number of faces.
//
//    Input, int FACE_ORDER_MAX, the maximum number of vertices per face.
//
//    Input, double POINT_COORD[3*POINT_NUM], the point coordinates.
//
//    Input, int FACE_ORDER[FACE_NUM], the number of vertices per face.
//
//    Input, int FACE_POINT[FACE_ORDER_MAX*FACE_NUM]; FACE_POINT(I,J)
//    contains the index of the I-th point in the J-th face.  The
//    points are listed in the counter clockwise direction defined
//    by the outward normal at the face.
//
//    Output, int *POINT_NUM2, the number of points in the dual.
//
//    Output, int *EDGE_NUM2, the number of edges in the dual.
//
//    Output, int *FACE_NUM2, the number of faces in the dual.
//
//    Output, int *FACE_ORDER_MAX2, the maximum number of vertices per face
//    in the dual.
//
{
  int i;
  int face;
  int *face_order2;
  int face2;
//
//  These values are easy to compute:
//
  *point_num2 = face_num;
  *edge_num2 = edge_num;
  *face_num2 = point_num;
//
//  To determine FACE_ORDER_MAX2 is not so easy.
//  You have to construct the FACE_ORDER array for the dual shape.
//  The order of a dual face is the number of edges that the vertex occurs in.
//  But then all we have to do is count how many times each item shows up
//  in the FACE_POINT array.
//
  face_order2 = new int[(*face_num2)];

  for ( i = 0; i < *face_num2; i++ )
  {
    face_order2[i] = 0;
  }

  for ( face = 0; face < face_num; face++ )
  {
    for ( i = 0; i < face_order[face]; i++ )
    {
      face2 = face_point[i+face*face_order_max];
      if ( face2 == 0 ) cout << "WHOA!\n";
      face_order2[face2-1] = face_order2[face2-1] + 1;
    }
  }

  *face_order_max2 = 0;
  for ( i = 0; i < *face_num2; i++ )
  {
    *face_order_max2 = i4_max ( *face_order_max2, face_order2[i] );
  }

  delete [] face_order2;

  return;
}
//****************************************************************************80

double enorm0_nd ( int dim_num, double x[], double y[] )

//****************************************************************************80
//
//  Purpose:
//
//    ENORM0_ND computes the Euclidean norm of a (X-Y) in N space.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    18 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input, double X[DIM_NUM], Y[DIM_NUM], the coordinates of the vectors.
//
//    Output, double ENORM0_ND, the Euclidean norm of the vector.
//
{
  int i;
  double value;

  value = 0.0;

  for ( i = 0; i < dim_num; i++ )
  {
    value = value + ( x[i] - y[i] ) * ( x[i] - y[i] );
  }

  value = sqrt ( value );

  return value;
}
//****************************************************************************80

int get_seed ( )

//****************************************************************************80
//
//  Purpose:
//
//    GET_SEED returns a random seed for the random number generator.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 May 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, int GET_SEED, a random seed value.
//
{
# define I4_MAX 2147483647

  time_t clock;
  int ihour;
  int imin;
  int isec;
  struct tm *lt;
  int seed;
  static int seed_internal = 0;
  time_t tloc;
//
//  If the internal seed is 0, generate a value based on the time.
//
  if ( seed_internal == 0 )
  {

    clock = time ( &tloc );
    lt = localtime ( &clock );
//
//  Hours is 1, 2, ..., 12.
//
    ihour = lt->tm_hour;

    if ( 12 < ihour )
    {
      ihour = ihour - 12;
    }
//
//  Move Hours to 0, 1, ..., 11
//
    ihour = ihour - 1;

    imin = lt->tm_min;

    isec = lt->tm_sec;

    seed_internal = isec + 60 * ( imin + 60 * ihour );
//
//  We want values in [1,43200], not [0,43199].
//
    seed_internal = seed_internal + 1;
//
//  Remap SEED from [1,43200] to [1,IMAX].
//
    seed_internal = ( int ) (
      ( double ) seed_internal * ( double ) I4_MAX
      / ( 60.0 * 60.0 * 12.0 )
    );

  }
//
//  Never use a seed of 0.
//
  if ( seed_internal == 0 )
  {
    seed_internal = 1;
  }

  if ( seed_internal == I4_MAX )
  {
    seed_internal = I4_MAX - 1;
  }

  seed = seed_internal;

  return seed;
# undef I4_MAX
}
//****************************************************************************80

void glob2loc_3d ( double cospitch, double cosroll, double cosyaw,
  double sinpitch, double sinroll, double sinyaw, double globas[3],
  double glopts[3], double locpts[3] )

//****************************************************************************80
//
//  Purpose:
//
//    GLOB2LOC_3D converts from a global to a local coordinate system in 3D.
//
//  Discussion:
//
//    A global coordinate system is given.
//
//    A local coordinate system has been translated to the point with
//    global coordinates GLOBAS, and rotated through a yaw, a pitch, and
//    a roll.
//
//    A point has global coordinates GLOPTS, and it is desired to know
//    the point's local coordinates LOCPTS.
//
//    The transformation may be written as
//
//      LOC = M_ROLL * M_PITCH * M_YAW * ( GLOB - GLOBAS )
//
//    where
//
//               (       1            0            0      )
//    M_ROLL =   (       0        cos(Roll)    sin(Roll)  )
//               (       0      - sin(Roll)    cos(Roll)  )
//
//               (   cos(Pitch)       0      - sin(Pitch) )
//    M_PITCH =  (       0            1            0      )
//               (   sin(Pitch)       0        cos(Pitch) )
//
//               (   cos(Yaw)     sin(Yaw)         0      )
//    M_YAW    = ( - sin(Yaw)     cos(Yaw)         0      )
//               (       0            0            1      )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double COSPITCH, COSROLL, COSYAW, the cosines of the pitch,
//    roll and yaw angles.
//
//    Input, double SINPITCH, SINROLL, SINYAW, the sines of the pitch,
//    roll and yaw angles.
//
//    Input, double GLOBAS[3], the global coordinates of the base vector.
//
//    Input, double GLOPTS[3], the global coordinates of the point.
//
//    Output, double LOCPTS[3], the local coordinates of the point.
//
{
  locpts[0] = ( cosyaw * cospitch ) * ( glopts[0] - globas[0] )
            + ( sinyaw * cospitch ) * ( glopts[1] - globas[1] )
            -   sinpitch * ( glopts[2] - globas[2] );

  locpts[1] = ( cosyaw * sinpitch * sinroll - sinyaw * cosroll )
    * ( glopts[0] - globas[0] )
    + ( sinyaw * sinpitch * sinroll + cosyaw * cosroll )
    * ( glopts[1] - globas[1] )
    +   cospitch * sinroll * ( glopts[2] - globas[2] );

  locpts[2] = ( cosyaw * sinpitch * cosroll + sinyaw * sinroll )
    * ( glopts[0] - globas[0] )
    + ( sinyaw * sinpitch * cosroll - cosyaw * sinroll  )
    * ( glopts[1] - globas[1] )
    + ( cospitch * cosroll ) * ( glopts[2] - globas[2] );

  return;
}
//****************************************************************************80

bool halfplane_contains_point_2d ( double pa[2], double pb[2], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    HALFPLANE_CONTAINS_POINT_2D reports if a half-plane contains a point in 2d.
//
//  Discussion:
//
//    The halfplane is assumed to be all the points "to the left" of the
//    line segment from PA = (XA,YA) to PB = (XB,YB).  Thus, one way to
//    understand where the point P  is, is to compute the signed
//    area of the triangle ( PA, PB, P ).
//
//    If this area is
//      positive, the point is strictly inside the halfplane,
//      zero, the point is on the boundary of the halfplane,
//      negative, the point is strictly outside the halfplane.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PA[2], PB[2], two points on the line defining the half plane.
//
//    Input, double P[2], the point to be checked.
//
//    Output, bool HALFPLANE_CONTAINS_POINT_2D, is TRUE if the halfplane
//    contains the point, and FALSE otherwise.
//
{
  double area_signed;

  area_signed = 0.5 *
    ( pa[0] * ( pb[1] - p[1]  )
    + pb[0] * ( p[1]  - pa[1] )
    + p[0]  * ( pa[1] - pb[1] ) );

  return ( 0.0 <= area_signed );
}
//****************************************************************************80

int halfspace_imp_triangle_int_3d ( double a, double b, double c, double d,
  double t[3*3], double p[3*4] )

//****************************************************************************80
//
//  Purpose:
//
//    HALFSPACE_IMP_TRIANGLE_INT_3D: intersection ( implicit halfspace, triangle ) in 3D.
//
//  Discussion:
//
//    The implicit form of a half-space in 3D may be described as the set
//    of points on or "above" an implicit plane:
//
//      0 <= A * X + B * Y + C * Z + D
//
//    The triangle is specified by listing its three vertices.
//
//    The intersection may be described by the number of vertices of the
//    triangle that are included in the halfspace, and by the location of
//    points between vertices that separate a side of the triangle into
//    an included part and an unincluded part.
//
//    0 vertices, 0 separators    (no intersection)
//    1 vertex,   0 separators    (point intersection)
//    2 vertices, 0 separators    (line intersection)
//    3 vertices, 0 separators    (triangle intersection)
//
//    1 vertex,   2 separators,     (intersection is a triangle)
//    2 vertices, 2 separators,   (intersection is a quadrilateral).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, C, D, the parameters that define the implicit plane,
//    which in turn define the implicit halfspace.
//
//    Input, double T[3*3], the vertices of the triangle.
//
//    Output, double P[3*4], the coordinates of the
//    intersection points.  The points will lie in sequence on the triangle.
//    Some points will be vertices, and some may be separators.
//
//    Output, int HALFSPACE_IMP_TRIANGLE_INT_3D, the number of intersection
//    points returned, which will always be between 0 and 4.
//
{
  double dist1;
  double dist2;
  double dist3;
  int int_num;
//
//  Compute the signed distances between the vertices and the plane.
//
  dist1 = a * t[0+0*3] + b * t[1+0*3] + c * t[2+0*3] + d;
  dist2 = a * t[0+1*3] + b * t[1+1*3] + c * t[2+1*3] + d;
  dist3 = a * t[0+2*3] + b * t[1+2*3] + c * t[2+2*3] + d;
//
//  Now we can find the intersections.
//
  int_num = halfspace_triangle_int_3d ( dist1, dist2, dist3, t, p );

  return int_num;
}
//****************************************************************************80

int halfspace_norm_triangle_int_3d ( double pp[3], double pn[3], double t[3*3],
  double p[3*4] )

//****************************************************************************80
//
//  Purpose:
//
//    HALFSPACE_NORM_TRIANGLE_INT_3D: intersection ( normal halfspace, triangle ) in 3D.
//
//  Discussion:
//
//    The normal form of a halfspace in 3D may be described as the set
//    of points P on or "above" a plane described in normal form:
//
//      PP is a point on the plane,
//      PN is the unit normal vector, pointing "out" of the halfspace
//
//    The triangle is specified by listing its three vertices.
//
//    The intersection may be described by the number of vertices of the
//    triangle that are included in the halfspace, and by the location of
//    points between vertices that separate a side of the triangle into
//    an included part and an unincluded part.
//
//    0 vertices, 0 separators    (no intersection)
//    1 vertex, 0 separators  (point intersection)
//    2 vertices, 0 separators    (line intersection)
//    3 vertices, 0 separators    (triangle intersection)
//
//    1 vertex, 2 separators,     (intersection is a triangle)
//    2 vertices, 2 separators,   (intersection is a quadrilateral).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PP[3], a point on the bounding plane that defines
//    the halfspace.
//
//    Input, double PN[3], the components of the normal vector to the
//    bounding plane that defines the halfspace.  By convention, the
//    normal vector points "outwards" from the halfspace.
//
//    Input, double T[3*3], the vertices of the triangle.
//
//    Output, double P[3*4], the intersection points.  The points will lie
//    in sequence on the triangle.  Some points will be vertices, and some
//    may be separators.
//
//    Output, int HALFSPACE_NORM_TRIANGLE_INT_3D, the number of intersection
//    points returned, which will always be between 0 and 4.
//
{
  double dist1;
  double dist2;
  double dist3;
  int int_num;
//
//  Compute the signed distances between the vertices and the plane.
//
  dist1 = r8vec_dot_product ( 3, pn, t+0*3 );
  dist2 = r8vec_dot_product ( 3, pn, t+1*3 );
  dist3 = r8vec_dot_product ( 3, pn, t+2*3 );
//
//  Now we can find the intersections.
//
  int_num = halfspace_triangle_int_3d ( dist1, dist2, dist3, t, p );

  return int_num;
}
//****************************************************************************80

int halfspace_triangle_int_3d ( double dist1, double dist2, double dist3,
  double t[3*3], double p[3*4] )

//****************************************************************************80
//
//  Purpose:
//
//    HALFSPACE_TRIANGLE_INT_3D: intersection ( halfspace, triangle ) in 3D.
//
//  Discussion:
//
//    The triangle is specified by listing its three vertices.
//
//    The halfspace is not described in the input data.  Rather, the
//    distances from the triangle vertices to the halfspace are given.
//
//    The intersection may be described by the number of vertices of the
//    triangle that are included in the halfspace, and by the location of
//    points between vertices that separate a side of the triangle into
//    an included part and an unincluded part.
//
//    0 vertices, 0 separators    (no intersection)
//    1 vertex, 0 separators  (point intersection)
//    2 vertices, 0 separators    (line intersection)
//    3 vertices, 0 separators    (triangle intersection)
//
//    1 vertex, 2 separators,     (intersection is a triangle)
//    2 vertices, 2 separators,   (intersection is a quadrilateral).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double DIST1, DIST2, DIST3, the distances from each of the
//    three vertices of the triangle to the halfspace.  The distance is
//    zero if a vertex lies within the halfspace, or on the plane that
//    defines the boundary of the halfspace.  Otherwise, it is the
//    distance from that vertex to the bounding plane.
//
//    Input, double T[3*3], the vertices of the triangle.
//
//    Output, double P[3*4], the coordinates of the
//    intersection points.  The points will lie in sequence on the triangle.
//    Some points will be vertices, and some may be separators.
//
//    Output, int HALFSPACE_TRIANGLE_INT_3D, the number of intersection points
//    returned, which will always be between 0 and 4.
//
{
  int int_num;
//
//  Walk around the triangle, looking for vertices that are included,
//  and points of separation.
//
  int_num = 0;

  if ( dist1 <= 0.0 )
  {
    p[0+int_num*3] = t[0+0*3];
    p[1+int_num*3] = t[1+0*3];
    p[2+int_num*3] = t[2+0*3];
    int_num = int_num + 1;
  }

  if ( dist1 * dist2 < 0.0 )
  {
    p[0+int_num*3] = ( dist1 * t[0+1*3] - dist2 * t[0+0*3] ) / ( dist1 - dist2 );
    p[1+int_num*3] = ( dist1 * t[1+1*3] - dist2 * t[1+0*3] ) / ( dist1 - dist2 );
    p[2+int_num*3] = ( dist1 * t[2+1*3] - dist2 * t[2+0*3] ) / ( dist1 - dist2 );
    int_num = int_num + 1;
  }

  if ( dist2 <= 0.0 )
  {
    p[0+int_num*3] = t[0+1*3];
    p[1+int_num*3] = t[1+1*3];
    p[2+int_num*3] = t[2+1*3];
    int_num = int_num + 1;
  }

  if ( dist2 * dist3 < 0.0 )
  {
    p[0+int_num*3] = ( dist2 * t[0+2*3] - dist3 * t[0+1*3] ) / ( dist2 - dist3 );
    p[1+int_num*3] = ( dist2 * t[1+2*3] - dist3 * t[1+1*3] ) / ( dist2 - dist3 );
    p[2+int_num*3] = ( dist2 * t[2+2*3] - dist3 * t[2+0*3] ) / ( dist2 - dist3 );
    int_num = int_num + 1;
  }

  if ( dist3 <= 0.0 )
  {
    p[0+int_num*3] = t[0+2*3];
    p[1+int_num*3] = t[1+2*3];
    p[2+int_num*3] = t[2+2*3];
    int_num = int_num + 1;
  }

  if ( dist3 * dist1 < 0.0 )
  {
    p[0+int_num*3] = ( dist3 * t[0+0*3] - dist1 * t[0+2*3] ) / ( dist3 - dist1 );
    p[1+int_num*3] = ( dist3 * t[1+0*3] - dist1 * t[1+2*3] ) / ( dist3 - dist1 );
    p[2+int_num*3] = ( dist3 * t[2+0*3] - dist1 * t[2+2*3] ) / ( dist3 - dist1 );
    int_num = int_num + 1;
  }

  return int_num;
}
//****************************************************************************80

double haversine ( double a )

//****************************************************************************80
//
//  Purpose:
//
//    HAVERSINE computes the haversine of an angle.
//
//  Discussion:
//
//    haversine(A) = ( 1 - cos ( A ) ) / 2
//
//    The haversine is useful in spherical trigonometry.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, the angle.
//
//    Output, double HAVERSINE, the haversine of the angle.
//
{
  return ( ( 1.0 - cos ( a ) ) / 2.0 );
}
//****************************************************************************80

void helix_shape_3d ( double a, int n, double r, double theta1, double theta2,
  double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    HELIX_SHAPE_3D computes points on a helix in 3D.
//
//  Discussion:
//
//    The user specifies the parameters A and R, the first and last
//    THETA values, and the number of equally spaced THETA values
//    at which values are to be computed.
//
//      ( R * COS ( THETA ), R * SIN ( THETA ), A * THETA )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, the rate at which Z advances with THETA.
//
//    Input, int N, the number of points to compute on the helix.
//
//    Input, double R, the radius of the helix.
//
//    Input, double THETA1, THETA2, the first and last THETA values at
//    which to compute points on the helix.  THETA is measured in
//    radians.
//
//    Output, double P[3*N], the coordinates of points on the helix.
//
{
  int i;
  double theta;

  for ( i = 0; i < n; i++ )
  {
    if ( n == 1 )
    {
      theta = 0.5 * ( theta1 + theta2 );
    }
    else
    {
      theta = ( ( ( double ) ( n - i     ) ) * theta1
              + ( ( double ) (     i - 1 ) ) * theta2 )
              / ( ( double ) ( n     - 1 ) );
    }

    p[0+i*3] = r * cos ( theta );
    p[1+i*3] = r * sin ( theta );
    p[2+i*3] = a * theta;
  }

  return;
}
//****************************************************************************80

double hexagon_area_2d ( double r )

//****************************************************************************80
//
//  Purpose:
//
//    HEXAGON_AREA_2D returns the area of a regular hexagon in 2D.
//
//  Discussion:
//
//    The radius of a regular hexagon is the distance from the center
//    of the hexagon to any vertex.  This happens also to equal the
//    length of any side.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the hexagon.
//
//    Output, double HEXAGON_AREA_2D, the area of the hexagon.
//
{
  double value;

  value = r * r * hexagon_unit_area_2d ( );

  return value;
}
//****************************************************************************80

bool hexagon_contains_point_2d ( double v[2*6], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    HEXAGON_CONTAINS_POINT_2D finds if a point is inside a hexagon in 2D.
//
//  Discussion:
//
//    This test is only valid if the hexagon is convex.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    20 June 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V[2*6], the vertics, in counter clockwise order.
//
//    Input, double P[2], the point to be tested.
//
//    Output, bool HEXAGON_CONTAINS_POINT_2D, is TRUE if X is in the hexagon.
//
{
# define N 6

  int i;
  int j;
//
//  A point is inside a convex hexagon if and only if it is "inside"
//  each of the 6 halfplanes defined by lines through consecutive
//  vertices.
//
  for ( i = 0; i < N; i++ )
  {
    j = ( i + 1 ) % N;

    if (  v[0+i*2] * ( v[1+j*2] - p[1    ] )
        + v[0+j*2] * ( p[1    ] - v[1+i*2] )
        + p[0    ] * ( v[1+i*2] - v[1+j*2] ) < 0.0 )
    {
      return false;
    }
  }

  return true;
# undef N
}
//****************************************************************************80

void hexagon_shape_2d ( double angle, double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    HEXAGON_SHAPE_2D returns points on the unit regular hexagon in 2D.
//
//  Discussion:
//
//    The unit regular hexagon has radius 1. The radius is the distance from
//    the center to any vertex, and it is also the length of any side.
//    An example of a unit hexagon is the convex hull of the points
//
//      (   1,              0 ),
//      (   0.5,   sqrt (3)/2 ),
//      ( - 0.5,   sqrt (3)/2 ),
//      ( - 1,              0 ),
//      ( - 0.5, - sqrt (3)/2 ),
//      (   0.5, - sqrt (3)/2 ).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double ANGLE, the angle, in degrees, of the point.
//
//    Output, double P[2], the coordinates of the point.
//
{
//
//  Ensure that 0.0 <= ANGLE2 < 360.
//
  angle = r8_modp ( angle, 360.0 );
//
//  y = - sqrt(3) * x + sqrt(3)
//
  if ( 0.0 <= angle && angle <= 60.0 )
  {
    p[0] = sqrt ( 3.0 ) / ( r8_tand ( angle ) + sqrt ( 3.0 ) );
    p[1] = r8_tand ( angle ) * p[0];
  }
//
//  y = sqrt(3) / 2
//
  else if ( angle <= 120.0 )
  {
    p[1] = sqrt ( 3.0 ) / 2.0;
    p[0] = r8_cotd ( angle ) * p[1];
  }
//
//  y = sqrt(3) * x + sqrt(3)
//
  else if ( angle <= 180.0 )
  {
    p[0] = sqrt ( 3.0 ) / ( r8_tand ( angle ) - sqrt ( 3.0 ) );
    p[1] = r8_tand ( angle ) * p[0];
  }
//
//  y = - sqrt(3) * x - sqrt(3)
//
  else if ( angle <= 240.0 )
  {
    p[0] = - sqrt ( 3.0 ) / ( r8_tand ( angle ) + sqrt ( 3.0 ) );
    p[1] = r8_tand ( angle ) * p[0];
  }
//
//  y = - sqrt(3) / 2
//
  else if ( angle <= 300.0 )
  {
    p[1] = - sqrt ( 3.0 ) / 2.0;
    p[0] = r8_cotd ( angle ) * p[1];
  }
//
//  y = sqrt(3) * x - sqrt(3)
//
  else if ( angle <= 360.0 )
  {
    p[0] = - sqrt ( 3.0 ) / ( r8_tand ( angle ) - sqrt ( 3.0 ) );
    p[1] = r8_tand ( angle ) * p[0];
  }

  return;
}
//****************************************************************************80

double hexagon_unit_area_2d ( )

//****************************************************************************80
//
//  Purpose:
//
//    HEXAGON_UNIT_AREA_2D returns the area of a unit regular hexagon in 2D.
//
//  Discussion:
//
//    A "unit" regular hexagon has both a "radius" of 1 (distance
//    from the center to any vertex), and a side length of 1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, double HEXAGON_UNIT_AREA_2D, the area of the hexagon.
//
{
  double value;

  value = 3.0 * sqrt ( 3.0 ) / 2.0;

  return value;
}
//****************************************************************************80

void hexagon_vertices_2d ( double h[2*6] )

//****************************************************************************80
//
//  Purpose:
//
//    HEXAGON_VERTICES_2D returns the vertices of the unit hexagon in 2D.
//
//  Discussion:
//
//    The unit hexagon has maximum radius 1, and is the hull of the points
//
//      (   1,              0 ),
//      (   0.5,   sqrt (3)/2 ),
//      ( - 0.5,   sqrt (3)/2 ),
//      ( - 1,              0 ),
//      ( - 0.5, - sqrt (3)/2 ),
//      (   0.5, - sqrt (3)/2 ).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, double H[2*6], the coordinates of the vertices.
//
{
# define A 0.8660254037844386
# define DIM_NUM 2

  h[0+0*2] =  1.0;
  h[0+1*2] =  0.5;
  h[0+2*2] = -0.5;
  h[0+3*2] = -1.0;
  h[0+4*2] = -0.5;
  h[0+5*2] =  0.5;

  h[1+0*2] =  0.0;
  h[1+1*2] =  A;
  h[1+2*2] =  A;
  h[1+3*2] =  0.0;
  h[1+4*2] = -A;
  h[1+5*2] = -A;

  return;
# undef A
# undef DIM_NUM
}
//****************************************************************************80

double i4_dedekind_factor ( int p, int q )

//****************************************************************************80
//
//  Purpose:
//
//    I4_DEDEKIND_FACTOR computes a function needed for a Dedekind sum.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Hans Rademacher, Emil Grosswald,
//    Dedekind Sums,
//    Mathematics Association of America, 1972,
//    LC: QA241.R2.
//
//  Parameters:
//
//    Input, int P, Q, two positive integers.
//
//    Input, double I4_DEDEKIND_FACTOR, the Dedekind factor of P / Q.
//
{
  double s;

  if ( ( p % q ) == 0 )
  {
    s = 0.0;
  }
  else
  {
    s = ( double ) ( p ) / ( double ) ( q )  - ( ( double ) ( p / q ) ) - 0.5;
  }

  return s;
}
//****************************************************************************80

double i4_dedekind_sum ( int p, int q )

//****************************************************************************80
//
//  Purpose:
//
//    I4_DEDEKIND_SUM computes the Dedekind sum of two I4's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Hans Rademacher, Emil Grosswald,
//    Dedekind Sums,
//    Mathematics Association of America, 1972,
//    LC: QA241.R2.
//
//  Parameters:
//
//    Input, int P, Q, two positive integers.
//
//    Output, double I4_DEDEKIND_SUM, the Dedekind sum of P and Q.
//
{
  int i;
  double s;

  s = 0.0;

  for ( i = 1; i <= q; i++ )
  {
    s = s + i4_dedekind_factor ( i, q ) * i4_dedekind_factor ( p * i, q );
  }

  return s;
}
//****************************************************************************80

int i4_factorial2 ( int n )

//****************************************************************************80
//
//  Purpose:
//
//    I4_FACTORIAL2 computes the double factorial function N!!
//
//  Discussion:
//
//    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
//                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
//
//  Example:
//
//     N    N!!
//
//     0     1
//     1     1
//     2     2
//     3     3
//     4     8
//     5    15
//     6    48
//     7   105
//     8   384
//     9   945
//    10  3840
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 May 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the argument of the double factorial function.
//    If N is less than 1, I4_FACTORIAL2 is returned as 1.
//
//    Output, int I4_FACTORIAL2, the value of N!!.
//
{
  int value;

  if ( n < 1 )
  {
    return 1;
  }

  value = 1;

  while ( 1 < n )
  {
    value = value * n;
    n = n - 2;
  }

  return value;
}
//****************************************************************************80

int i4_gcd ( int i, int j )

//****************************************************************************80
//
//  Purpose:
//
//    I4_GCD finds the greatest common divisor of two I4's.
//
//  Discussion:
//
//    Note that only the absolute values of I and J are
//    considered, so that the result is always nonnegative.
//
//    If I or J is 0, I4_GCD is returned as max ( 1, abs ( I ), abs ( J ) ).
//
//    If I and J have no common factor, I4_GCD is returned as 1.
//
//    Otherwise, using the Euclidean algorithm, I4_GCD is the
//    greatest common divisor of I and J.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    25 March 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int I, J, two numbers whose GCD is desired.
//
//    Output, int I4_GCD, the greatest common divisor of I and J.
//
{
  int p;
  int q;
  int r;
//
//  Return immediately if either I or J is zero.
//
  if ( i == 0 )
  {
    q = i4_max ( 1, abs ( j ) );
    return q;
  }
  else if ( j == 0 )
  {
    q = i4_max ( 1, abs ( i ) );
    return q;
  }
//
//  Set IP to the larger of I and J, IQ to the smaller.
//  This way, we can alter IP and IQ as we go.
//
  p = i4_max ( abs ( i ), abs ( j ) );
  q = i4_min ( abs ( i ), abs ( j ) );
//
//  Carry out the Euclidean algorithm.
//
  for ( ; ; )
  {
    r = p % q;

    if ( r == 0 )
    {
      break;
    }
    p = q;
    q = r;
  }

  return q;
}
//****************************************************************************80

int i4_lcm ( int i, int j )

//****************************************************************************80
//
//  Purpose:
//
//    I4_LCM computes the least common multiple of two I4's.
//
//  Discussion:
//
//    The least common multiple may be defined as
//
//      LCM(I,J) = ABS( I * J ) / GCF(I,J)
//
//    where GCF(I,J) is the greatest common factor of I and J.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    26 May 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int I, J, the integers whose LCM is desired.
//
//    Output, int I4_LCM, the least common multiple of I and J.
//    I4_LCM is never negative.  I4_LCM is 0 if either I or J is zero.
//
{
  int value;

  value = abs ( i * ( j / i4_gcd ( i, j ) ) );

  return value;
}
//****************************************************************************80

int i4_max ( int i1, int i2 )

//****************************************************************************80
//
//  Purpose:
//
//    I4_MAX returns the maximum of two I4's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 October 1998
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int I1, I2, are two integers to be compared.
//
//    Output, int I4_MAX, the larger of I1 and I2.
//
{
  int value;

  if ( i2 < i1 )
  {
    value = i1;
  }
  else
  {
    value = i2;
  }
  return value;
}
//****************************************************************************80

int i4_min ( int i1, int i2 )

//****************************************************************************80
//
//  Purpose:
//
//    I4_MIN returns the smaller of two I4's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 October 1998
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int I1, I2, two integers to be compared.
//
//    Output, int I4_MIN, the smaller of I1 and I2.
//
{
  int value;

  if ( i1 < i2 )
  {
    value = i1;
  }
  else
  {
    value = i2;
  }
  return value;
}
//****************************************************************************80

int i4_modp ( int i, int j )

//****************************************************************************80
//
//  Purpose:
//
//    I4_MODP returns the nonnegative remainder of I4 division.
//
//  Discussion:
//
//    If
//      NREM = I4_MODP ( I, J )
//      NMULT = ( I - NREM ) / J
//    then
//      I = J * NMULT + NREM
//    where NREM is always nonnegative.
//
//    The MOD function computes a result with the same sign as the
//    quantity being divided.  Thus, suppose you had an angle A,
//    and you wanted to ensure that it was between 0 and 360.
//    Then mod(A,360) would do, if A was positive, but if A
//    was negative, your result would be between -360 and 0.
//
//    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
//
//  Example:
//
//        I         J     MOD  I4_MODP   I4_MODP Factorization
//
//      107        50       7       7    107 =  2 *  50 + 7
//      107       -50       7       7    107 = -2 * -50 + 7
//     -107        50      -7      43   -107 = -3 *  50 + 43
//     -107       -50      -7      43   -107 =  3 * -50 + 43
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    26 May 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int I, the number to be divided.
//
//    Input, int J, the number that divides I.
//
//    Output, int I4_MODP, the nonnegative remainder when I is
//    divided by J.
//
{
  int value;

  if ( j == 0 )
  {
    cerr << "\n";
    cerr << "I4_MODP - Fatal error!\n";
    cerr << "  I4_MODP ( I, J ) called with J = " << j << "\n";
    exit ( 1 );
  }

  value = i % j;

  if ( value < 0 )
  {
    value = value + abs ( j );
  }

  return value;
}
//****************************************************************************80

int i4_sign ( int i )

//****************************************************************************80
//
//  Purpose:
//
//    I4_SIGN returns the sign of an I4.
//
//  Discussion:
//
//    The sign of 0 and all positive integers is taken to be +1.
//    The sign of all negative integers is -1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 May 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int I, the integer whose sign is desired.
//
//    Output, int I4_SIGN, the sign of I.
{
  int value;

  if ( i < 0 )
  {
    value = -1;
  }
  else
  {
    value = 1;
  }
  return value;
}
//****************************************************************************80

void i4_swap ( int *i, int *j )

//****************************************************************************80
//
//  Purpose:
//
//    I4_SWAP switches two I4's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 January 2002
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int *I, *J.  On output, the values of I and
//    J have been interchanged.
//
{
  int k;

  k = *i;
  *i = *j;
  *j = k;

  return;
}
//****************************************************************************80

int i4_uniform_ab ( int a, int b, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    I4_UNIFORM_AB returns a scaled pseudorandom I4 between A and B.
//
//  Discussion:
//
//    The pseudorandom number should be uniformly distributed
//    between A and B.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    02 October 2012
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paul Bratley, Bennett Fox, Linus Schrage,
//    A Guide to Simulation,
//    Second Edition,
//    Springer, 1987,
//    ISBN: 0387964673,
//    LC: QA76.9.C65.B73.
//
//    Bennett Fox,
//    Algorithm 647:
//    Implementation and Relative Efficiency of Quasirandom
//    Sequence Generators,
//    ACM Transactions on Mathematical Software,
//    Volume 12, Number 4, December 1986, pages 362-376.
//
//    Pierre L'Ecuyer,
//    Random Number Generation,
//    in Handbook of Simulation,
//    edited by Jerry Banks,
//    Wiley, 1998,
//    ISBN: 0471134031,
//    LC: T57.62.H37.
//
//    Peter Lewis, Allen Goodman, James Miller,
//    A Pseudo-Random Number Generator for the System/360,
//    IBM Systems Journal,
//    Volume 8, Number 2, 1969, pages 136-143.
//
//  Parameters:
//
//    Input, int A, B, the limits of the interval.
//
//    Input/output, int &SEED, the "seed" value, which should NOT be 0.
//    On output, SEED has been updated.
//
//    Output, int I4_UNIFORM, a number between A and B.
//
{
  int c;
  const int i4_huge = 2147483647;
  int k;
  float r;
  int value;

  if ( seed == 0 )
  {
    cerr << "\n";
    cerr << "I4_UNIFORM_AB - Fatal error!\n";
    cerr << "  Input value of SEED = 0.\n";
    exit ( 1 );
  }
//
//  Guarantee A <= B.
//
  if ( b < a )
  {
    c = a;
    a = b;
    b = c;
  }

  k = seed / 127773;

  seed = 16807 * ( seed - k * 127773 ) - k * 2836;

  if ( seed < 0 )
  {
    seed = seed + i4_huge;
  }

  r = ( float ) ( seed ) * 4.656612875E-10;
//
//  Scale R to lie between A-0.5 and B+0.5.
//
  r = ( 1.0 - r ) * ( ( float ) a - 0.5 ) 
    +         r   * ( ( float ) b + 0.5 );
//
//  Use rounding to convert R to an integer between A and B.
//
  value = round ( r );
//
//  Guarantee A <= VALUE <= B.
//
  if ( value < a )
  {
    value = a;
  }
  if ( b < value )
  {
    value = b;
  }

  return value;
}
//****************************************************************************80

int i4_wrap ( int ival, int ilo, int ihi )

//****************************************************************************80
//
//  Purpose:
//
//    I4_WRAP forces an I4 to lie between given limits by wrapping.
//
//  Example:
//
//    ILO = 4, IHI = 8
//
//    I  I4_WRAP
//
//    -2     8
//    -1     4
//     0     5
//     1     6
//     2     7
//     3     8
//     4     4
//     5     5
//     6     6
//     7     7
//     8     8
//     9     4
//    10     5
//    11     6
//    12     7
//    13     8
//    14     4
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int IVAL, an integer value.
//
//    Input, int ILO, IHI, the desired bounds for the integer value.
//
//    Output, int I4_WRAP, a "wrapped" version of IVAL.
//
{
  int jhi;
  int jlo;
  int value;
  int wide;

  jlo = i4_min ( ilo, ihi );
  jhi = i4_max ( ilo, ihi );

  wide = jhi + 1 - jlo;

  if ( wide == 1 )
  {
    value = jlo;
  }
  else
  {
    value = jlo + i4_modp ( ival - jlo, wide );
  }

  return value;
}
//****************************************************************************80

int i4col_compare ( int m, int n, int a[], int i, int j )

//****************************************************************************80
//
//  Purpose:
//
//    I4COL_COMPARE compares columns I and J of an I4COL
//
//  Example:
//
//    Input:
//
//      M = 3, N = 4, I = 2, J = 4
//
//      A = (
//        1  2  3  4
//        5  6  7  8
//        9 10 11 12 )
//
//    Output:
//
//      I4COL_COMPARE = -1
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    25 March 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns.
//
//    Input, int A[M*N], an array of N columns of vectors of length M.
//
//    Input, int I, J, the columns to be compared.
//    I and J must be between 1 and N.
//
//    Output, int I4COL_COMPARE, the results of the comparison:
//    -1, column I < column J,
//     0, column I = column J,
//    +1, column J < column I.
//
{
  int k;
//
//  Check.
//
  if ( i < 1 || n < i )
  {
    cerr << "\n";
    cerr << "I4COL_COMPARE - Fatal error!\n";
    cerr << "  Column index I is out of bounds.\n";
    exit ( 1 );
  }

  if ( j < 1 || n < j )
  {
    cerr << "\n";
    cerr << "I4COL_COMPARE - Fatal error!\n";
    cerr << "  Column index J is out of bounds.\n";
    exit ( 1 );
  }

  if ( i == j )
  {
    return 0;
  }

  k = 1;

  while ( k <= m )
  {
    if ( a[k-1+(i-1)*m] < a[k-1+(j-1)*m] )
    {
      return (-1);
    }
    else if ( a[k-1+(j-1)*m] < a[k-1+(i-1)*m] )
    {
      return 1;
    }
    k = k + 1;
  }

  return 0;
}
//****************************************************************************80

void i4col_find_item ( int m, int n, int table[], int item,
  int *row, int *col )

//****************************************************************************80
//
//  Purpose:
//
//    I4COL_FIND_ITEM searches an I4COL for a given value.
//
//  Discussion:
//
//    The two dimensional information in TABLE is stored as a one dimensional
//    array, by columns.
//
//    The values ROW and COL will be one-based indices.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns
//    in the table.
//
//    Input, int TABLE[M*N], the table to search.
//
//    Input, int ITEM, the value to search for.
//
//    Output, int *ROW, *COL, the row and column indices
//    of the first occurrence of the value ITEM.  The search
//    is conducted by rows.  If the item is not found, then
//    ROW = COL = -1.
//
{
  int i;
  int j;

  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < m; i++ )
    {
      if ( table[i+j*m] == item )
      {
        *row = i+1;
        *col = j+1;
        return;
      }
    }
  }

  *row = -1;
  *col = -1;

  return;
}
//****************************************************************************80

void i4col_find_pair_wrap ( int m, int n, int a[], int item1, int item2,
  int *row, int *col )

//****************************************************************************80
//
//  Purpose:
//
//    I4COL_FIND_PAIR_WRAP wrap searches an I4COL for a pair of items.
//
//  Discussion:
//
//    The items (ITEM1, ITEM2) must occur consecutively.
//    However, wrapping is allowed, that is, if ITEM1 occurs
//    in the last row, and ITEM2 "follows" it in the first row
//    of the same column, a match is declared.
//
//    If the pair of items is not found, then ROW = COL = -1.
//
//    The values ROW and COL will be one-based indices.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns
//    in the table.
//
//    Input, int A[M*N], the table to search.
//
//    Input, int ITEM1, ITEM2, the values to search for.
//
//    Output, int *ROW, *COL, the row and column indices
//    of the first occurrence of the value ITEM1 followed immediately
//    by ITEM2.
//
{
  int i;
  int i2;
  int j;

  for ( j = 1; j <= n; j++ )
  {
    for ( i = 1; i <= m; i++ )
    {
      if ( a[i-1+(j-1)*m] == item1 )
      {
        i2 = i + 1;

        if ( m < i2 )
        {
          i2 = 1;
        }

        if ( a[i2-1+(j-1)*m] == item2 )
        {
          *row = i;
          *col = j;
          return;
        }
      }
    }
  }

  *row = -1;
  *col = -1;

  return;
}
//****************************************************************************80

void i4col_sort_a ( int m, int n, int a[] )

//****************************************************************************80
//
//  Purpose:
//
//    I4COL_SORT_A ascending sorts the columns of an integer array.
//
//  Discussion:
//
//    In lexicographic order, the statement "X < Y", applied to two
//    vectors X and Y of length M, means that there is some index I, with
//    1 <= I <= M, with the property that
//
//      X(J) = Y(J) for J < I,
//    and
//      X(I) < Y(I).
//
//    In other words, X is less than Y if, at the first index where they
//    differ, the X value is less than the Y value.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 April 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the number of rows of A.
//
//    Input, int N, the number of columns of A.
//
//    Input/output, int A[M*N].
//    On input, the array of N columns of M vectors;
//    On output, the columns of A have been sorted in ascending
//    lexicographic order.
//
{
  int i;
  int indx;
  int isgn;
  int j;
//
//  Initialize.
//
  i = 0;
  indx = 0;
  isgn = 0;
  j = 0;
//
//  Call the external heap sorter.
//
  for ( ; ; )
  {
    sort_heap_external ( m, &indx, &i, &j, isgn );
//
//  Interchange the I and J objects.
//
    if ( 0 < indx )
    {
      i4col_swap ( m, n, a, i, j );
    }
//
//  Compare the I and J objects.
//
    else if ( indx < 0 )
    {
      isgn = i4col_compare ( m, n, a, i, j );
    }
    else if ( indx == 0 )
    {
      break;
    }

  }

  return;
}
//****************************************************************************80

int i4col_sorted_unique_count ( int m, int n, int a[] )

//****************************************************************************80
//
//  Purpose:
//
//    I4COL_SORTED_UNIQUE_COUNT counts unique elements in an ICOL array.
//
//  Discussion:
//
//    The columns of the array may be ascending or descending sorted.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 February 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns.
//
//    Input, int A[M*N], a sorted array, containing
//    N columns of data.
//
//    Output, int I4COL_SORTED_UNIQUE_COUNT, the number of unique columns.
//
{
  int i;
  int j1;
  int j2;
  int unique_num;

  if ( n <= 0 )
  {
    unique_num = 0;
    return unique_num;
  }

  unique_num = 1;
  j1 = 0;

  for ( j2 = 1; j2 < n; j2++ )
  {
    for ( i = 0; i < m; i++ )
    {
      if ( a[i+j1*m] != a[i+j2*m] )
      {
        unique_num = unique_num + 1;
        j1 = j2;
        break;
      }
    }
  }

  return unique_num;
}
//****************************************************************************80

void i4col_swap ( int m, int n, int a[], int icol1, int icol2 )

//****************************************************************************80
//
//  Purpose:
//
//    I4COL_SWAP swaps two columns of an integer array.
//
//  Discussion:
//
//    The two dimensional information is stored as a one dimensional
//    array, by columns.
//
//    The row indices are 1 based, NOT 0 based!  However, a preprocessor
//    variable, called OFFSET, can be reset from 1 to 0 if you wish to
//    use 0-based indices.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 April 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns.
//
//    Input/output, int A[M*N], an array of data.
//
//    Input, int ICOL1, ICOL2, the two columns to swap.
//    These indices should be between 1 and N.
//
{
# define OFFSET 1

  int i;
  int t;
//
//  Check.
//
  if ( icol1 - OFFSET < 0 || n-1 < icol1 - OFFSET )
  {
    cerr << "\n";
    cerr << "I4COL_SWAP - Fatal error!\n";
    cerr << "  ICOL1 is out of range.\n";
    exit ( 1 );
  }

  if ( icol2 - OFFSET < 0 || n-1 < icol2 - OFFSET )
  {
    cerr << "\n";
    cerr << "I4COL_SWAP - Fatal error!\n";
    cerr << "  ICOL2 is out of range.\n";
    exit ( 1 );
  }

  if ( icol1 == icol2 )
  {
    return;
  }
  for ( i = 0; i < m; i++ )
  {
    t                     = a[i+(icol1-OFFSET)*m];
    a[i+(icol1-OFFSET)*m] = a[i+(icol2-OFFSET)*m];
    a[i+(icol2-OFFSET)*m] = t;
  }

  return;
# undef OFFSET
}
//****************************************************************************80

void i4mat_print ( int m, int n, int a[], string title )

//****************************************************************************80
//
//  Purpose:
//
//    I4MAT_PRINT prints an I4MAT, with an optional title.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 April 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the number of rows in A.
//
//    Input, int N, the number of columns in A.
//
//    Input, int A[M*N], the M by N matrix.
//
//    Input, string TITLE, a title.
//
{
  i4mat_print_some ( m, n, a, 1, 1, m, n, title );

  return;
}
//****************************************************************************80

void i4mat_print_some ( int m, int n, int a[], int ilo, int jlo, int ihi,
  int jhi, string title )

//****************************************************************************80
//
//  Purpose:
//
//    I4MAT_PRINT_SOME prints some of an I4MAT.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 April 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the number of rows of the matrix.
//    M must be positive.
//
//    Input, int N, the number of columns of the matrix.
//    N must be positive.
//
//    Input, int A[M*N], the matrix.
//
//    Input, int ILO, JLO, IHI, JHI, designate the first row and
//    column, and the last row and column to be printed.
//
//    Input, string TITLE, a title.
{
# define INCX 10

  int i;
  int i2hi;
  int i2lo;
  int j;
  int j2hi;
  int j2lo;

  cout << "\n";
  cout << title << "\n";
//
//  Print the columns of the matrix, in strips of INCX.
//
  for ( j2lo = jlo; j2lo <= jhi; j2lo = j2lo + INCX )
  {
    j2hi = j2lo + INCX - 1;
    j2hi = i4_min ( j2hi, n );
    j2hi = i4_min ( j2hi, jhi );

    cout << "\n";
//
//  For each column J in the current range...
//
//  Write the header.
//
    cout << "  Col:    ";
    for ( j = j2lo; j <= j2hi; j++ )
    {
      cout << setw(7) << j << "       ";
    }
    cout << "\n";
    cout << "  Row\n";
    cout << "\n";
//
//  Determine the range of the rows in this strip.
//
    i2lo = i4_max ( ilo, 1 );
    i2hi = i4_min ( ihi, m );

    for ( i = i2lo; i <= i2hi; i++ )
    {
//
//  Print out (up to INCX) entries in row I, that lie in the current strip.
//
      cout << setw(5) << i << ": ";
      for ( j = j2lo; j <= j2hi; j++ )
      {
        cout << setw(6) << a[i-1+(j-1)*m] << "  ";
      }
      cout << "\n";
    }
  }

  return;
# undef INCX
}
//****************************************************************************80

void i4mat_transpose_print ( int m, int n, int a[], string title )

//****************************************************************************80
//
//  Purpose:
//
//    I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    31 January 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the number of rows in A.
//
//    Input, int N, the number of columns in A.
//
//    Input, int A[M*N], the M by N matrix.
//
//    Input, string TITLE, a title.
//
{
  i4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title );

  return;
}
//****************************************************************************80

void i4mat_transpose_print_some ( int m, int n, int a[], int ilo, int jlo,
  int ihi, int jhi, string title )

//****************************************************************************80
//
//  Purpose:
//
//    I4MAT_TRANSPOSE_PRINT_SOME prints some of an I4MAT, transposed.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 February 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the number of rows of the matrix.
//    M must be positive.
//
//    Input, int N, the number of columns of the matrix.
//    N must be positive.
//
//    Input, int A[M*N], the matrix.
//
//    Input, int ILO, JLO, IHI, JHI, designate the first row and
//    column, and the last row and column to be printed.
//
//    Input, string TITLE, a title.
{
# define INCX 10

  int i;
  int i2hi;
  int i2lo;
  int j;
  int j2hi;
  int j2lo;

  cout << "\n";
  cout << title << "\n";
//
//  Print the columns of the matrix, in strips of INCX.
//
  for ( i2lo = ilo; i2lo <= ihi; i2lo = i2lo + INCX )
  {
    i2hi = i2lo + INCX - 1;
    i2hi = i4_min ( i2hi, m );
    i2hi = i4_min ( i2hi, ihi );

    cout << "\n";
//
//  For each row I in the current range...
//
//  Write the header.
//
    cout << "  Row:    ";
    for ( i = i2lo; i <= i2hi; i++ )
    {
      cout << setw(7) << i << "       ";
    }
    cout << "\n";
    cout << "  Col\n";
    cout << "\n";
//
//  Determine the range of the rows in this strip.
//
    j2lo = i4_max ( jlo, 1 );
    j2hi = i4_min ( jhi, n );

    for ( j = j2lo; j <= j2hi; j++ )
    {
//
//  Print out (up to INCX) entries in column J, that lie in the current strip.
//
      cout << setw(5) << j << ": ";
      for ( i = i2lo; i <= i2hi; i++ )
      {
        cout << setw(6) << a[i-1+(j-1)*m] << "  ";
      }
      cout << "\n";
    }
  }

  return;
# undef INCX
}
//****************************************************************************80

int i4row_compare ( int m, int n, int a[], int i, int j )

//****************************************************************************80
//
//  Purpose:
//
//    I4ROW_COMPARE compares two rows of an I4ROW.
//
//  Discussion:
//
//    The two dimensional information is stored in a one dimensional array,
//    by ROWS.  The entry A(I,J) is stored in A[I*N+J].
//
//    The input arguments I and J are row indices.  They DO NOT use the
//    C convention of starting at 0, but rather start at 1.
//
//  Example:
//
//    Input:
//
//      M = 3, N = 4, I = 2, J = 3
//
//      A = (
//        1  2  3  4
//        5  6  7  8
//        9 10 11 12 )
//
//    Output:
//
//      I4ROW_COMPARE = -1
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns.
//
//    Input, int A[M*N], an array of M rows of vectors of length N.
//
//    Input, int I, J, the rows to be compared.
//    I and J must be between 1 and M.
//
//    Output, int I4ROW_COMPARE, the results of the comparison:
//    -1, row I < row J,
//     0, row I = row J,
//    +1, row J < row I.
//
{
  int k;
//
//  Check that I and J are legal.
//
  if ( i < 1 )
  {
    cerr << "\n";
    cerr << "I4ROW_COMPARE - Fatal error!\n";
    cerr << "  Row index I is less than 1.\n";
    cerr << "  I = " << i << "\n";
    exit ( 1 );
  }
  else if ( m < i )
  {
    cerr << "\n";
    cerr << "I4ROW_COMPARE - Fatal error!\n";
    cerr << "  Row index I is out of bounds.\n";
    cerr << "  I = " << i << "\n";
    cerr << "  Maximum legal value is M = " << m << "\n";
    exit ( 1 );
  }

  if ( j < 1 )
  {
    cerr << "\n";
    cerr << "I4ROW_COMPARE - Fatal error!\n";
    cerr << "  Row index J is less than 1.\n";
    cerr << "  J = " << j << "\n";
    exit ( 1 );
  }
  else if ( m < j )
  {
    cerr << "\n";
    cerr << "I4ROW_COMPARE - Fatal error!\n";
    cerr << "  Row index J is out of bounds.\n";
    cerr << "  J = " << j << "\n";
    cerr << "  Maximum legal value is M = " << m << "\n";
    exit ( 1 );
  }

  if ( i == j )
  {
    return 0;
  }

  for ( k = 0; k < n; k++ )
  {
    if ( a[(i-1)*n+k] < a[(j-1)*n+k] )
    {
      return -1;
    }
    else if ( a[(j-1)*n+k] < a[(i-1)*n+k] )
    {
      return +1;
    }
  }

  return 0;
}
//****************************************************************************80

void i4row_sort_a ( int m, int n, int a[] )

//****************************************************************************80
//
//  Purpose:
//
//    I4ROW_SORT_A ascending sorts the rows of an I4ROW.
//
//  Discussion:
//
//    In lexicographic order, the statement "X < Y", applied to two
//    vectors X and Y of length M, means that there is some index I, with
//    1 <= I <= M, with the property that
//
//      X(J) = Y(J) for J < I,
//    and
//      X(I) < Y(I).
//
//    In other words, X is less than Y if, at the first index where they
//    differ, the X value is less than the Y value.
//
//  Example:
//
//    Input:
//
//      M = 5, N = 3
//
//      A =
//        3  2  1
//        2  4  3
//        3  1  8
//        2  4  2
//        1  9  9
//
//    Output:
//
//      A =
//        1  9  9
//        2  4  2
//        2  4  3
//        3  1  8
//        3  2  1
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the number of rows of A.
//
//    Input, int N, the number of columns of A.
//
//    Input/output, int A[M*N].
//    On input, the array of M rows of N-vectors.
//    On output, the rows of A have been sorted in ascending
//    lexicographic order.
//
{
  int i;
  int indx;
  int isgn;
  int j;
//
//  Initialize.
//
  i = 0;
  indx = 0;
  isgn = 0;
  j = 0;
//
//  Call the external heap sorter.
//
  for ( ; ; )
  {
    sort_heap_external ( m, &indx, &i, &j, isgn );
//
//  Interchange the I and J objects.
//
    if ( 0 < indx )
    {
      i4row_swap ( m, n, a, i, j );
    }
//
//  Compare the I and J objects.
//
    else if ( indx < 0 )
    {
      isgn = i4row_compare ( m, n, a, i, j );
    }
    else if ( indx == 0 )
    {
      break;
    }

  }

  return;
}
//****************************************************************************80

void i4row_swap ( int m, int n, int a[], int irow1, int irow2 )

//****************************************************************************80
//
//  Purpose:
//
//    I4ROW_SWAP swaps two rows of an I4ROW.
//
//  Discussion:
//
//    The two dimensional information is stored as a one dimensional
//    array, by columns.
//
//    The row indices are 1 based, NOT 0 based!  However, a preprocessor
//    variable, called OFFSET, can be reset from 1 to 0 if you wish to
//    use 0-based indices.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns.
//
//    Input/output, int A[M*N], an array of data.
//
//    Input, int IROW1, IROW2, the two rows to swap.
//    These indices should be between 1 and M.
//
{
# define OFFSET 1

  int j;
  int t;
//
//  Check.
//
  if ( irow1 < 0+OFFSET || m-1+OFFSET < irow1 )
  {
    cerr << "\n";
    cerr << "I4ROW_SWAP - Fatal error!\n";
    cerr << "  IROW1 is out of range.\n";
    exit ( 1 );
  }

  if ( irow2 < 0+OFFSET || m-1+OFFSET < irow2 )
  {
    cerr << "\n";
    cerr << "I4ROW_SWAP - Fatal error!\n";
    cerr << "  IROW2 is out of range.\n";
    exit ( 1 );
  }

  if ( irow1 == irow2 )
  {
    return;
  }
  for ( j = 0; j < n; j++ )
  {
    t                   = a[irow1-OFFSET+j*m];
    a[irow1-OFFSET+j*m] = a[irow2-OFFSET+j*m];
    a[irow2-OFFSET+j*m] = t;
  }

  return;
# undef OFFSET
}
//****************************************************************************80

void i4vec_copy ( int n, int a1[], int a2[] )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC_COPY copies an I4VEC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the vectors.
//
//    Input, int A1[N], the vector to be copied.
//
//    Input, int A2[N], the copy of A1.
//
{
  int i;

  for ( i = 0; i < n; i++ )
  {
    a2[i] = a1[i];
  }
  return;
}
//****************************************************************************80

void i4vec_heap_d ( int n, int a[] )

//****************************************************************************80
//
//
//  Purpose:
//
//    I4VEC_HEAP_D reorders an I4VEC into a descending heap.
//
//  Discussion:
//
//    A heap is an array A with the property that, for every index J,
//    A[J] >= A[2*J+1] and A[J] >= A[2*J+2], (as long as the indices
//    2*J+1 and 2*J+2 are legal).
//
//  Diagram:
//
//                  A(0)
//                        
//            A(1)         A(2)
//                             
//      A(3)       A(4)  A(5) A(6)
//                     
//    A(7) A(8)  A(9) A(10)
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Albert Nijenhuis, Herbert Wilf,
//    Combinatorial Algorithms,
//    Academic Press, 1978, second edition,
//    ISBN 0-12-519260-6.
//
//  Parameters:
//
//    Input, int N, the size of the input array.
//
//    Input/output, int A[N].
//    On input, an unsorted array.
//    On output, the array has been reordered into a heap.
//
{
  int i;
  int ifree;
  int key;
  int m;
//
//  Only nodes (N/2)-1 down to 0 can be "parent" nodes.
//
  for ( i = (n/2)-1; 0 <= i; i-- )
  {
//
//  Copy the value out of the parent node.
//  Position IFREE is now "open".
//
    key = a[i];
    ifree = i;

    for ( ;; )
    {
//
//  Positions 2*IFREE + 1 and 2*IFREE + 2 are the descendants of position
//  IFREE.  (One or both may not exist because they equal or exceed N.)
//
      m = 2 * ifree + 1;
//
//  Does the first position exist?
//
      if ( n <= m )
      {
        break;
      }
      else
      {
//
//  Does the second position exist?
//
        if ( m + 1 < n )
        {
//
//  If both positions exist, take the larger of the two values,
//  and update M if necessary.
//
          if ( a[m] < a[m+1] )
          {
            m = m + 1;
          }
        }
//
//  If the large descendant is larger than KEY, move it up,
//  and update IFREE, the location of the free position, and
//  consider the descendants of THIS position.
//
        if ( key < a[m] )
        {
          a[ifree] = a[m];
          ifree = m;
        }
        else
        {
          break;
        }
      }
    }
//
//  When you have stopped shifting items up, return the item you
//  pulled out back to the heap.
//
    a[ifree] = key;
  }

  return;
}
//****************************************************************************80

int *i4vec_indicator_new ( int n )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC_INDICATOR_NEW sets an I4VEC to the indicator vector.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    25 February 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of elements of A.
//
//    Output, int I4VEC_INDICATOR(N), the initialized array.
//
{
  int *a;
  int i;

  a = new int[n];

  for ( i = 0; i < n; i++ )
  {
    a[i] = i + 1;
  }

  return a;
}
//****************************************************************************80

int i4vec_lcm ( int n, int v[] )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC_LCM returns the least common multiple of an I4VEC.
//
//  Discussion:
//
//    An I4VEC is a vector of I4's.
//
//    The value LCM returned has the property that it is the smallest integer
//    which is evenly divisible by every element of V.
//
//    The entries in V may be negative.
//
//    If any entry of V is 0, then LCM is 0.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the order of V.
//
//    Input, int V[N], the vector.
//
//    Output, int I4VEC_LCM, the least common multiple of V.
//
{
  int i;
  int lcm;

  lcm = 1;

  for ( i = 0; i < n; i++ )
  {
    if ( v[i] == 0 )
    {
      lcm = 0;
      break;
    }
    lcm = i4_lcm ( lcm, v[i] );
  }
  return lcm;
}
//****************************************************************************80

void i4vec_print ( int n, int a[], string title )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC_PRINT prints an I4VEC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    25 February 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of components of the vector.
//
//    Input, int A[N], the vector to be printed.
//
//    Input, string TITLE, a title.
//
{
  int i;

  cout << "\n";
  cout << title << "\n";
  cout << "\n";
  for ( i = 0; i <= n-1; i++ )
  {
    cout << setw(6) << i << ": " << setw(8) << a[i] << "\n";
  }

  return;
}
//****************************************************************************80

int i4vec_product ( int n, int a[] )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC_PRODUCT multiplies the entries of an I4VEC.
//
//  Discussion:
//
//    An I4VEC is a vector of I4's.
//
//  Example:
//
//    Input:
//
//      A = ( 1, 2, 3, 4 )
//
//    Output:
//
//      I4VEC_PRODUCT = 24
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 May 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the vector.
//
//    Input, int A[N], the vector
//
//    Output, int I4VEC_PRODUCT, the product of the entries of A.
//
{
  int i;
  int product;

  product = 1;
  for ( i = 0; i < n; i++ )
  {
    product = product * a[i];
  }

  return product;
}
//****************************************************************************80

void i4vec_reverse ( int n, int a[] )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC_REVERSE reverses the elements of an I4VEC.
//
//  Discussion:
//
//    An I4VEC is a vector of I4's.
//
//  Example:
//
//    Input:
//
//      N = 5,
//      A = ( 11, 12, 13, 14, 15 ).
//
//    Output:
//
//      A = ( 15, 14, 13, 12, 11 ).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 September 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the array.
//
//    Input/output, int A(N), the array to be reversed.
//
{
  int i;
  int j;

  for ( i = 0; i < n / 2; i++ )
  {
    j        = a[i];
    a[i]     = a[n-1-i];
    a[n-1-i] = j;
  }

  return;
}
//****************************************************************************80

void i4vec_sort_heap_a ( int n, int a[] )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Albert Nijenhuis, Herbert Wilf,
//    Combinatorial Algorithms,
//    Academic Press, 1978, second edition,
//    ISBN 0-12-519260-6.
//
//  Parameters:
//
//    Input, int N, the number of entries in the array.
//
//    Input/output, int A[N].
//    On input, the array to be sorted;
//    On output, the array has been sorted.
//
{
  int n1;
  int temp;

  if ( n <= 1 )
  {
    return;
  }
//
//  1: Put A into descending heap form.
//
  i4vec_heap_d ( n, a );
//
//  2: Sort A.
//
//  The largest object in the heap is in A[0].
//  Move it to position A[N-1].
//
  temp = a[0];
  a[0] = a[n-1];
  a[n-1] = temp;
//
//  Consider the diminished heap of size N1.
//
  for ( n1 = n-1; 2 <= n1; n1-- )
  {
//
//  Restore the heap structure of the initial N1 entries of A.
//
    i4vec_heap_d ( n1, a );
//
//  Take the largest object from A[0] and move it to A[N1-1].
//
    temp = a[0];
    a[0] = a[n1-1];
    a[n1-1] = temp;

  }

  return;
}
//****************************************************************************80

void i4vec_sorted_unique ( int n, int a[], int *nuniq )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC_SORTED_UNIQUE finds unique elements in a sorted I4VEC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of elements in A.
//
//    Input/output, int A[N].  On input, the sorted
//    integer array.  On output, the unique elements in A.
//
//    Output, int *NUNIQ, the number of unique elements in A.
//
{
  int i;

  *nuniq = 0;

  if ( n <= 0 )
  {
    return;
  }

  *nuniq = 1;

  for ( i = 1; i < n; i++ )
  {
    if ( a[i] != a[*nuniq] )
    {
      *nuniq = *nuniq + 1;
      a[*nuniq] = a[i];
    }

  }

  return;
}
//****************************************************************************80

int *i4vec_uniform_ab_new ( int n, int a, int b, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC_UNIFORM_AB_NEW returns a scaled pseudorandom I4VEC.
//
//  Discussion:
//
//    The pseudorandom numbers should be uniformly distributed
//    between A and B.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 November 2006
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paul Bratley, Bennett Fox, Linus Schrage,
//    A Guide to Simulation,
//    Springer Verlag, pages 201-202, 1983.
//
//    Pierre L'Ecuyer,
//    Random Number Generation,
//    in Handbook of Simulation,
//    edited by Jerry Banks,
//    Wiley Interscience, page 95, 1998.
//
//    Bennett Fox,
//    Algorithm 647:
//    Implementation and Relative Efficiency of Quasirandom
//    Sequence Generators,
//    ACM Transactions on Mathematical Software,
//    Volume 12, Number 4, pages 362-376, 1986.
//
//    Peter Lewis, Allen Goodman, James Miller
//    A Pseudo-Random Number Generator for the System/360,
//    IBM Systems Journal,
//    Volume 8, pages 136-143, 1969.
//
//  Parameters:
//
//    Input, integer N, the dimension of the vector.
//
//    Input, int A, B, the limits of the interval.
//
//    Input/output, int &SEED, the "seed" value, which should NOT be 0.
//    On output, SEED has been updated.
//
//    Output, int IVEC_UNIFORM[N], a vector of random values between A and B.
//
{
  int i;
  int k;
  float r;
  int value;
  int *x;

  if ( seed == 0 )
  {
    cerr << "\n";
    cerr << "I4VEC_UNIFORM - Fatal error!\n";
    cerr << "  Input value of SEED = 0.\n";
    exit ( 1 );
  }

  x = new int[n];

  for ( i = 0; i < n; i++ )
  {
    k = seed / 127773;

    seed = 16807 * ( seed - k * 127773 ) - k * 2836;

    if ( seed < 0 )
    {
      seed = seed + 2147483647;
    }

    r = ( ( float ) ( seed ) ) * 4.656612875E-10;
//
//  Scale R to lie between A-0.5 and B+0.5.
//
    r = ( 1.0 - r ) * ( ( ( float ) ( i4_min ( a, b ) ) ) - 0.5 )
      +         r   * ( ( ( float ) ( i4_max ( a, b ) ) ) + 0.5 );
//
//  Use rounding to convert R to an integer between A and B.
//
    value = r4_nint ( r );

    value = i4_max ( value, i4_min ( a, b ) );
    value = i4_min ( value, i4_max ( a, b ) );

    x[i] = value;
  }

  return x;
}
//****************************************************************************80

void i4vec_zero ( int n, int a[] )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC_ZERO zeroes an I4VEC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the vector.
//
//    Output, int A[N], a vector of zeroes.
//
{
  int i;

  for ( i = 0; i < n; i++ )
  {
    a[i] = 0;
  }
  return;
}

//****************************************************************************80

int i4vec2_compare ( int n, int a1[], int a2[], int i, int j )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC2_COMPARE compares pairs of integers stored in two vectors.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of data items.
//
//    Input, int A1[N], A2[N], contain the two components of each item.
//
//    Input, int I, J, the items to be compared.  These values will be
//    1-based indices for the arrays A1 and A2.
//
//    Output, int I4VEC2_COMPARE, the results of the comparison:
//    -1, item I < item J,
//     0, item I = item J,
//    +1, item J < item I.
//
{
  int isgn;

  isgn = 0;

       if ( a1[i-1] < a1[j-1] )
  {
    isgn = -1;
  }
  else if ( a1[i-1] == a1[j-1] )
  {
         if ( a2[i-1] < a2[j-1] )
    {
      isgn = -1;
    }
    else if ( a2[i-1] < a2[j-1] )
    {
      isgn = 0;
    }
    else if ( a2[j-1] < a2[i-1] )
    {
      isgn = +1;
    }
  }
  else if ( a1[j-1] < a1[i-1] )
  {
    isgn = +1;
  }

  return isgn;
}
//****************************************************************************80

void i4vec2_sort_a ( int n, int a1[], int a2[] )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC2_SORT_A ascending sorts a vector of pairs of integers.
//
//  Discussion:
//
//    Each item to be sorted is a pair of integers (I,J), with the I
//    and J values stored in separate vectors A1 and A2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of items of data.
//
//    Input/output, int A1[N], A2[N], the data to be sorted.
//
{
  int i;
  int indx;
  int isgn;
  int j;
  int temp;
//
//  Initialize.
//
  i = 0;
  indx = 0;
  isgn = 0;
  j = 0;
//
//  Call the external heap sorter.
//
  for ( ; ; )
  {
    sort_heap_external ( n, &indx, &i, &j, isgn );
//
//  Interchange the I and J objects.
//
    if ( 0 < indx )
    {
      temp    = a1[i-1];
      a1[i-1] = a1[j-1];
      a1[j-1] = temp;

      temp    = a2[i-1];
      a2[i-1] = a2[j-1];
      a2[j-1] = temp;
    }
//
//  Compare the I and J objects.
//
    else if ( indx < 0 )
    {
      isgn = i4vec2_compare ( n, a1, a2, i, j );
    }
    else if ( indx == 0 )
    {
      break;
    }

  }

  return;
}
//****************************************************************************80

void i4vec2_sorted_unique ( int n, int a1[], int a2[], int *nuniq )

//****************************************************************************80
//
//  Purpose:
//
//    I4VEC2_SORTED_UNIQUE finds unique elements in a sorted I4VEC2.
//
//  Discussion:
//
//    Item I is stored as the pair A1(I), A2(I).
//
//    The items must have been sorted, or at least it must be the
//    case that equal items are stored in adjacent vector locations.
//
//    If the items were not sorted, then this routine will only
//    replace a string of equal values by a single representative.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 July 2000
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of items.
//
//    Input/output, int A1[N], A2[N].
//    On input, the array of N items.
//    On output, an array of NUNIQ unique items.
//
//    Output, int *NUNIQ, the number of unique items.
//
{
  int itest;

  *nuniq = 0;

  if ( n <= 0 )
  {
    return;
  }

  *nuniq = 1;

  for ( itest = 1; itest < n; itest++ )
  {
    if ( a1[itest] != a1[*nuniq-1] ||
         a2[itest] != a2[*nuniq-1] )
    {
      a1[*nuniq] = a1[itest];
      a2[*nuniq] = a2[itest];
      *nuniq = *nuniq + 1;
    }
  }

  return;
}
//****************************************************************************80

void icos_shape ( int point_num, int edge_num, int face_num,
  int face_order_max, double point_coord[], int edge_point[], int face_order[],
  int face_point[] )

//****************************************************************************80
//
//  Purpose:
//
//    ICOS_SHAPE describes a icosahedron.
//
//  Discussion:
//
//    The input data required for this routine can be retrieved from
//    ICOS_SIZE.
//
//    The vertices lie on the unit sphere.
//
//    The dual of an icosahedron is the dodecahedron.
//
//    The data has been rearranged from a previous assignment.
//    The STRIPACK program refuses to triangulate data if the first
//    three nodes are "collinear" on the sphere.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    24 September 2010.
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int POINT_NUM, the number of points (12).
//
//    Input, int EDGE_NUM, the number of edges (30).
//
//    Input, int FACE_NUM, the number of faces (20).
//
//    Input, int FACE_ORDER_MAX, the maximum number of vertices
//    per face (3).
//
//    Output, double POINT_COORD[3*POINT_NUM], the point coordinates.
//
//    Output, int EDGE_POINT[2*EDGE_NUM], the points that make up each
//    edge, listed in ascending order of their indexes.
//
//    Output, int FACE_ORDER[FACE_NUM], the number of vertices per face.
//
//    Output, int FACE_POINT[FACE_ORDER_MAX*FACE_NUM]; FACE_POINT(I,J)
//    contains the index of the I-th point in the J-th face.  The
//    points are listed in the counter clockwise direction defined
//    by the outward normal at the face.  The nodes of each face are
//    ordered so that the lowest index occurs first.  The faces are
//    then sorted by nodes.
//
{
# define DIM_NUM 3
# define EDGE_NUM 30
# define EDGE_ORDER 2
# define FACE_NUM 20
# define POINT_NUM 12

  double phi = 0.5 * ( sqrt ( 5.0 ) + 1.0 );

  double a = phi / sqrt ( 1.0 + phi * phi );
  double b = 1.0 / sqrt ( 1.0 + phi * phi );
  double z = 0.0;

  static int edge_point_save[EDGE_ORDER*EDGE_NUM] = {
     1,  2,
     1,  3,
     1,  4,
     1,  5,
     1,  6,
     2,  3,
     2,  4,
     2,  7,
     2,  8,
     3,  5,
     3,  7,
     3,  9,
     4,  6,
     4,  8,
     4, 10,
     5,  6,
     5,  9,
     5, 11,
     6, 10,
     6, 11,
     7,  8,
     7,  9,
     7, 12,
     8, 10,
     8, 12,
     9, 11,
     9, 12,
    10, 11,
    10, 12,
    11, 12 };
  static int face_order_save[FACE_NUM] = {
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3 };
  static int face_point_save[3*FACE_NUM] = {
     1,  2,  4,
     1,  3,  2,
     1,  4,  6,
     1,  5,  3,
     1,  6,  5,
     2,  3,  7,
     2,  7,  8,
     2,  8,  4,
     3,  5,  9,
     3,  9,  7,
     4,  8, 10,
     4, 10,  6,
     5,  6, 11,
     5, 11,  9,
     6, 10, 11,
     7,  9, 12,
     7, 12,  8,
     8, 12, 10,
     9, 11, 12,
    10, 12, 11 };
  int i;
  int j;
  static double point_coord_save[DIM_NUM*POINT_NUM] = {
      a,  b,  z,
      a, -b,  z,
      b,  z,  a,
      b,  z, -a,
      z,  a,  b,
      z,  a, -b,
      z, -a,  b,
      z, -a, -b,
     -b,  z,  a,
     -b,  z, -a,
     -a,  b,  z,
     -a, -b,  z };

  r8vec_copy ( DIM_NUM * point_num,       point_coord_save, point_coord );
  i4vec_copy ( EDGE_ORDER * edge_num,     edge_point_save,  edge_point );
  i4vec_copy ( face_num,                  face_order_save,  face_order );
  i4vec_copy ( face_order_max * face_num, face_point_save,  face_point );
//
//  Really, the indices in EDGE_POINT and FACE_POINT should be 
//  decremented by 1, so that they can be conveniently used as
//  C++ indices.  However, I have to do this to ALL the shape
//  functions, and then warn DUAL_SIZE_3D that it has been done...
//  So for now, maybe DON'T do this.
//
  if ( 0 )
  {
    for ( j = 0; j < edge_num; j++ )
    {
      for ( i = 0; i < 2; i++ )
      {
        edge_point[i+j*2] = edge_point[i+j*2] - 1;
      }
    }

    for ( j = 0; j < face_num; j++ )
    {
      for ( i = 0; i < face_order_max; i++ )
      {
        face_point[i+j*face_order_max] = face_point[i+j*face_order_max] - 1;
      }
    }
  }
  return;
# undef DIM_NUM
# undef EDGE_NUM
# undef EDGE_ORDER
# undef FACE_NUM
# undef POINT_NUM
}
//****************************************************************************80

void icos_size ( int *point_num, int *edge_num, int *face_num,
  int *face_order_max )

//****************************************************************************80
//
//  Purpose:
//
//    ICOS_SIZE gives "sizes" for an icosahedron.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 July 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, int *POINT_NUM, the number of points.
//
//    Output, int *EDGE_NUM, the number of edges.
//
//    Output, int *FACE_NUM, the number of faces.
//
//    Output, int *FACE_ORDER_MAX, the maximum order of any face.
//
{
  *point_num = 12;
  *edge_num = 30;
  *face_num = 20;
  *face_order_max = 3;

  return;
}
//****************************************************************************80

bool line_exp_is_degenerate_nd ( int dim_num, double p1[], double p2[] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_EXP_IS_DEGENERATE_ND finds if an explicit line is degenerate in ND.
//
//  Discussion:
//
//    The explicit form of a line in ND is:
//
//      the line through the points P1 and P2.
//
//    An explicit line is degenerate if the two defining points are equal.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, double P1[DIM_NUM], P2[DIM_NUM], two points on the line.
//
//    Output, bool LINE_EXP_IS_DEGENERATE_ND, is TRUE if the line
//    is degenerate.
//
{
  bool value;

  value = r8vec_eq ( dim_num, p1, p2 );

  return value;
}
//****************************************************************************80

double *line_exp_normal_2d ( double p1[2], double p2[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_EXP_NORMAL_2D computes the unit normal vector to a line in 2D.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through the points P1 and P2.
//
//    The sign of the normal vector N is chosen so that the normal vector
//    points "to the left" of the direction of the line.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 October 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], two distinct points on the line.
//
//    Output, double LINE_EXP_NORMAL_2D[2], a unit normal vector to the line.
//
{
# define DIM_NUM 2

  double norm;
  double *normal;

  normal = new double[DIM_NUM];

  norm = sqrt ( ( p2[0] - p1[0] ) * ( p2[0] - p1[0] )
              + ( p2[1] - p1[1] ) * ( p2[1] - p1[1] ));

  if ( norm == 0.0 )
  {
    normal[0] = sqrt ( 2.0 );
    normal[1] = sqrt ( 2.0 );
  }
  else
  {
    normal[0] = - ( p2[1] - p1[1] ) / norm;
    normal[1] =   ( p2[0] - p1[0] ) / norm;
  }

  return normal;
# undef DIM_NUM
}
//****************************************************************************80

double *line_exp_perp_2d ( double p1[2], double p2[2], double p3[2],
  bool *flag )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_EXP_PERP_2D computes a line perpendicular to a line and through a point.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    31 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], two points on the given line.
//
//    Input, double P3[2], a point not on the given line, through which the
//    perpendicular must pass.
//
//    Output, bool *FLAG, is TRUE if the value could not be computed.
//
//    Output, double LINE_EXP_PERP_2D[2], a point on the given line, such that
//    the line through P3 and P4 is perpendicular to the given line.
//
{
# define DIM_NUM 2

  double bot;
  double *p4;
  double t;

  p4 = new double[DIM_NUM];
  *flag = false;

  bot = pow ( p2[0] - p1[0], 2 ) + pow ( p2[1] - p1[1], 2 );

  if ( bot == 0.0 )
  {
    *flag = true;
    p4[0] = HUGE_VAL;
    p4[1] = HUGE_VAL;
    return p4;
  }
//
//  (P3-P1) dot (P2-P1) = Norm(P3-P1) * Norm(P2-P1) * Cos(Theta).
//
//  (P3-P1) dot (P2-P1) / Norm(P3-P1)^2 = normalized coordinate T
//  of the projection of (P3-P1) onto (P2-P1).
//
  t = ( ( p1[0] - p3[0] ) * ( p1[0] - p2[0] )
      + ( p1[1] - p3[1] ) * ( p1[1] - p2[1] ) ) / bot;

  p4[0] = p1[0] + t * ( p2[0] - p1[0] );
  p4[1] = p1[1] + t * ( p2[1] - p1[1] );

  return p4;
# undef DIM_NUM
}
//****************************************************************************80

double line_exp_point_dist_2d ( double p1[2], double p2[2], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_EXP_POINT_DIST_2D: distance ( explicit line, point ) in 2D.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], two points on the line.
//
//    Input, double P[2], the point whose distance from the line is
//    to be measured.
//
//    Output, double LINE_EXP_DIST_2D, the distance from the point to the line.
//
{
  double bot;
  double dist;
  double dot;
  double t;
  double pn[2];

  bot = ( pow ( p2[0] - p1[0], 2 )
        + pow ( p2[1] - p1[1], 2 ) );

  if ( bot == 0.0 )
  {
    pn[0] = p1[0];
    pn[1] = p1[1];
  }
//
//  (P-P1) dot (P2-P1) = Norm(P-P1) * Norm(P2-P1) * Cos(Theta).
//
//  (P-P1) dot (P2-P1) / Norm(P-P1)^2 = normalized coordinate T
//  of the projection of (P-P1) onto (P2-P1).
//
  else
  {

    dot =
        ( p[0] - p1[0] ) * ( p2[0] - p1[0] )
      + ( p[1] - p1[1] ) * ( p2[1] - p1[1] );

    t = dot / bot;

    pn[0] = p1[0] + t * ( p2[0] - p1[0] );
    pn[1] = p1[1] + t * ( p2[1] - p1[1] );

  }

  dist = sqrt ( pow ( p[0] - pn[0], 2 )
              + pow ( p[1] - pn[1], 2 ) );

  return dist;
}
//****************************************************************************80

double line_exp_point_dist_3d ( double p1[3], double p2[3], double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_EXP_POINT_DIST_3D: distance ( explicit line, point ) in 3D.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], two points on a line.
//
//    Input, double P[3], the point whose distance from the line is
//    to be measured.
//
//    Output, double LINE_EXP_POINT_DIST_3D, the distance from the point
//    to the line.
//
{
# define DIM_NUM 3

  double bot;
  double dist;
  double t;
  double pn[DIM_NUM];

  bot = ( pow ( p2[0] - p1[0], 2 )
        + pow ( p2[1] - p1[1], 2 )
        + pow ( p2[2] - p1[2], 2 ) );

  if ( bot == 0.0 )
  {
    r8vec_copy ( DIM_NUM, p1, pn );
  }
//
//  (P-P1) dot (P2-P1) = Norm(P-P1) * Norm(P2-P1) * Cos(Theta).
//
//  (P-P1) dot (P2-P1) / Norm(P-P1)^2 = normalized coordinate T
//  of the projection of (P-P1) onto (P2-P1).
//
  else
  {

    t = (
      ( p[0] - p1[0] ) * ( p2[0] - p1[0] ) +
      ( p[1] - p1[1] ) * ( p2[1] - p1[1] ) +
      ( p[2] - p1[2] ) * ( p2[2] - p1[2] ) ) / bot;

    pn[0] = p1[0] + t * ( p2[0] - p1[0] );
    pn[1] = p1[1] + t * ( p2[1] - p1[1] );
    pn[2] = p1[2] + t * ( p2[2] - p1[2] );
  }
//
//  Now compute the distance between the projection point and P.
//
  dist = sqrt ( pow ( p[0] - pn[0], 2 )
              + pow ( p[1] - pn[1], 2 )
              + pow ( p[2] - pn[2], 2 ) );

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

double line_exp_point_dist_signed_2d ( double p1[2], double p2[2], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_EXP_POINT_DIST_SIGNED_2D: signed distance ( explicit line, point ) in 2D.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through P1 and P2.
//
//    The signed distance has two interesting properties:
//
//    *  The absolute value of the signed distance is the
//       usual (Euclidean) distance.
//
//    *  Points with signed distance 0 lie on the line,
//       points with a negative signed distance lie on one side
//         of the line,
//       points with a positive signed distance lie on the
//         other side of the line.
//
//    Assuming that C is nonnegative, then if a point is a positive
//    distance away from the line, it is on the same side of the
//    line as the point (0,0), and if it is a negative distance
//    from the line, it is on the opposite side from (0,0).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], two points that determine the line.
//
//    Input, double P[2], the point whose signed distance is desired.
//
//    Output, double LINE_EXP_DIST_SIGNED_2D, the signed distance from the
//    point to the line.
//
{
  double a;
  double b;
  double c;
  double dist_signed;
//
//  Convert the line to A*x+B*y+C form.
//
  line_exp2imp_2d ( p1, p2, &a, &b, &c );
//
//  Compute the signed distance from the point to the line.
//
  dist_signed = ( a * p[0] + b * p[1] + c ) / sqrt ( a * a + b * b );

  return dist_signed;
}
//****************************************************************************80

void line_exp_point_near_2d ( double p1[2], double p2[2], double p[2],
  double pn[2], double *dist, double *t )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_EXP_POINT_NEAR_2D computes the point on an explicit line nearest a point in 2D.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through P1 and P2.
//
//    The nearest point PN will have the form:
//
//      PN = (1-T) * P1 + T * P2.
//
//    If T is less than 0, then PN is furthest away from P2.
//    If T is between 0 and 1, PN is between P1 and P2.
//    If T is greater than 1, PN is furthest away from P1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], two points that define a line.
//
//    Input, double P[2], the point whose nearest neighbor on the line is
//    to be determined.
//
//    Output, double PN[2], the nearest point on the line to P.
//
//    Output, double DIST, the distance from the point to the line.
//
//    Output, double *T, the relative position of the point PN to the points P1 and P2.
//
{
  double bot;

  bot = pow ( p2[0] - p1[0], 2 ) + pow ( p2[1] - p1[1], 2 );

  if ( bot == 0.0 )
  {
    cerr << "\n";
    cerr << "LINE_EXP_POINT_NEAR_2D - Fatal error!\n";
    cerr << "  The points P1 and P2 are identical.\n";
    exit ( 1 );
  }
//
//  (P-P1) dot (P2-P1) = Norm(P-P1) * Norm(P2-P1) * Cos(Theta).
//
//  (P-P1) dot (P2-P1) / Norm(P-P1)^2 = normalized coordinate T
//  of the projection of (P-P1) onto (P2-P1).
//
  *t = ( ( p1[0] - p[0] ) * ( p1[0] - p2[0] )
       + ( p1[1] - p[1] ) * ( p1[1] - p2[1] ) ) / bot;

  pn[0] = p1[0] + (*t) * ( p2[0] - p1[0] );
  pn[1] = p1[1] + (*t) * ( p2[1] - p1[1] );

  *dist = sqrt ( pow ( p[0] - pn[0], 2 ) + pow ( p[1] - pn[1], 2 ) );

  return;
}
//****************************************************************************80

void line_exp_point_near_3d ( double p1[3], double p2[3], double p[3],
  double pn[3], double *dist, double *t )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_EXP_POINT_NEAR_3D: nearest point on explicit line to point in 3D.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through P1 and P2.
//
//    The nearest point PN will have the form:
//
//      PN = (1-T) * P1 + T * P2.
//
//    If T is less than 0, then PN is furthest away from P2.
//    If T is between 0 and 1, PN is between P1 and P2.
//    If T is greater than 1, PN is furthest away from P1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], two points that define a line.
//
//    Input, double P[3], the point whose nearest neighbor on the line is
//    to be determined.
//
//    Output, double PN[3], the nearest point on the line to P.
//
//    Output, double DIST, the distance from the point to the line.
//
//    Output, double *T, the relative position of the point PN to the points P1 and P2.
//
{
  double bot;

  bot = pow ( p2[0] - p1[0], 2 )
      + pow ( p2[1] - p1[1], 2 )
      + pow ( p2[2] - p1[2], 2 );

  if ( bot == 0.0 )
  {
    cerr << "\n";
    cerr << "LINE_EXP_POINT_NEAR_3D - Fatal error!\n";
    cerr << "  The points P1 and P2 are identical.\n";
    exit ( 1 );
  }
//
//  (P-P1) dot (P2-P1) = Norm(P-P1) * Norm(P2-P1) * Cos(Theta).
//
//  (P-P1) dot (P2-P1) / Norm(P-P1)^2 = normalized coordinate T
//  of the projection of (P-P1) onto (P2-P1).
//
  *t = ( ( p1[0] - p[0] ) * ( p1[0] - p2[0] )
       + ( p1[1] - p[1] ) * ( p1[1] - p2[1] )
       + ( p1[2] - p[2] ) * ( p1[2] - p2[2] ) ) / bot;
//
//  Now compute the location of the projection point.
//
  pn[0] = p1[0] + (*t) * ( p2[0] - p1[0] );
  pn[1] = p1[1] + (*t) * ( p2[1] - p1[1] );
  pn[2] = p1[2] + (*t) * ( p2[2] - p1[2] );
//
//  Now compute the distance between the projection point and P.
//
  *dist = sqrt ( pow ( p[0] - pn[0], 2 )
               + pow ( p[1] - pn[1], 2 )
               + pow ( p[2] - pn[2], 2 ) );
  return;
}
//****************************************************************************80

void line_exp2imp_2d ( double p1[2], double p2[2], double *a, double *b,
  double *c )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_EXP2IMP_2D converts an explicit line to implicit form in 2D.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through P1 and P2
//
//    The implicit form of a line in 2D is:
//
//      A * X + B * Y + C = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], two distinct points on the line.
//
//    Output, double *A, *B, *C, three coefficients which describe
//    the line that passes through P1 and P2.
//
{
//
//  Take care of degenerate cases.
//
  if ( r8vec_eq ( 2, p1, p2 ) )
  {
    cerr << "\n";
    cerr << "LINE_EXP2IMP_2D - Fatal error!\n";
    cerr << "  P1 = P2\n";
    cerr << "  P1 = " << p1[0] << " " << p1[1] << "\n";
    cerr << "  P2 = " << p2[0] << " " << p2[1] << "\n";
    exit ( 1 );
  }

  *a = p2[1] - p1[1];
  *b = p1[0] - p2[0];
  *c = p2[0] * p1[1] - p1[0] * p2[1];

  return;
}
//****************************************************************************80

void line_exp2par_2d ( double p1[2], double p2[2], double *f, double *g,
  double *x0, double *y0 )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_EXP2PAR_2D converts a line from explicit to parametric form in 2D.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through P1 and P2.
//
//    The parametric form of a line in 2D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//
//    For normalization, we choose F*F+G*G = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], two points on the line.
//
//    Output, double *F, *G, *X0, *Y0, the parametric parameters of the line.
//
{
  double norm;

  *x0 = p1[0];
  *y0 = p1[1];

  norm = sqrt ( ( p2[0] - p1[0] ) * ( p2[0] - p1[0] )
              + ( p2[1] - p1[1] ) * ( p2[1] - p1[1] ) );

  if ( norm == 0.0 )
  {
    *f = 0.0;
    *g = 0.0;
  }
  else
  {
    *f = ( p2[0] - p1[0] ) / norm;
    *g = ( p2[1] - p1[1] ) / norm;
  }

  if ( *f < 0.0 )
  {
    *f = - ( *f );
    *g = - ( *g );
  }

  return;
}
//****************************************************************************80

void line_exp2par_3d ( double p1[3], double p2[3], double *f, double *g,
  double *h, double *x0, double *y0, double *z0 )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_EXP2PAR_3D converts an explicit line into parametric form in 3D.
//
//  Discussion:
//
//    The explicit form of a line in 3D is:
//
//      the line through P1 and P2.
//
//    The parametric form of a line in 3D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//      Z = Z0 + H * T
//
//    For normalization, we choose F*F+G*G+H*H = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], two points on the line.
//
//    Output, double *F, *G, *H, the components of the direction vector.
//
//    Output, double *X0, *Y0, *Z0, the base vector.
//
{
  double norm;

  *f = ( p2[0] - p1[0] );
  *g = ( p2[1] - p1[1] );
  *h = ( p2[2] - p1[2] );

  norm = sqrt ( pow ( *f, 2 ) + pow ( *g, 2 ) + pow ( *h, 2 ) );

  if ( norm == 0.0 )
  {
    *f = 0.0;
    *g = 0.0;
    *h = 0.0;
  }
  else
  {
    *f = ( *f ) / norm;
    *g = ( *g ) / norm;
    *h = ( *h ) / norm;
  }

  if ( *f < 0.0 )
  {
    *f = - ( *f );
    *g = - ( *g );
    *h = - ( *h );
  }

  *x0 = p1[0];
  *y0 = p1[1];
  *z0 = p1[2];


  return;
}
//****************************************************************************80

bool line_imp_is_degenerate_2d ( double a, double b, double c )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_IMP_IS_DEGENERATE_2D finds if an implicit point is degenerate in 2D.
//
//  Discussion:
//
//    The implicit form of a line in 2D is:
//
//      A * X + B * Y + C = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, C, the implicit line parameters.
//
//    Output, bool LINE_IMP_IS_DEGENERATE_2D, is true if the
//    line is degenerate.
//
{
  bool value;

  value = ( a * a + b * b == 0.0 );

  return value;
}
//****************************************************************************80

double line_imp_point_dist_2d ( double a, double b, double c, double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_IMP_POINT_DIST_2D: distance ( implicit line, point ) in 2D.
//
//  Discussion:
//
//    The implicit form of a line in 2D is:
//
//      A * X + B * Y + C = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, C, the implicit line parameters.
//
//    Input, double P[2], the point whose distance from the line is
//    to be measured.
//
//    Output, double LINE_IMP_POINT_DIST_2D, the distance from the
//    point to the line.
//
{
  if ( a * a + b * b == 0.0 )
  {
    cerr << "\n";
    cerr << "LINE_IMP_POINT_DIST_2D - Fatal error!\n";
    cerr << "  A * A + B * B = 0.\n";
    exit ( 1 );
  }

  return ( fabs ( a * p[0] + b * p[1] + c ) / sqrt ( a * a + b * b ) );

}
//****************************************************************************80

double line_imp_point_dist_signed_2d ( double a, double b, double c,
  double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_IMP_POINT_DIST_SIGNED_2D: signed distance ( implicit line, point ) in 2D.
//
//  Discussion:
//
//    The implicit form of a line in 2D is:
//
//      A * X + B * Y + C * Z + D = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, C, the equation of the line is A*X + B*Y + C = 0.
//
//    Input, double P[2], the coordinates of the point.
//
//    Output, double LINE_IMP_POINT_DIST_SIGNED_2D, the signed distance
//    from the point to the line.
//
{
  double dist;

  if ( a * a + b * b == 0.0 )
  {
    cerr << "\n";
    cerr << "LINE_IMP_POINT_DIST_SIGNED_2D - Fatal error!\n";
    cerr << "  A * A + B * B = 0.\n";
    exit ( 1 );
  }

  dist = - r8_sign ( c ) * ( a * p[0] + b * p[1] + c ) / sqrt ( a * a + b * b );

  return dist;
}
//****************************************************************************80

void line_imp2exp_2d ( double a, double b, double c, double p1[2],
  double p2[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_IMP2EXP_2D converts an implicit line to explicit form in 2D.
//
//  Discussion:
//
//    The implicit form of line in 2D is:
//
//      A * X + B * Y + C = 0
//
//    The explicit form of a line in 2D is:
//
//      the line through the points P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double A, B, C, the implicit line parameters.
//
//    Output, double P1[2], P2[2], two points on the line.
//
{
# define DIM_NUM 2

  double normsq;

  if ( line_imp_is_degenerate_2d ( a, b, c ) )
  {
    cerr << "\n";
    cerr << "LINE_IMP2EXP_2D - Fatal error!\n";
    cerr << "  The line is degenerate.\n";
    exit ( 1 );
  }

  normsq = a * a + b * b;

  p1[0] = - a * c / normsq;
  p1[1] = - b * c / normsq;

  if ( fabs ( b ) < fabs ( a ) )
  {
    p2[0] = - ( a - b / a ) * c / normsq;
    p2[1] = - ( b + 1.0 ) * c / normsq;
  }
  else
  {
    p2[0] = - ( a + 1.0 ) * c / normsq;
    p2[1] = - ( b - a / b ) * c / normsq;
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

void line_imp2par_2d ( double a, double b, double c, double *f, double *g,
  double *x0, double *y0 )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_IMP2PAR_2D converts an implicit line to parametric form in 2D.
//
//  Discussion:
//
//    The implicit form of line in 2D is:
//
//      A * X + B * Y + C = 0
//
//    The parametric form of a line in 2D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//
//    For normalization, we choose F*F+G*G = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double A, B, C, the implicit parameters of the line.
//
//    Output, double *F, *G, *X0, *Y0, the parametric parameters of the line.
//
{
  double test;

  test = a * a + b * b;

  if ( test == 0.0 )
  {
    cerr << "\n";
    cerr << "LINE_IMP2PAR_2D - Fatal error!\n";
    cerr << "  A * A + B * B = 0.\n";
    exit ( 1 );
  }

  *x0 = - a * c /  test;
  *y0 = - b * c /  test;

  *f =    b  / sqrt ( test );
  *g =  - a  / sqrt ( test );

  if ( *f < 0.0 )
  {
    *f = - ( *f );
    *g = - ( *g );
  }

  return;
}
//****************************************************************************80

double line_par_point_dist_2d ( double f, double g, double x0, double y0,
  double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_PAR_POINT_DIST_2D: distance ( parametric line, point ) in 2D.
//
//  Discussion:
//
//    The parametric form of a line in 2D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//
//    For normalization, we may choose F*F+G*G = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double F, G, X0, Y0, the parametric line parameters.
//
//    Input, double P[2], the point whose distance from the line is
//    to be measured.
//
//    Output, double LINE_PAR_POINT_DIST_2D, the distance from the
//    point to the line.
//
{
  double dx;
  double dy;
  double value;

  dx =   g * g * ( p[0] - x0 ) - f * g * ( p[1] - y0 );
  dy = - f * g * ( p[0] - x0 ) + f * f * ( p[1] - y0 );

  value = sqrt ( dx * dx + dy * dy ) / ( f * f + g * g );

  return value;
}
//****************************************************************************80

double line_par_point_dist_3d ( double f, double g, double h, double x0,
  double y0, double z0, double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_PAR_POINT_DIST_3D: distance ( parametric line, point ) in 3D.
//
//  Discussion:
//
//    The parametric form of a line in 3D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//      Z = Z0 + H * T
//
//    For normalization, we may choose F*F+G*G+H*H = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double F, G, H, X0, Y0, Z0, the parametric line parameters.
//
//    Input, double P[3], the point whose distance from the line is
//    to be measured.
//
//    Output, double LINE_PAR_POINT_DIST_3D, the distance from the point
//    to the line.
//
{
  double dx;
  double dy;
  double dz;
  double value;

  dx =   g * ( f * ( p[1] - y0 ) - g * ( p[0] - x0 ) )
       + h * ( f * ( p[2] - z0 ) - h * ( p[0] - x0 ) );

  dy =   h * ( g * ( p[2] - z0 ) - h * ( p[1] - y0 ) )
       - f * ( f * ( p[1] - y0 ) - g * ( p[0] - x0 ) );

  dz = - f * ( f * ( p[2] - z0 ) - h * ( p[0] - x0 ) )
       - g * ( g * ( p[2] - z0 ) - h * ( p[1] - y0 ) );

  value = sqrt ( dx * dx + dy * dy + dz * dz )
    / ( f * f + g * g + h * h );

  return value;
}
//****************************************************************************80

double *line_par_point_near_2d ( double f, double g, double x0, double y0,
  double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_PAR_POINT_NEAR_2D: nearest point on parametric line to point in 2D.
//
//  Discussion:
//
//    The parametric form of a line in 2D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//
//    For normalization, we may choose F*F+G*G = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    14 April 2013
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double F, G, X0, Y0, the parametric line parameters.
//
//    Input, double P[2], the point whose distance from the line is
//    to be measured.
//
//    Output, double LINE_PAR_POINT_DIST_2D[2], the nearest point.
//
{
  double t;
  double *pn;

  t = ( f * ( p[0] - x0 ) + g * ( p[1] - y0 ) ) / ( f * f + g * g );

  pn = new double[2];

  pn[0] = x0 + t * f;
  pn[1] = y0 + t * g;

  return pn;
}
//****************************************************************************80

double *line_par_point_near_3d ( double f, double g, double h, double x0,
  double y0, double z0, double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_PAR_POINT_DIST_3D: distance ( parametric line, point ) in 3D.
//
//  Discussion:
//
//    The parametric form of a line in 3D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//      Z = Z0 + H * T
//
//    For normalization, we may choose F*F+G*G+H*H = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    14 April 2013
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double F, G, H, X0, Y0, Z0, the parametric line parameters.
//
//    Input, double P[3], the point whose distance from the line is
//    to be measured.
//
//    Output, double LINE_PAR_POINT_NEAR_3D[3], the nearest point.
//
{
  double *pn;
  double t;

  t = ( f * ( p[0] - x0 ) + g * ( p[1] - y0 ) + h * ( p[2] - z0 ) ) 
    / ( f * f + g * g + h * h );

  pn = new double[3];

  pn[0] = x0 + t * f;
  pn[1] = y0 + t * g;
  pn[2] = z0 + t * h;

  return pn;
}
//****************************************************************************80

void line_par2exp_2d ( double f, double g, double x0, double y0,
  double p1[2], double p2[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_PAR2EXP_2D converts a parametric line to explicit form in 2D.
//
//  Discussion:
//
//    The parametric form of a line in 2D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//
//    For normalization, we choose F*F+G*G = 1 and 0 <= F.
//
//    The explicit form of a line in 2D is:
//
//      the line through the points P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double F, G, X0, Y0, the parametric line parameters.
//
//    Output, double P1[2], P2[2], two points on the line.
//
{
# define DIM_NUM 2

  p1[0] = x0;
  p1[1] = y0;

  p2[0] = p1[0] + f;
  p2[1] = p1[1] + g;

  return;
# undef DIM_NUM
}
//****************************************************************************80

void line_par2exp_3d ( double f, double g, double h, double x0, double y0,
  double z0, double p1[3], double p2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_PAR2EXP_2D converts a parametric line to explicit form in 3D.
//
//  Discussion:
//
//    The parametric form of a line in 3D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//      Z = Z0 + H * T
//
//    For normalization, we choose F*F+G*G+H*H = 1 and 0 <= F.
//
//    The explicit form of a line in 3D is:
//
//      the line through the points P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 April 2013
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double F, G, H, X0, Y0, Z0, the parametric line parameters.
//
//    Output, double P1[3], P2[3], two points on the line.
//
{
  p1[0] = x0;
  p1[1] = y0;
  p1[2] = z0;

  p2[0] = p1[0] + f;
  p2[1] = p1[1] + g;
  p2[2] = p1[2] + h;

  return;
}
//****************************************************************************80

void line_par2imp_2d ( double f, double g, double x0, double y0, double *a,
  double *b, double *c )

//****************************************************************************80
//
//  Purpose:
//
//    LINE_PAR2IMP_2D converts a parametric line to implicit form in 2D.
//
//  Discussion:
//
//    The parametric form of a line in 2D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//
//    For normalization, we choose F*F+G*G = 1 and 0 <= F.
//
//    The implicit form of a line in 2D is:
//
//      A * X + B * Y + C = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double F, G, X0, Y0, the parametric parameters of the line.
//
//    Output, double *A, *B, *C, the implicit parameters of the line.
//
{
  *a = -g;
  *b = f;
  *c = g * x0 - f * y0;

  return;
}
//****************************************************************************80

double lines_exp_angle_3d ( double p1[3], double p2[3], double p3[3],
  double p4[3] )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_EXP_ANGLE_3D finds the angle between two explicit lines in 3D.
//
//  Discussion:
//
//    The explicit form of a line in 3D is:
//
//      the line through P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], two distince points on the first line.
//
//    Input, double P3[3], P4[3], two distinct points on the second line.
//
//    Output, double LINES_EXP_ANGLE_3D, the angle in radians between the
//    two lines.  The angle is computed using the ACOS function, and so
//    lies between 0 and PI.  But if one of the lines is degenerate,
//    the angle is returned as -1.0.
//
{
  double angle;
  double ctheta;
  double pdotq;
  double pnorm;
  double qnorm;

  pnorm = sqrt ( pow ( p2[0] - p1[0], 2 )
               + pow ( p2[1] - p1[1], 2 )
               + pow ( p2[2] - p1[2], 2 ) );

  qnorm = sqrt ( pow ( p4[0] - p3[0], 2 )
               + pow ( p4[1] - p3[1], 2 )
               + pow ( p4[2] - p3[2], 2 ) );

  pdotq =    ( p2[0] - p1[0] ) * ( p4[0] - p3[0] )
           + ( p2[1] - p1[1] ) * ( p4[1] - p3[1] )
           + ( p2[2] - p1[2] ) * ( p4[2] - p3[2] );

  if ( pnorm <= 0.0 || qnorm <= 0.0 )
  {
    cout << "\n";
    cout << "LINES_EXP_ANGLE_3D - Warning!\n";
    cout << "  One of the lines is degenerate!\n";
    angle = HUGE_VAL;
  }
  else
  {
    ctheta = pdotq / ( pnorm * qnorm );
    angle = r8_acos ( ctheta );
  }

  return angle;
}
//****************************************************************************80

double lines_exp_angle_nd ( double p1[], double p2[], double q1[], double q2[],
  int dim_num )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_EXP_ANGLE_ND returns the angle between two explicit lines in ND.
//
//  Discussion:
//
//    The explicit form of a line in 3D is:
//
//      the line through P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[DIM_NUM], P2[DIM_NUM], two points on the first line.
//
//    Input, double Q1[DIM_NUM], Q2[DIM_NUM], two points on the second line.
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Output, double LINES_EXP_ANGLE_ND, the angle in radians between the two lines.
//    The angle is computed using the ACOS function, and so lies between 0 and PI.
//    But if one of the lines is degenerate, the angle is returned as -1.0.
//
{
  double angle;
  double ctheta;
  int i;
  double pdotq;
  double pnorm;
  double qnorm;

  pnorm = 0.0;
  for ( i = 0; i < dim_num; i++ )
  {
    pnorm = pnorm + pow ( p2[i] - p1[i], 2 );
  }
  pnorm = sqrt ( pnorm );

  qnorm = 0.0;
  for ( i = 0; i < dim_num; i++ )
  {
    qnorm = qnorm + pow ( q2[i] - q1[i], 2 );
  }
  qnorm = sqrt ( qnorm );

  pdotq = 0.0;
  for ( i = 0; i < dim_num; i++ )
  {
    pdotq = pdotq + ( p2[i] - p1[i] ) * ( q2[i] - q1[i] );
  }

  if ( pnorm == 0.0 || qnorm == 0.0 )
  {
    cerr << "\n";
    cerr << "LINES_EXP_ANGLE_ND - Fatal error!\n";
    cerr << "  One of the lines is degenerate!\n";
    exit ( 1 );
  }
  else
  {
    ctheta = pdotq / ( pnorm * qnorm );
    angle = r8_acos ( ctheta );
  }

  return angle;
}
//****************************************************************************80

double lines_exp_dist_3d ( double p1[3], double p2[3], double q1[3],
  double q2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_EXP_DIST_3D computes the distance between two explicit lines in 3D.
//
//  Discussion:
//
//    The explicit form of a line in 3D is:
//
//      the line through P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], two distinct points on the first line.
//
//    Input, double Q1[3], Q2[3], two distinct points on the second line.
//
//    Output, double LINES_EXP_DIST_3D, the distance between the lines.
//
{
# define DIM_NUM 3

  double a1[DIM_NUM];
  double a2[DIM_NUM];
  double a3[DIM_NUM];
  double bot;
  double *cr;
  double dist;
  double top;
//
//  The distance is found by computing the volume of a parallelipiped,
//  and dividing by the area of its base.
//
//  But if the lines are parallel, we compute the distance by
//  finding the distance between the first line and any point
//  on the second line.
//
  a1[0] = q1[0] - p1[0];
  a1[1] = q1[1] - p1[1];
  a1[2] = q1[2] - p1[2];

  a2[0] = p2[0] - p1[0];
  a2[1] = p2[1] - p1[1];
  a2[2] = p2[2] - p1[2];

  a3[0] = q2[0] - q1[0];
  a3[1] = q2[1] - q1[1];
  a3[2] = q2[2] - q1[2];

  cr = r8vec_cross_product_3d ( a2, a3 );

  bot = r8vec_norm ( 3, cr );

  if ( bot == 0.0 )
  {
    dist = line_exp_point_dist_3d ( p1, p2, q1 );
  }
  else
  {
    top = fabs (  a1[0] * ( a2[1] * a3[2] - a2[2] * a3[1] )
                  - a1[1] * ( a2[0] * a3[2] - a2[2] * a3[0] )
                  + a1[2] * ( a2[0] * a3[1] - a2[1] * a3[0] ) );

    dist = top / bot;

  }

  delete [] cr;

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

double lines_exp_dist_3d_2 ( double p1[3], double p2[3], double q1[3],
  double q2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_EXP_DIST_3D_2 computes the distance between two explicit lines in 3D.
//
//  Discussion:
//
//    The explicit form of a line in 3D is:
//
//      the line through the points P1 and P2.
//
//    This routine uses a method that is essentially independent of dimension.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 August 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], two points on the first line.
//
//    Input, double Q1[3], Q2[3], two points on the second line.
//
//    Output, double LINES_EXP_DIST_3D_2, the distance between the lines.
//
{
# define DIM_NUM 3

  double a;
  double b;
  double c;
  double d;
  double det;
  double dist;
  double e;
  int i;
  double pn[DIM_NUM];
  double qn[DIM_NUM];
  double sn;
  double tn;
  double u[DIM_NUM];
  double v[DIM_NUM];
  double w0[DIM_NUM];
//
//  Let U = (P2-P1) and V = (Q2-Q1) be the direction vectors on
//  the two lines.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    u[i] = p2[i] - p1[i];
  }
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v[i] = q2[i] - q1[i];
  }
//
//  Let SN be the unknown coordinate of the nearest point PN on line 1,
//  so that PN = P(SN) = P1 + SN * (P2-P1).
//
//  Let TN be the unknown coordinate of the nearest point QN on line 2,
//  so that QN = Q(TN) = Q1 + TN * (Q2-Q1).
//
//  Let W0 = (P1-Q1).
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    w0[i] = p1[i] - q1[i];
  }
//
//  The vector direction WC = P(SN) - Q(TC) is unique (among directions)
//  perpendicular to both U and V, so
//
//    U dot WC = 0
//    V dot WC = 0
//
//  or, equivalently:
//
//    U dot ( P1 + SN * (P2 - P1) - Q1 - TN * (Q2 - Q1) ) = 0
//    V dot ( P1 + SN * (P2 - P1) - Q1 - TN * (Q2 - Q1) ) = 0
//
//  or, equivalently:
//
//    (u dot u ) * sn - (u dot v ) tc = -u * w0
//    (v dot u ) * sn - (v dot v ) tc = -v * w0
//
//  or, equivalently:
//
//   ( a  -b ) * ( sn ) = ( -d )
//   ( b  -c )   ( tc )   ( -e )
//
  a = r8vec_dot_product ( DIM_NUM, u, u );
  b = r8vec_dot_product ( DIM_NUM, u, v );
  c = r8vec_dot_product ( DIM_NUM, v, v );
  d = r8vec_dot_product ( DIM_NUM, u, w0 );
  e = r8vec_dot_product ( DIM_NUM, v, w0 );
//
//  Check the determinant.
//
  det = - a * c + b * b;

  if ( det == 0.0 )
  {
    sn = 0.0;
    if ( fabs ( b ) < fabs ( c ) )
    {
      tn = e / c;
    }
    else
    {
      tn = d / b;
    }
  }
  else
  {
    sn = ( c * d - b * e ) / det;
    tn = ( b * d - a * e ) / det;
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    pn[i] = p1[i] + sn * ( p2[i] - p1[i] );
  }
  for ( i = 0; i < DIM_NUM; i++ )
  {
    qn[i] = q1[i] + tn * ( q2[i] - q1[i] );
  }

  dist = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    dist = dist + pow ( pn[i] - qn[i], 2 );
  }
  dist = sqrt ( dist );

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

bool lines_exp_equal_2d ( double p1[2], double p2[2], double q1[2],
  double q2[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_EXP_EQUAL_2D determines if two explicit lines are equal in 2D.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through the points P1 and P2.
//
//    It is essentially impossible to accurately determine whether two
//    explicit lines are equal in 2D.  However, for form's sake, and
//    because occasionally the correct result can be determined, we
//    provide this routine.  Since divisions are avoided, if the
//    input data is exactly representable, the result should be
//    correct.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 July 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], two points on the first line.
//
//    Input, double Q1[2], Q2[2], two points on the second line.
//
//    Output, bool LINES_EXP_EQUAL_2D, is TRUE if the two lines are
//    determined to be identical.
//
{
  double test1;
  double test2;
  double test3;
  double test4;
  bool value;
//
//  Slope (P1,P2) = Slope (P2,Q1).
//
  test1 = ( p2[1] - p1[1] ) * ( q1[0] - p2[0] )
        - ( p2[0] - p1[0] ) * ( q1[1] - p2[1] );

  if ( test1 != 0.0 )
  {
    value = false;
    return value;
  }
//
//  Slope (Q1,Q2) = Slope (P2,Q1).
//
  test2 = ( q2[1] - q1[1] ) * ( q1[0] - p2[0] )
        - ( q2[0] - q1[0] ) * ( q1[1] - p2[1] );

  if ( test2 != 0.0 )
  {
    value = false;
    return value;
  }
//
//  Slope (P1,P2) = Slope (P1,Q2).
//
  test3 = ( p2[1] - p1[1] ) * ( q2[0] - p1[0] )
        - ( p2[0] - p1[0] ) * ( q2[1] - p1[1] );

  if ( test3 != 0.0 )
  {
    value = false;
    return value;
  }
//
//  Slope (Q1,Q2) = Slope (P1,Q2).
//
  test4 = ( q2[1] - q1[1] ) * ( q2[0] - p1[0] )
        - ( q2[0] - q1[0] ) * ( q2[1] - p1[1] );

  if ( test4 != 0.0 )
  {
    value = false;
    return value;
  }

  value = true;

  return value;
}
//****************************************************************************80

void lines_exp_int_2d ( double p1[2], double p2[2], double p3[2], double p4[2],
  int *ival, double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_EXP_INT_2D determines where two explicit lines intersect in 2D.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through P1 and P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], define the first line.
//
//    Input, double P3[2], P4[2], define the second line.
//
//    Output, int *IVAL, reports on the intersection:
//    0, no intersection, the lines may be parallel or degenerate.
//    1, one intersection point, returned in P.
//    2, infinitely many intersections, the lines are identical.
//
//    Output, double P[2], if IVAl = 1, then P contains
//    the intersection point.  Otherwise, P = 0.
//
{
# define DIM_NUM 2

  double a1 = 0.0;
  double a2 = 0.0;
  double b1 = 0.0;
  double b2 = 0.0;
  double c1 = 0.0;
  double c2 = 0.0;
  double point_1 = 0.0;
  double point_2 = 0.0;

  *ival = 0;
  p[0] = 0.0;
  p[1] = 0.0;
//
//  Check whether either line is a point.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) )
  {
    point_1 = true;
  }
  else
  {
    point_1 = false;
  }

  if ( r8vec_eq ( DIM_NUM, p3, p4 ) )
  {
    point_2 = true;
  }
  else
  {
    point_2 = false;
  }
//
//  Convert the lines to ABC format.
//
  if ( !point_1 )
  {
    line_exp2imp_2d ( p1, p2, &a1, &b1, &c1 );
  }

  if ( !point_2 )
  {
    line_exp2imp_2d ( p3, p4, &a2, &b2, &c2 );
  }
//
//  Search for intersection of the lines.
//
  if ( point_1 && point_2 )
  {
    if ( r8vec_eq ( DIM_NUM, p1, p3 ) )
    {
      *ival = 1;
      r8vec_copy ( DIM_NUM, p1, p );
    }
  }
  else if ( point_1 )
  {
    if ( a2 * p1[0] + b2 * p1[1] == c2 )
    {
      *ival = 1;
      r8vec_copy ( DIM_NUM, p1, p );
    }
  }
  else if ( point_2 )
  {
    if ( a1 * p3[0] + b1 * p3[1] == c1 )
    {
      *ival = 1;
      r8vec_copy ( DIM_NUM, p3, p );
    }
  }
  else
  {
    lines_imp_int_2d ( a1, b1, c1, a2, b2, c2, ival, p );
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

void lines_exp_near_3d ( double p1[3], double p2[3], double q1[3],
  double q2[3], double pn[3], double qn[3] )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_EXP_NEAR_3D computes nearest points on two explicit lines in 3D.
//
//  Discussion:
//
//    The explicit form of a line in 3D is:
//
//      the line through the points P1 and P2.
//
//    This routine uses a method that is essentially independent of dimension.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 August 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], two points on the first line.
//
//    Input, double Q1[3], Q2[3], two points on the second line.
//
//    Output, double PN[3], QN[3], the nearest points on the lines.
//
{
# define DIM_NUM 3

  double a;
  double b;
  double c;
  double d;
  double det;
  double e;
  int i;
  double sn;
  double tn;
  double u[DIM_NUM];
  double v[DIM_NUM];
  double w0[DIM_NUM];
//
//  Let U = (P2-P1) and V = (Q2-Q1) be the direction vectors on
//  the two lines.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    u[i] = p2[i] - p1[i];
  }
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v[i] = q2[i] - q1[i];
  }
//
//  Let SN be the unknown coordinate of the nearest point PN on line 1,
//  so that PN = P(SN) = P1 + SN * (P2-P1).
//
//  Let TN be the unknown coordinate of the nearest point QN on line 2,
//  so that QN = Q(TN) = Q1 + TN * (Q2-Q1).
//
//  Let W0 = (P1-Q1).
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    w0[i] = p1[i] - q1[i];
  }
//
//  The vector direction WC = P(SN) - Q(TC) is unique (among directions)
//  perpendicular to both U and V, so
//
//    U dot WC = 0
//    V dot WC = 0
//
//  or, equivalently:
//
//    U dot ( P1 + SN * (P2 - P1) - Q1 - TN * (Q2 - Q1) ) = 0
//    V dot ( P1 + SN * (P2 - P1) - Q1 - TN * (Q2 - Q1) ) = 0
//
//  or, equivalently:
//
//    (u dot u ) * sn - (u dot v ) tc = -u * w0
//    (v dot u ) * sn - (v dot v ) tc = -v * w0
//
//  or, equivalently:
//
//   ( a  -b ) * ( sn ) = ( -d )
//   ( b  -c )   ( tc )   ( -e )
//
  a = r8vec_dot_product ( DIM_NUM, u, u );
  b = r8vec_dot_product ( DIM_NUM, u, v );
  c = r8vec_dot_product ( DIM_NUM, v, v );
  d = r8vec_dot_product ( DIM_NUM, u, w0 );
  e = r8vec_dot_product ( DIM_NUM, v, w0 );
//
//  Check the determinant.
//
  det = - a * c + b * b;

  if ( det == 0.0 )
  {
    sn = 0.0;
    if ( fabs ( b ) < fabs ( c ) )
    {
      tn = e / c;
    }
    else
    {
      tn = d / b;
    }
  }
  else
  {
    sn = ( c * d - b * e ) / det;
    tn = ( b * d - a * e ) / det;
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    pn[i] = p1[i] + sn * ( p2[i] - p1[i] );
  }
  for ( i = 0; i < DIM_NUM; i++ )
  {
    qn[i] = q1[i] + tn * ( q2[i] - q1[i] );
  }
  return;
# undef DIM_NUM
}
//****************************************************************************80

bool lines_exp_parallel_2d ( double p1[2], double p2[2], double q1[2],
  double q2[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_EXP_PARALLEL_2D determines if two lines are parallel in 2D.
//
//  Discussion:
//
//    The explicit form of a line in 2D is:
//
//      the line through P1 and P2.
//
//    The test is essentially a comparison of slopes, but should be
//    more accurate than an explicit slope comparison, and unfazed
//    by degenerate cases.
//
//    On the other hand, there is NO tolerance for error.  If the
//    slopes differ by a single digit in the last place, then the
//    lines are judged to be nonparallel.  A more robust test would
//    be to compute the angle between the lines, because then it makes
//    sense to say the lines are "almost" parallel: the angle is small.
//
//    If the lines are determined to be parallel, then you can
//    determine whether they are identical or distinct by evaluating:
//
//      lines_exp_parallel_2d ( p1, q2, q1, p2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], define the first line.
//
//    Input, double Q1[2], Q2[2], define the second line.
//
//    Output, bool LINES_EXP_PARALLEL_2D is TRUE if the lines are parallel.
//
{
  bool value;

  value = ( ( p2[1] - p1[1] ) * ( q2[0] - q1[0] ) ==
            ( q2[1] - q1[1] ) * ( p2[0] - p1[0] ) );

  return value;
}
//****************************************************************************80

bool lines_exp_parallel_3d ( double p1[3], double p2[3], double q1[3],
  double q2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_EXP_PARALLEL_3D determines if two lines are parallel in 3D.
//
//  Discussion:
//
//    The explicit form of a line in 3D is:
//
//      the line through P1 and P2.
//
//    The points P1, P2 define a direction (P2-P1).  Similarly, the
//    points (Q1,Q2) define a direction (Q2-Q1).  The quantity
//
//      (P2-P1) dot (Q2-Q1) = norm(P2-P1) * norm(Q2-Q1) * cos ( angle )
//
//    Therefore, the following value is between 0 and 1;
//
//      abs ( (P2-P1) dot (Q2-Q1) / ( norm(P2-P1) * norm(Q2-Q1) ) )
//
//    and the lines are parallel if
//
//      abs ( (P2-P1) dot (Q2-Q1) / ( norm(P2-P1) * norm(Q2-Q1) ) ) = 1
//
//    We can rephrase this as requiring:
//
//      ( (P2-P1)dot(Q2-Q1) )^2 = (P2-P1)dot(P2-P1) * (Q2-Q1)dot(Q2-Q1)
//
//    which avoids division and square roots.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 August 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], define the first line.
//
//    Input, double Q1[3], Q2[3, define the second line.
//
//    Output, bool LINES_EXP_PARALLEL_3D is TRUE if the lines are parallel.
//
{
# define DIM_NUM 3

  bool value;

  int i;
  double *p;
  double pdotp;
  double pdotq;
  double *q;
  double qdotq;

  p = new double[DIM_NUM];
  q = new double[DIM_NUM];

  for ( i = 0; i < DIM_NUM; i++ )
  {
    p[i] = p2[i] - p1[i];
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    q[i] = q2[i] - q1[i];
  }

  pdotq = r8vec_dot_product ( DIM_NUM, p, q );
  pdotp = r8vec_dot_product ( DIM_NUM, p, p );
  qdotq = r8vec_dot_product ( DIM_NUM, q, q );

  delete [] p;
  delete [] q;

  value = ( pdotq * pdotq == pdotp * qdotq );

  return value;
# undef DIM_NUM
}
//****************************************************************************80

double lines_imp_angle_2d ( double a1, double b1, double c1,
  double a2, double b2, double c2 )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_IMP_ANGLE_2D finds the angle between two implicit lines in 2D.
//
//  Discussion:
//
//    The implicit form of a line in 2D is:
//
//      A * X + B * Y + C = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 June 1999
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double A1, B1, C1, the implicit parameters of the first line.
//
//    Input, double A2, B2, C2, the implicit parameters of the second line.
//
//    Output, double LINES_IMP_ANGLE_2D, the angle between the two lines.
//
{
  double ctheta;
  double pdotq;
  double pnorm;
  double qnorm;
  double theta;

  pdotq = a1 * a2 + b1 * b2;
  pnorm = sqrt ( a1 * a1 + b1 * b1 );
  qnorm = sqrt ( a2 * a2 + b2 * b2 );

  ctheta = pdotq / ( pnorm * qnorm );

  theta = acos ( ctheta );

  return theta;
}
//****************************************************************************80

double lines_imp_dist_2d ( double a1, double b1, double c1, double a2,
  double b2, double c2 )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_IMP_DIST_2D determines the distance between two implicit lines in 2D.
//
//  Discussion:
//
//    If the lines are not parallel, then they must intersect, so their
//    distance is zero.
//
//    If the two lines are parallel, then they may have a nonzero distance.
//
//    The implicit form of a line in 2D is:
//
//      A * X + B * Y + C = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A1, B1, C1, define the first line.
//    At least one of A1 and B1 must be nonzero.
//
//    Input, double A2, B2, C2, define the second line.
//    At least one of A2 and B2 must be nonzero.
//
//    Output, double LINES_IMP_DIST_2D, the distance between the two lines.
//
{
  double value;
//
//  Refuse to handle degenerate lines.
//
  if ( a1 == 0.0 && b1 == 0.0 )
  {
    cerr << "\n";
    cerr << "LINES_IMP_DIST_2D - Fatal error!\n";
    cerr << "  Line 1 is degenerate.\n";
    exit ( 1 );
  }

  if ( a2 == 0.0 && b2 == 0.0 )
  {
    cerr << "\n";
    cerr << "LINES_IMP_DIST_2D - Fatal error!\n";
    cerr << "  Line 2 is degenerate.\n";
    exit ( 1 );
  }
//
//  If the lines are not parallel, they intersect, and have distance 0.
//
  if ( a1 * b2 != a2 * b1 )
  {
    value = 0.0;
    return value;
  }
//
//  Determine the distance between the parallel lines.
//
  value = fabs ( c2 / sqrt ( a2 * a2 + b2 * b2 )
                 - c1 / sqrt ( a1 * a1 + b1 * b1 ) );

  return value;
}
//****************************************************************************80

void lines_imp_int_2d ( double a1, double b1, double c1, double a2, double b2,
  double c2, int *ival, double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_IMP_INT_2D determines where two implicit lines intersect in 2D.
//
//  Discussion:
//
//    The implicit form of a line in 2D is:
//
//      A * X + B * Y + C = 0
//
//    22 May 2004: Thanks to John Asmuth for pointing out that the
//    B array was not being deallocated on exit.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A1, B1, C1, define the first line.
//    At least one of A1 and B1 must be nonzero.
//
//    Input, double A2, B2, C2, define the second line.
//    At least one of A2 and B2 must be nonzero.
//
//    Output, int *IVAL, reports on the intersection.
//    -1, both A1 and B1 were zero.
//    -2, both A2 and B2 were zero.
//     0, no intersection, the lines are parallel.
//     1, one intersection point, returned in P.
//     2, infinitely many intersections, the lines are identical.
//
//    Output, double P[2], if IVAL = 1, then P contains
//    the intersection point.  Otherwise, P = 0.
//
{
# define DIM_NUM 2

  double a[DIM_NUM*2];
  double *b;

  p[0] = 0.0;
  p[1] = 0.0;
//
//  Refuse to handle degenerate lines.
//
  if ( a1 == 0.0 && b1 == 0.0 )
  {
    *ival = - 1;
    return;
  }
  else if ( a2 == 0.0 && b2 == 0.0 )
  {
    *ival = - 2;
    return;
  }
//
//  Set up a linear system, and compute its inverse.
//
  a[0+0*2] = a1;
  a[0+1*2] = b1;
  a[1+0*2] = a2;
  a[1+1*2] = b2;

  b = r8mat_inverse_2d ( a );
//
//  If the inverse exists, then the lines intersect.
//  Multiply the inverse times -C to get the intersection point.
//
  if ( b != NULL )
  {

    *ival = 1;
    p[0] = - b[0+0*2] * c1 - b[0+1*2] * c2;
    p[1] = - b[1+0*2] * c1 - b[1+1*2] * c2;
  }
//
//  If the inverse does not exist, then the lines are parallel
//  or coincident.  Check for parallelism by seeing if the
//  C entries are in the same ratio as the A or B entries.
//
  else
  {

    *ival = 0;

    if ( a1 == 0.0 )
    {
      if ( b2 * c1 == c2 * b1 )
      {
        *ival = 2;
      }
    }
    else
    {
      if ( a2 * c1 == c2 * a1 )
      {
        *ival = 2;
      }
    }
  }

  delete [] b;

  return;
# undef DIM_NUM
}
//****************************************************************************80

double lines_par_angle_2d ( double f1, double g1, double x01, double y01,
  double f2, double g2, double x02, double y02 )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_PAR_ANGLE_2D finds the angle between two parametric lines in 2D.
//
//  Discussion:
//
//    The parametric form of a line in 2D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//
//    For normalization, we choose F*F+G*G = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double F1, G1, X01, Y01, the parametric parameters of the
//    first line.
//
//    Input, double F2, G2, X02, Y02, the parametric parameters of the
//    second line.
//
//    Output, double LINES_PAR_ANGLE_2D, the angle between the two lines.
//
{
  double pdotq;
  double pnorm;
  double qnorm;
  double value;

  pdotq =        f1 * f2 + g1 * g2;
  pnorm = sqrt ( f1 * f1 + g1 * g1 );
  qnorm = sqrt ( f2 * f2 + g2 * g2 );

  value = r8_acos ( pdotq / ( pnorm * qnorm ) );

  return value;
}
//****************************************************************************80

double lines_par_angle_3d ( double f1, double g1, double h1, double x01,
  double y01, double z01, double f2, double g2, double h2, double x02,
  double y02, double z02 )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_PAR_ANGLE_3D finds the angle between two parametric lines in 3D.
//
//  Discussion:
//
//    The parametric form of a line in 3D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//      Z = Z0 + H * T
//
//    For normalization, we choose F*F+G*G+H*H = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double F1, G1, H1, X01, Y01, Z01, the parametric parameters
//    of the first line.
//
//    Input, double F2, G2, H2, X02, Y02, Z02, the parametric parameters
//    of the second line.
//
//    Output, double LINES_PAR_ANGLE_3D, the angle between the two lines.
//
{
  double pdotq;
  double pnorm;
  double qnorm;
  double value;

  pdotq =        f1 * f2 + g1 * g2 + h1 * h2;
  pnorm = sqrt ( f1 * f1 + g1 * g1 + h1 * h1 );
  qnorm = sqrt ( f2 * f2 + g2 * g2 + h2 * h2 );

  value = r8_acos ( pdotq / ( pnorm * qnorm ) );

  return value;
}
//****************************************************************************80

double lines_par_dist_3d ( double f1, double g1, double h1, double x01,
  double y01, double z01, double f2, double g2, double h2, double x02,
  double y02, double z02 )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_PAR_DIST_3D finds the distance between two parametric lines in 3D.
//
//  Discussion:
//
//    The parametric form of a line in 3D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//      Z = Z0 + H * T
//
//    For normalization, we choose F*F+G*G+H*H = 1 and 0 <= F.
//
//    This code does not work for parallel or near parallel lines.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double F1, G1, H1, X01, Y01, Z01, the parametric parameters
//    of the first line.
//
//    Input, double F2, G2, H2, X02, Y02, Z02, the parametric parameters
//    of the second line.
//
//    Output, double LINES_PAR_DIST_3D, the distance between the two lines.
//
{
  double value;

  value = fabs ( ( x02 - x01 ) * ( g1 * h2 - g2 * h1 )
             + ( y02 - y01 ) * ( h1 * f2 - h2 * f1 )
             + ( z02 - z01 ) * ( f1 * g2 - f2 * g1 ) )  /
             ( ( f1 * g2 - f2 * g1 ) * ( f1 * g2 - f2 * g1 )
             + ( g1 * h2 - g2 * h1 ) * ( g1 * h2 - g2 * h1 )
             + ( h1 * f2 - h2 * f1 ) * ( h1 * f2 - h2 * f1 ) );

  return value;
}
//****************************************************************************80

void lines_par_int_2d ( double f1, double g1, double x1, double y1, double f2,
  double g2, double x2, double y2, double *t1, double *t2, double pint[2] )

//****************************************************************************80
//
//  Purpose:
//
//    LINES_PAR_INT_2D determines where two parametric lines intersect in 2D.
//
//  Discussion:
//
//    The parametric form of a line in 2D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//
//    For normalization, we choose F*F+G*G = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double F1, G1, X1, Y1, define the first parametric line.
//
//    Input, double F2, G2, X2, Y2, define the second parametric line.
//
//    Output, double *T1, *T2, the T parameters on the first and second
//    lines of the intersection point.
//
//    Output, double PINT[2], the intersection point.
//
{
  double det;

  det = f2 * g1 - f1 * g2;

  if ( det == 0.0 )
  {
    *t1 = 0.0;
    *t2 = 0.0;
    r8vec_zero ( 2, pint );
  }
  else
  {
    *t1 = ( f2 * ( y2 - y1 ) - g2 * ( x2 - x1 ) ) / det;
    *t2 = ( f1 * ( y2 - y1 ) - g1 * ( x2 - x1 ) ) / det;
    pint[0] = x1 + f1 * (*t1);
    pint[1] = y1 + g1 * (*t1);
  }

  return;
}
//****************************************************************************80

void loc2glob_3d ( double cospitch, double cosroll, double cosyaw,
  double sinpitch, double sinroll, double sinyaw, double locpts[3],
  double globas[3], double glopts[3] )

//****************************************************************************80
//
//  Purpose:
//
//    LOC2GLOB_3D converts from a local to global coordinate system in 3D.
//
//  Discussion:
//
//    A global coordinate system is given.
//
//    A local coordinate system has been translated to the point with
//    global coordinates GLOBAS, and rotated through a yaw, a pitch, and
//    a roll.
//
//    A point has local coordinates LOCPTS, and it is desired to know
//    the point's global coordinates GLOPTS.
//
//    The transformation may be written as
//
//      GLOB = GLOBAS + N_YAW * N_PITCH * N_ROLL * LOC
//
//    where
//
//               (  cos(Yaw)   -sin(Yaw)        0      )
//    N_YAW    = (  sin(Yaw)    cos(Yaw)        0      )
//               (      0           0           1      )
//
//               (  cos(Pitch)      0       sin(Pitch) )
//    N_PITCH =  (      0           1           0      )
//               ( -sin(Pitch)      0       cos(Pitch) )
//
//               (      1           0           0      )
//    N_ROLL =   (      0       cos(Roll)  -sin(Roll)  )
//               (      0       sin(Roll)   cos(Roll)  )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double COSPITCH, COSROLL, COSYAW, the cosines of the pitch,
//    roll and yaw angles.
//
//    Input, double SINPITCH, SINROLL, SINYAW, the sines of the pitch,
//    roll and yaw angles.
//
//    Input, double LOCPTS[3], the local coordinates of the point.
//
//    Input, double GLOBAS[3], the global coordinates of the base vector.
//
//    Output, double GLOPTS[3], the global coordinates of the point.
//
{
  glopts[0] = globas[0] + (  cosyaw * cospitch ) * locpts[0]
    + (  cosyaw * sinpitch * sinroll - sinyaw * cosroll ) * locpts[1]
    + (  cosyaw * sinpitch * cosroll + sinyaw * sinroll ) * locpts[2];

  glopts[1] = globas[1] + (  sinyaw * cospitch ) * locpts[0]
    + (  sinyaw * sinpitch * sinroll + cosyaw * cosroll ) * locpts[1]
    + (  sinyaw * sinpitch * cosroll - cosyaw * sinroll ) * locpts[2];

  glopts[2] = globas[2] + ( -sinpitch ) * locpts[0]
    + (  cospitch * sinroll ) * locpts[1]
    + (  cospitch * cosroll ) * locpts[2];

  return;
}
//****************************************************************************80

void lvec_print ( int n, bool a[], string title )

//****************************************************************************80
//
//  Purpose:
//
//    LVEC_PRINT prints a logical vector.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 April 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of components of the vector.
//
//    Input, bool A[N], the vector to be printed.
//
//    Input, string TITLE, a title.
//
{
  int i;

  cout << "\n";
  cout << title << "\n";
  cout << "\n";
  for ( i = 0; i <= n-1; i++ )
  {
    cout << setw(6) << i << ": " << setw(1) << a[i] << "\n";
  }

  return;
}
//****************************************************************************80

void minabs ( double x1, double y1, double x2, double y2, double x3, double y3,
  double *xmin, double *ymin )

//****************************************************************************80
//
//  Purpose:
//
//    MINABS finds a local minimum of F(X) = A * abs ( X ) + B.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, double X1, Y1, X2, Y2, X3, Y3, are three sets of data
//    of the form ( X, F(X) ).  The three X values must be distinct.
//    On output, the data has been sorted so that X1 < X2 < X3,
//    and the Y values have been rearranged accordingly.
//
//    Output, double *XMIN, *YMIN.  XMIN is a point within the interval
//    spanned by X1, X2 and X3, at which F takes its local minimum
//    value YMIN.
//
{
  double slope;
  double slope12;
  double slope13;
  double slope23;
//
//  Refuse to deal with coincident data.
//
  if ( x1 == x2 || x2 == x3 || x3 == x1 )
  {
    cerr << "\n";
    cerr << "MINABS - Fatal error!\n";
    cerr << "  X values are equal.\n";
    exit ( 1 );
  }
//
//  Sort the data.
//
  if ( x2 < x1 )
  {
    r8_swap ( &x1, &x2 );
    r8_swap ( &y1, &y2 );
  }

  if ( x3 < x1 )
  {
    r8_swap ( &x1, &x3 );
    r8_swap ( &y1, &y3 );
  }

  if ( x3 < x2 )
  {
    r8_swap ( &x2, &x3 );
    r8_swap ( &y2, &y3 );
  }
//
//  Now determine the slopes.
//
  slope12 = ( y2 - y1 ) / ( x2 - x1 );
  slope23 = ( y3 - y2 ) / ( x3 - x2 );
  slope13 = ( y3 - y1 ) / ( x3 - x1 );
//
//  Case 1: Minimum must be at an endpoint.
//
  if ( slope13 <= slope12 || 0.0 <= slope12 )
  {
    if ( y1 < y3 )
    {
      *xmin = x1;
      *ymin = y1;
    }
    else
    {
      *xmin = x3;
      *ymin = y3;
    }
  }
//
//  Case 2: The curve decreases, and decreases faster than the line
//  joining the endpoints.
//
//  Whichever of SLOPE12 and SLOPE23 is the greater in magnitude
//  represents the actual slope of the underlying function.
//  Find where two lines of that slope, passing through the
//  endpoint data, intersect.
//
  else
  {
    slope = fmax ( fabs ( slope12 ), slope23 );
    *xmin = 0.5 * ( x1 + x3 + ( y1 - y3 ) / slope );
    *ymin = y1 - slope * ( (*xmin) - x1 );
  }

  return;
}
//****************************************************************************80

bool minquad ( double x1, double y1, double x2, double y2, double x3, double y3,
  double *xmin, double *ymin )

//****************************************************************************80
//
//  Purpose:
//
//    MINQUAD finds a local minimum of F(X) = A * X^2 + B * X + C.
//
//  Discussion:
//
//    MINQUAD is primarily intended as a utility routine for use by
//    DISLSLS3.  The square of the distance function between a point
//    and a line segment has the form of F(X).  Hence, we can seek
//    the line on the second segment which minimizes the square of
//    the distance to the other line segment.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 November 1998
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X1, Y1, X2, Y2, X3, Y3, are three sets of data
//    of the form ( X, F(X) ).  The three X values must be distinct.
//
//    Output, double *XMIN, *YMIN.  XMIN is a point within the interval
//    spanned by X1, X2 and X3, at which F takes its local minimum
//    value YMIN.
//
//    Output, bool MINQUAD,
//    true if no error,
//    false if error because X values are not distinct.
//
{
  int ierror;
  double x;
  double xleft;
  double xrite;
  double y;

  *xmin = 0.0;
  *ymin = 0.0;
//
//  Refuse to deal with coincident data.
//
  if ( x1 == x2 || x2 == x3 || x3 == x1 )
  {
    return false;
  }
//
//  Find the interval endpoints.
//
  xleft = x1;
  if ( x2 < xleft )
  {
    xleft = x2;
  }
  if ( x3 < xleft )
  {
    xleft = x3;
  }
  xrite = x1;
  if ( xrite < x2 )
  {
    xrite = x2;
  }
  if ( xrite < x3 )
  {
    xrite = x3;
  }
//
//  Find the minimizer and its function value over the three input points.
//
  if ( y1 <= y2 && y1 <= y3 )
  {
    *xmin = x1;
    *ymin = y1;
  }
  else if ( y2 <= y1 && y2 <= y3 )
  {
    *xmin = x2;
    *ymin = y2;
  }
  else if ( y3 <= y1 && y3 <= y2 )
  {
    *xmin = x3;
    *ymin = y3;
  }
//
//  Find the minimizer and its function value over the real line.
//
  ierror = parabola_ex ( x1, y1, x2, y2, x3, y3, &x, &y );

  if ( ierror != 2 && y < *ymin && xleft < x && x < xrite )
  {
    *xmin = x;
    *ymin = y;
  }

  return true;
}
//****************************************************************************80

void octahedron_shape_3d ( int point_num, int face_num, int face_order_max,
  double point_coord[], int face_order[], int face_point[] )

//****************************************************************************80
//
//  Purpose:
//
//    OCTAHEDRON_SHAPE_3D describes an octahedron in 3D.
//
//  Discussion:
//
//    The vertices lie on the unit sphere.
//
//    The dual of the octahedron is the cube.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int POINT_NUM, the number of points.
//
//    Input, int FACE_NUM, the number of faces.
//
//    Input, int FACE_ORDER_MAX, the maximum number of vertices
//    per face.
//
//    Output, double POINT_COORD[3*POINT_NUM], the point coordinates.
//
//    Output, int FACE_ORDER[FACE_NUM], the number of vertices per face.
//
//    Output, int FACE_POINT[FACE_ORDER_MAX*FACE_NUM]; FACE_POINT(I,J)
//    contains the index of the I-th point in the J-th face.  The
//    points are listed in the counter clockwise direction defined
//    by the outward normal at the face.
//
{
# define DIM_NUM 3

  static int face_order_save[8] = {
    3, 3, 3, 3, 3, 3, 3, 3 };
  static int face_point_save[3*8] = {
     1, 3, 2,
     1, 4, 3,
     1, 5, 4,
     1, 2, 5,
     2, 3, 6,
     3, 4, 6,
     4, 5, 6,
     5, 2, 6 };
  static double point_coord_save[DIM_NUM*6] = {
     0.0,  0.0, -1.0,
     0.0, -1.0,  0.0,
     1.0,  0.0,  0.0,
     0.0,  1.0,  0.0,
    -1.0,  0.0,  0.0,
     0.0,  0.0,  1.0 };

  i4vec_copy ( face_num, face_order_save, face_order );
  i4vec_copy ( face_order_max*face_num, face_point_save, face_point );
  r8vec_copy ( DIM_NUM*point_num, point_coord_save, point_coord );

  return;
# undef DIM_NUM
}
//****************************************************************************80

void octahedron_size_3d ( int *point_num, int *edge_num, int *face_num,
  int *face_order_max )

//****************************************************************************80
//
//  Purpose:
//
//    OCTAHEDRON_SIZE_3D returns size information for an octahedron in 3D.
//
//  Discussion:
//
//    This routine can be called before calling OCTAHEDRON_SHAPE_3D,
//    so that space can be allocated for the arrays.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 July 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, int *POINT_NUM, the number of points.
//
//    Output, int *EDGE_NUM, the number of edges.
//
//    Output, int *FACE_NUM, the number of faces.
//
//    Output, int *FACE_ORDER_MAX, the maximum number of vertices
//    per face.
//
{
  *point_num = 6;
  *edge_num = 12;
  *face_num = 8;
  *face_order_max = 3;

  return;
}
//****************************************************************************80

int parabola_ex ( double x1, double y1, double x2, double y2, double x3,
  double y3, double *x, double *y )

//****************************************************************************80
//
//  Purpose:
//
//    PARABOLA_EX finds the extremal point of a parabola determined by three points.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X1, Y1, X2, Y2, X3, Y3, the coordinates of three points
//    on the parabola.  X1, X2 and X3 must be distinct.
//
//    Output, double *X, *Y, the X coordinate of the extremal point of the
//    parabola, and the value of the parabola at that point.
//
//    Output, int PARABOLA_EX, error flag.
//    0, no error.
//    1, two of the X values are equal.
//    2, the data lies on a straight line; there is no finite extremal
//    point.
//    3, the data lies on a horizontal line; every point is "extremal".
//
{
  double bot;

  *x = 0.0;
  *y = 0.0;

  if ( x1 == x2 || x2 == x3 || x3 == x1 )
  {
    return 1;
  }

  if ( y1 == y2 && y2 == y3 && y3 == y1 )
  {
    *x = x1;
    *y = y1;
    return 3;
  }

  bot = ( x2 - x3 ) * y1 - ( x1 - x3 ) * y2 + ( x1 - x2 ) * y3;

  if ( bot == 0.0 )
  {
    return 2;
  }

  *x = 0.5 * (
      x1 * x1 * ( y3 - y2 )
    + x2 * x2 * ( y1 - y3 )
    + x3 * x3 * ( y2 - y1 ) ) / bot;

  *y =  (
      ( *x - x2 ) * ( *x - x3 ) * ( x2 - x3 ) * y1
    - ( *x - x1 ) * ( *x - x3 ) * ( x1 - x3 ) * y2
    + ( *x - x1 ) * ( *x - x2 ) * ( x1 - x2 ) * y3 ) /
    ( ( x1 - x2 ) * ( x2 - x3 ) * ( x1 - x3 ) );

  return 0;
}
//****************************************************************************80

int parabola_ex2 ( double x1, double y1, double x2, double y2, double x3,
  double y3, double *x, double *y, double *a, double *b, double *c )

//****************************************************************************80
//
//  Purpose:
//
//    PARABOLA_EX2 finds the extremal point of a parabola determined by three points.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 May 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X1, Y1, X2, Y2, X3, Y3, the coordinates of three points
//    on the parabola.  X1, X2 and X3 must be distinct.
//
//    Output, double *X, *Y, the X coordinate of the extremal point of the
//    parabola, and the value of the parabola at that point.
//
//    Output, double *A, *B, *C, the coefficients that define the parabola:
//    P(X) = A * X * X + B * X + C.
//
//    Output, int PARABOLA_EX2, error flag.
//    0, no error.
//    1, two of the X values are equal.
//    2, the data lies on a straight line; there is no finite extremal
//    point.
//    3, the data lies on a horizontal line; any point is an "extremal point".
//
{
  double v[3*3];
  double *w;

  *a = 0.0;
  *b = 0.0;
  *c = 0.0;
  *x = 0.0;
  *y = 0.0;

  if ( x1 == x2 || x2 == x3 || x3 == x1 )
  {
    return 1;
  }

  if ( y1 == y2 && y2 == y3 && y3 == y1 )
  {
    *x = x1;
    *y = y1;
    return 3;
  }
//
//  Set up the Vandermonde matrix.
//
  v[0+0*3] = 1.0;
  v[1+0*3] = 1.0;
  v[2+0*3] = 1.0;

  v[0+1*3] = x1;
  v[1+1*3] = x2;
  v[2+1*3] = x3;

  v[0+2*3] = x1 * x1;
  v[1+2*3] = x2 * x2;
  v[2+2*3] = x3 * x3;
//
//  Get the inverse.
//
  w = r8mat_inverse_3d ( v );
//
//  Compute the parabolic coefficients.
//
  *c = w[0+0*3] * y1 + w[0+1*3] * y2 + w[0+2*3] * y3;
  *b = w[1+0*3] * y1 + w[1+1*3] * y2 + w[1+2*3] * y3;
  *a = w[2+0*3] * y1 + w[2+1*3] * y2 + w[2+2*3] * y3;

  delete [] w;
//
//  Determine the extremal point.
//
  if ( *a == 0.0 )
  {
    return 2;
  }

  *x = - *b / ( 2.0 * *a );
  *y = *a * *x * *x + *b * *x + *c;

  return 0;
}
//****************************************************************************80

double parallelogram_area_2d ( double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    parallelogram_area_2d() computes the area of a parallelogram in 2D.
//
//  Discussion:
//
//    A parallelogram is a polygon having four sides, with the property
//    that each pair of opposite sides is paralell.
//
//    Given the first three vertices of the parallelogram,
//    P1, P2, and P3, the fourth vertex must satisfy
//
//      P4 = P1 + ( P3 - P2 )
//
//    This routine uses the fact that the norm of the cross product
//    of two vectors is the area of the parallelogram they form:
//
//      Area = ( P3 - P2 ) x ( P1 - P2 ).
//
//        P4<-----P3
//        /       /
//       /       /
//      P1----->P2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P[2*4], the parallelogram vertices,
//    given in counterclockwise order.  The fourth vertex is ignored.
//
//    Output, double PARALLELOGRAM_AREA_2D, the (signed) area.
//
{
  double area;
//
//  Compute the cross product vector, which only has a single
//  nonzero component.
//
  area = ( p[0+1*2] - p[0+0*2] ) * ( p[1+2*2] - p[1+0*2] )
       - ( p[1+1*2] - p[1+0*2] ) * ( p[0+2*2] - p[0+0*2] );

  return area;
}
//****************************************************************************80

double parallelogram_area_3d ( double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    parallelogram_area_3d() computes the area of a parallelogram in 3D.
//
//  Discussion:
//
//    A parallelogram is a polygon having four sides, with the property
//    that each pair of opposite sides is paralell.
//
//    A parallelogram in 3D must have the property that it is "really"
//    a 2D object, that is, that the four vertices that define it lie
//    in some plane.
//
//    Given the first three vertices of the parallelogram (in 2D or 3D),
//    P1, P2, and P3, the fourth vertex must satisfy
//
//      P4 = P1 + ( P3 - P2 )
//
//    This routine uses the fact that the norm of the cross product
//    of two vectors is the area of the parallelogram they form:
//
//      Area = ( P3 - P2 ) x ( P1 - P2 ).
//
//        P4<-----P3
//        /       /
//       /       /
//      P1----->P2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P[3*4], the parallelogram vertices,
//    given in counterclockwise order.  The fourth vertex is ignored.
//
//    Output, double PARALLELOGRAM_AREA_3D, the area
//
{
  double area;
  double cross;
//
//  Compute the cross product vector.
//
  area = 0.0;

  cross = ( p[1+1*3] - p[1+0*3] ) * ( p[2+2*3] - p[2+0*3] )
        - ( p[2+1*3] - p[2+0*3] ) * ( p[1+2*3] - p[1+0*3] );

  area = area + cross * cross;

  cross = ( p[2+1*3] - p[2+0*3] ) * ( p[0+2*3] - p[0+0*3] )
        - ( p[0+1*3] - p[0+0*3] ) * ( p[2+2*3] - p[2+0*3] );

  area = area + cross * cross;

  cross = ( p[0+1*3] - p[0+0*3] ) * ( p[1+2*3] - p[1+0*3] )
        - ( p[1+1*3] - p[1+0*3] ) * ( p[0+2*3] - p[0+0*3] );

  area = area + cross * cross;

  area = sqrt ( area );

  return area;
}
//****************************************************************************80

bool parallelogram_contains_point_2d ( double p1[2], double p2[2], double p3[2],
  double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    PARALLELOGRAM_CONTAINS_POINT_2D determines if a point is inside a parallelogram in 2D.
//
//  Discussion:
//
//         P2..............
//        /              .
//       /              .
//      /              .
//    P1------------->P3
//
//    The algorithm used here essentially computes the barycentric
//    coordinates of the point P, and accepts it if both coordinates
//    are between 0 and 1.  ( For a triangle, they must be positive,
//    and sum to no more than 1.)  The same trick works for a parallelepiped.
//
//    05 August 2005: Thanks to Gernot Grabmair for pointing out that a previous
//    version of this routine was incorrect.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], P3[2], three vertices of the parallelogram.
//    P1 should be directly connected to P2 and P3.
//
//    Input, double P[2], the point to be checked.
//
//    Output, bool PARALLELOGRAM_CONTAINS_POINT_2D is TRUE if P is inside the
//    parallelogram, or on its boundary, and FALSE otherwise.
//
{
# define DIM_NUM 2

  double a[DIM_NUM*(DIM_NUM+1)];
  int info;
  bool value;
//
//  Set up the linear system
//
//    ( X2-X1  X3-X1 ) C1  = X-X1
//    ( Y2-Y1  Y3-Y1 ) C2    Y-Y1
//
//  which is satisfied by the barycentric coordinates.
//
  a[0+0*DIM_NUM] = p2[0] - p1[0];
  a[1+0*DIM_NUM] = p2[1] - p1[1];

  a[0+1*DIM_NUM] = p3[0] - p1[0];
  a[1+1*DIM_NUM] = p3[1] - p1[1];

  a[0+2*DIM_NUM] = p[0] - p1[0];
  a[1+2*DIM_NUM] = p[1] - p1[1];
//
//  Solve the linear system.
//
  info = r8mat_solve ( DIM_NUM, 1, a );

  if ( info != 0 )
  {
    cerr << "\n";
    cerr << "PARALLELOGRAM_CONTAINS_POINT_2D - Fatal error!\n";
    cerr << "  The linear system is singular.\n";
    cerr << "  The input data does not form a proper triangle.\n";
    exit ( 1 );
  }

  if ( a[0+2*DIM_NUM] < 0.0 || 1.0 < a[0+2*DIM_NUM] )
  {
    value = false;
  }
  else if ( a[1+2*DIM_NUM] < 0.0 || 1.0 < a[1+2*DIM_NUM] )
  {
    value = false;
  }
  else
  {
    value = true;
  }

  return value;
# undef DIM_NUM
}
//****************************************************************************80

bool parallelogram_contains_point_3d ( double p1[3], double p2[3], double p3[3],
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PARALLELOGRAM_CONTAINS_POINT_3D determines if a point is inside a parallelogram in 3D.
//
//  Diagram:
//
//         P2..............
//        /              .
//       /              .
//      /              .
//    P1------------->P3
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 February 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], three vertices of the parallelogram.
//
//    Input, double P[3], the point to be checked.
//
//    Output, int PARALLELOGRAM_CONTAINS_POINT_3D, is true if P is inside the
//    parallelogram, or on its boundary, and false otherwise.
//    A slight amount of leeway is allowed for error, since a three
//    dimensional point may lie exactly in the plane of the parallelogram,
//    and yet be computationally slightly outside it.
//
{
# define DIM_NUM 3
# define TOL 0.00001

  double dot;
  double dotb;
  double dott;
  double v;
  double p21[DIM_NUM];
  double p31[DIM_NUM];
  double pn12[DIM_NUM];
  double *pn23;
  double *pn31;
//
//  Compute V3, the vector normal to V1 = P2-P1 and V2 = P3-P1.
//
  pn12[0] = ( p2[1] - p1[1] ) * ( p3[2] - p1[2] )
          - ( p2[2] - p1[2] ) * ( p3[1] - p1[1] );

  pn12[1] = ( p2[2] - p1[2] ) * ( p3[0] - p1[0] )
          - ( p2[0] - p1[0] ) * ( p3[2] - p1[2] );

  pn12[2] = ( p2[0] - p1[0] ) * ( p3[1] - p1[1] )
          - ( p2[1] - p1[1] ) * ( p3[0] - p1[0] );
//
//  If the component of V = P-P1 in the V3 direction is too large,
//  then it does not lie in the parallelogram.
//
  dot = ( p[0] - p1[0] ) * pn12[0]
      + ( p[1] - p1[1] ) * pn12[1]
      + ( p[2] - p1[2] ) * pn12[2];

  v = sqrt ( pow ( p2[0] - p[0], 2 )
           + pow ( p2[1] - p[1], 2 )
           + pow ( p2[2] - p[2], 2 ) );

  if ( TOL * ( 1.0 + v ) < fabs ( dot ) )
  {
    return false;
  }
//
//  Compute V23, the vector normal to V2 and V3.
//
  p31[0] = p3[0] - p1[0];
  p31[1] = p3[1] - p1[1];
  p31[2] = p3[2] - p1[2];

  pn23 = r8vec_cross_product_3d ( p31, pn12 );
//
//  Compute ALPHA = ( V dot V23 ) / ( V1 dot V23 )
//
  dott = ( p[0] - p1[0] ) * pn23[0]
       + ( p[1] - p1[1] ) * pn23[1]
       + ( p[2] - p1[2] ) * pn23[2];

  dotb =
    ( p2[0] - p1[0] ) * pn23[0] +
    ( p2[1] - p1[1] ) * pn23[1] +
    ( p2[2] - p1[2] ) * pn23[2];

  delete [] pn23;

  if ( dotb < 0.0 )
  {
    dott = -dott;
    dotb = -dotb;
  }

  if ( dott < 0.0 || dotb < dott )
  {
    return false;
  }
//
//  Compute V31, the vector normal to V3 and V1.
//
  p21[0] = p2[0] - p1[0];
  p21[1] = p2[1] - p1[1];
  p21[2] = p2[2] - p1[2];

  pn31 = r8vec_cross_product_3d ( pn12, p21 );
//
//  Compute BETA = ( V dot V31 ) / ( V2 dot V31 )
//
  dott = ( p[0] - p1[0] ) * pn31[0]
       + ( p[1] - p1[1] ) * pn31[1]
       + ( p[2] - p1[2] ) * pn31[2];

  dotb =
    ( p3[0] - p1[0] ) * pn31[0] +
    ( p3[1] - p1[1] ) * pn31[1] +
    ( p3[2] - p1[2] ) * pn31[2];

  delete [] pn31;

  if ( dotb < 0.0 )
  {
    dott = -dott;
    dotb = -dotb;
  }

  if ( dott < 0.0 || dotb < dott )
  {
    return false;
  }
//
//  V = ALPHA * V1 + BETA * V2, where both ALPHA and BETA are between
//  0 and 1.
//
  return true;
# undef DIM_NUM
# undef TOL
}
//****************************************************************************80

double parallelogram_point_dist_3d ( double p1[3], double p2[3], double p3[3],
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PARALLELOGRAM_POINT_DIST_3D: distance ( parallelogram, point ) in 3D.
//
//  Diagram:
//
//         P2..............
//        /              .
//       /              .
//      /              .
//    P1------------->P3
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], three vertices of the parallelogram.
//
//    Input, double P[3], the point to be checked.
//
//    Output, double PARALLELOGRAM_POINT_DIST_3D, the distance from the point
//    to the parallelogram.  DIST is zero if the point lies exactly on the
//    parallelogram.
//
{
# define DIM_NUM 3

  double dis13;
  double dis21;
  double dis34;
  double dis42;
  double dist;
  bool inside;
  double t;
  double temp;
  double p4[DIM_NUM];
  double pn[DIM_NUM];
  double pp[DIM_NUM];
//
//  Compute P, the unit normal to P2-P1 and P3-P1:
//
  pp[0] = ( p2[1] - p1[1] ) * ( p3[2] - p1[2] )
        - ( p2[2] - p1[2] ) * ( p3[1] - p1[1] );

  pp[1] = ( p2[2] - p1[2] ) * ( p3[0] - p1[0] )
        - ( p2[0] - p1[0] ) * ( p3[2] - p1[2] );

  pp[2] = ( p2[0] - p1[0] ) * ( p3[1] - p1[1] )
        - ( p2[1] - p1[1] ) * ( p3[0] - p1[0] );

  temp = sqrt ( pp[0] * pp[0] + pp[1] * pp[1] + pp[2] * pp[2] );

  if ( temp == 0.0 )
  {
    cerr << "\n";
    cerr << "PARALLELOGRAM_POINT_DIST_3D - Fatal error!\n";
    cerr << "  The normal vector is zero.\n";
    exit ( 1 );
  }

  pp[0] = pp[0] / temp;
  pp[1] = pp[1] / temp;
  pp[2] = pp[2] / temp;
//
//  Find PN, the nearest point to P in the plane.
//
  t = pp[0] * ( p[0] - p1[0] )
    + pp[1] * ( p[1] - p1[1] )
    + pp[2] * ( p[2] - p1[2] );

  pn[0] = p[0] - pp[0] * t;
  pn[1] = p[1] - pp[1] * t;
  pn[2] = p[2] - pp[2] * t;
//
//  if PN lies WITHIN the parallelogram, we're done.
//
  inside = parallelogram_contains_point_3d ( p1, p2, p3, p );

  if ( inside == true )
  {
    dist = sqrt ( pow ( pn[0] - p[0], 2 )
                + pow ( pn[1] - p[1], 2 )
                + pow ( pn[2] - p[2], 2 ) );
    return dist;
  }
//
//  Otherwise, find the distance between P and each of the
//  four line segments that make up the boundary of the parallelogram.
//
  p4[0] = p2[0] + p3[0] - p1[0];
  p4[1] = p2[1] + p3[1] - p1[1];
  p4[2] = p2[2] + p3[2] - p1[2];

  dis13 = segment_point_dist_3d ( p1, p3, p );

  dist = dis13;

  dis34 = segment_point_dist_3d ( p3, p4, p );

  if ( dis34 < dist )
  {
    dist = dis34;
  }

  dis42 = segment_point_dist_3d ( p4, p2, p );

  if ( dis42 < dist )
  {
    dist = dis42;
  }

  dis21 = segment_point_dist_3d ( p2, p1, p );

  if ( dis21 < dist )
  {
    dist = dis21;
  }

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

bool parallelepiped_contains_point_3d ( double p1[3], double p2[3], double p3[3],
  double p4[3], double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PARALLELEPIPED_CONTAINS_POINT_3D determines if a point is inside a parallelepiped in 3D.
//
//  Discussion:
//
//    A parallelepiped is a "slanted box", that is, opposite
//    sides are parallel planes.
//
//         *------------------*
//        / .                / .
//       /   .              /   .
//      /     .            /     .
//    P4------------------*       .
//      .        .         .       .
//       .        .         .       .
//        .        .         .       .
//         .       P2..........-------.
//          .     /            .     .
//           .   /              .   .
//            . /                . .
//             P1----------------P3
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], P4[3], four vertices of the parallelepiped.
//    It is assumed that P2, P3 and P4 are immediate neighbors of P1.
//
//    Input, double P, the point to be checked.
//
//    Output, bool PARAPP_CONTAINS_POINT_3D, is true if P is inside the
//    parallelepiped, or on its boundary, and false otherwise.
//
{
  double dot;

  dot = ( p2[0] - p1[0] ) * ( p[0] - p1[0] )
      + ( p2[1] - p1[1] ) * ( p[1] - p1[1] )
      + ( p2[2] - p1[2] ) * ( p[2] - p1[2] );

  if ( dot < 0.0 )
  {
    return false;
  }
  else if ( pow ( p2[0] - p1[0], 2 )
          + pow ( p2[1] - p1[1], 2 )
          + pow ( p2[2] - p1[2], 2 ) < dot )
  {
    return false;
  }

  dot = ( p3[0] - p1[0] ) * ( p[0] - p1[0] )
      + ( p3[1] - p1[1] ) * ( p[1] - p1[1] )
      + ( p3[2] - p1[2] ) * ( p[2] - p1[2] );

  if ( dot < 0.0 )
  {
    return false;
  }
  else if ( pow ( p3[0] - p1[0], 2 )
          + pow ( p3[1] - p1[1], 2 )
          + pow ( p3[2] - p1[2], 2 ) < dot )
  {
    return false;
  }

  dot = ( p4[0] - p1[0] ) * ( p[0] - p1[0] )
      + ( p4[1] - p1[1] ) * ( p[1] - p1[1] )
      + ( p4[2] - p1[2] ) * ( p[2] - p1[2] );

  if ( dot < 0.0 )
  {
    return false;
  }
  else if ( pow ( p4[0] - p1[0], 2 )
          + pow ( p4[1] - p1[1], 2 )
          + pow ( p4[2] - p1[2], 2 ) < dot )
  {
    return false;
  }

  return true;
}
//****************************************************************************80

double parallelepiped_point_dist_3d ( double p1[3], double p2[3], double p3[3],
  double p4[3], double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PARALLELEPIPED_POINT_DIST_3D: distance ( parallelepiped, point ) in 3D.
//
//  Discussion:
//
//    A parallelepiped is a "slanted box", that is, opposite
//    sides are parallel planes.
//
//    A parallelepiped is a "slanted box", that is, opposite
//    sides are parallel planes.
//
//         *------------------*
//        / .                / .
//       /   .              /   .
//      /     .            /     .
//    P4------------------*       .
//      .        .         .       .
//       .        .         .       .
//        .        .         .       .
//         .       P2..........-------.
//          .     /            .     /
//           .   /              .   /
//            . /                . /
//             P1----------------P3
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], P4[3], half of
//    the corners of the box, from which the other corners can be
//    deduced.  The corners should be chosen so that the first corner
//    is directly connected to the other three.  The locations of
//    corners 5, 6, 7 and 8 will be computed by the parallelogram
//    relation.
//
//    Input, double P[3], the point which is to be checked.
//
//    Output, double PARAPP_POINT_DIST_3D, the distance from the point to the box.
//    The distance is zero if the point lies exactly on the box.
//
{
# define DIM_NUM 3

  double dis;
  double dist;
  double p5[DIM_NUM];
  double p6[DIM_NUM];
  double p7[DIM_NUM];
  double p8[DIM_NUM];
//
//  Fill in the other corners
//
  p5[0] = p2[0] + p3[0] - p1[0];
  p5[1] = p2[1] + p3[1] - p1[1];
  p5[2] = p2[2] + p3[2] - p1[2];

  p6[0] = p2[0] + p4[0] - p1[0];
  p6[1] = p2[1] + p4[1] - p1[1];
  p6[2] = p2[2] + p4[2] - p1[2];

  p7[0] = p3[0] + p4[0] - p1[0];
  p7[1] = p3[1] + p4[1] - p1[1];
  p7[2] = p3[2] + p4[2] - p1[2];

  p8[0] = p2[0] + p3[0] + p4[0] - 2.0 * p1[0];
  p8[1] = p2[1] + p3[1] + p4[1] - 2.0 * p1[1];
  p8[2] = p2[2] + p3[2] + p4[2] - 2.0 * p1[2];
//
//  Compute the distance from the point P to each of the six
//  paralleogram faces.
//
  dis = parallelogram_point_dist_3d ( p1, p2, p3, p );

  dist = dis;

  dis = parallelogram_point_dist_3d ( p1, p2, p4, p );

  if ( dis < dist )
  {
    dist = dis;
  }

  dis = parallelogram_point_dist_3d ( p1, p3, p4, p );

  if ( dis < dist )
  {
    dist = dis;
  }

  dis = parallelogram_point_dist_3d ( p8, p5, p6, p );

  if ( dis < dist )
  {
    dist = dis;
  }

  dis = parallelogram_point_dist_3d ( p8, p5, p7, p );

  if ( dis < dist )
  {
    dist = dis;
  }

  dis = parallelogram_point_dist_3d ( p8, p6, p7, p );

  if ( dis < dist )
  {
    dist = dis;
  }

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

bool perm_check ( int n, int p[] )

//****************************************************************************80
//
//  Purpose:
//
//    PERM_CHECK checks that a vector represents a permutation.
//
//  Discussion:
//
//    The routine verifies that each of the integers from 1
//    to N occurs among the N entries of the permutation.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 January 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries.
//
//    Input, int P[N], the array to check.
//
//    Output, bool PERM_CHECK, is TRUE if the permutation is OK.
//
{
  bool found;
  int i;
  int seek;

  for ( seek = 1; seek <= n; seek++ )
  {
    found = false;

    for ( i = 0; i < n; i++ )
    {
      if ( p[i] == seek )
      {
        found = true;
        break;
      }
    }

    if ( !found )
    {
      return false;
    }

  }

  return true;
}
//****************************************************************************80

void perm_inv ( int n, int p[] )

//****************************************************************************80
//
//  Purpose:
//
//    PERM_INV inverts a permutation "in place".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 January 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of objects being permuted.
//
//    Input/output, int P[N], the permutation, in standard index form.
//    On output, P describes the inverse permutation
//
{
  int i;
  int i0;
  int i1;
  int i2;
  int is;

  if ( n <= 0 )
  {
    cerr << "\n";
    cerr << "PERM_INV - Fatal error!\n";
    cerr << "  Input value of N = " << n << "\n";
    exit ( 1 );
  }

  if ( !perm_check ( n, p ) )
  {
    cerr << "\n";
    cerr << "PERM_INV - Fatal error!\n";
    cerr << "  The input array does not represent\n";
    cerr << "  a proper permutation.\n";
    exit ( 1 );
  }

  is = 1;

  for ( i = 1; i <= n; i++ )
  {
    i1 = p[i-1];

    while ( i < i1 )
    {
      i2 = p[i1-1];
      p[i1-1] = -i2;
      i1 = i2;
    }

    is = - i4_sign ( p[i-1] );
    p[i-1] = i4_sign ( is ) * abs ( p[i-1] );
  }

  for ( i = 1; i <= n; i++ )
  {
    i1 = -p[i-1];

    if ( 0 <= i1 )
    {
      i0 = i;

      for ( ; ; )
      {
        i2 = p[i1-1];
        p[i1-1] = i0;

        if ( i2 < 0 )
        {
          break;
        }
        i0 = i1;
        i1 = i2;
      }
    }
  }

  return;
}
//****************************************************************************80

void plane_exp_grid_3d ( double p1[3], double p2[3], double p3[3], int *ncor3,
  int *line_num, double cor3[], int lines[], int maxcor3, int line_max,
  int *ierror )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_EXP_GRID_3D computes points and lines making up a planar grid in 3D.
//
//  Discussion:
//
//    The data format used is that of SGI Inventor.
//
//    On input, if NCOR3 is zero (or negative), then the data computed by
//    this routine will be stored normally in COR3.  But if NCOR3 is
//    positive, it is assumed that COR3 already contains NCOR3 items
//    of useful data.  The new data is appended to COR3.  On output, NCOR3
//    is increased by the number of points computed by this routine.
//
//    On input, if LINE_NUM is zero (or negative), then the data computed by
//    this routine will be stored normally in LINES.  But if LINE_NUM is positive,
//    it is assumed that LINES already contains some useful data.  The
//    new data is appended to LINES.  On output, LINE_NUM is increased by the
//    number of line data items computed by this routine.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], three points on the plane.
//
//    Input/output, int *NCOR3, the number of points stored in COR3.
//
//    Input/output, int *LINE_NUM, the number of line data items.
//
//    Output, double COR3[3*MAXCOR3], the coordinates of points
//    used in the grid.
//
//    Output, int LINES[LINE_MAX], the indices of points used in
//    the lines of the grid.  Successive entries of LINES are joined
//    by a line, unless an entry equals -1.  Note that indices begin
//    with 0.
//
//    Input, int MAXCOR3, the maximum number of points.
//
//    Input, int LINE_MAX, the maximum number of lines.
//
//    Output, int *IERROR, error indicator.
//    0, no error.
//    1, more space for point coordinates is needed.
//    2, more space for line data is needed.
//
{
# define DIM_NUM 3
# define NX 5
# define NY 5

  double a = 0.0;
  double amax = 0.0;
  double amin = 0.0;
  double b = 0.0;
  double bmax = 0.0;
  double bmin = 0.0;
  double dot = 0.0;
  int i;
  int j;
  int k;
  int nbase;
  double v1[DIM_NUM];
  double v2[DIM_NUM];

  *ierror = 0;

  if ( *ncor3 <= 0 )
  {
    *ncor3 = 0;
  }

  if ( *line_num <= 0 )
  {
    *line_num = 0;
  }

  nbase = *ncor3;
//
//  Compute the two basis vectors for the affine plane.
//
  v1[0] = p2[0] - p1[0];
  v1[1] = p2[1] - p1[1];
  v1[2] = p2[2] - p1[2];

  vector_unit_nd ( DIM_NUM, v1 );

  v2[0] = p3[0] - p1[0];
  v2[1] = p3[1] - p1[1];
  v2[2] = p3[2] - p1[2];

  dot = r8vec_dot_product ( 3, v1, v2 );
//
//  Remove the component of V1 from V2, and give the
//  resulting vector unit norm.  V1 and V2 are now orthogonal
//  and of unit length, and represent the two direction vectors
//  of our plane.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v2[i] = v2[i] - dot * v1[i];
  }

  vector_unit_nd ( DIM_NUM, v2 );
//
//  Compute the (V1,V2) coordinate range of the input data, if any.
//
  if ( *ncor3 == 0 )
  {
    amin = 0.0;
    amax = 1.0;
    bmin = 0.0;
    bmax = 1.0;
  }
  else
  {
    for ( i = 0; i < *ncor3; i++ )
    {
      a = 0.0;
      b = 0.0;
      for ( j = 0; j < 3; j++ )
      {
        a = a + v1[j] * cor3[j+i*3];
        b = b + v2[j] * cor3[j+i*3];
      }

      if ( i == 0 )
      {
        amin = a;
        amax = a;
        bmin = b;
        bmax = b;
      }
      else
      {
        amin = fmin ( amin, a );
        amax = fmax ( amax, a );
        bmin = fmin ( bmin, b );
        bmax = fmax ( bmax, b );
      }
    }
  }
//
//  Generate the points we will use.
//
  if ( maxcor3 < *ncor3 + NX * NY )
  {
    *ierror = 1;
    return;
  }

  for ( j = 1; j <= NY; j++ )
  {
    b = ( ( double ) ( NY - j     ) * bmin
        + ( double ) (      j - 1 ) * bmax )
        / ( double ) ( NY      - 1 );

    for ( i = 1; i <= NX; i++ )
    {
      a = ( ( double ) ( NX - i     ) * amin
          + ( double ) (      i - 1 ) * amax )
          / ( double ) ( NX      - 1 );

      for ( k = 0; k < 3; k++ )
      {
        cor3[k+(*ncor3)*3] = a * v1[k] + b * v2[k];
      }
      *ncor3 = *ncor3 + 1;
    }
  }
//
//  Do the "horizontals".
//
  for ( i = 1; i <= NX; i++ )
  {
    for ( j = 1; j <= NY; j++ )
    {
      if ( line_max <= *line_num )
      {
        *ierror = 2;
        return;
      }
      lines[*line_num] = nbase + ( j - 1 ) * NX + i;
      *line_num = *line_num + 1;
    }

    if ( line_max <= *line_num )
    {
      *ierror = 2;
      return;
    }
    lines[*line_num] = -1;
    *line_num = *line_num + 1;
  }
//
//  Do the "verticals".
//
  for ( j = 1; j <= NY; j++ )
  {
    for ( i = 1; i <= NX; i++ )
    {
      if ( line_max <= *line_num )
      {
        *ierror = 2;
        return;
      }
      lines[*line_num] = nbase + ( j - 1 ) * NX + i;
      *line_num = *line_num + 1;
    }

    if ( line_max <= *line_num )
    {
      *ierror = 2;
      return;
    }
    lines[*line_num] = -1;
    *line_num = *line_num + 1;
  }

  return;
# undef DIM_NUM
# undef NX
# undef NY
}
//****************************************************************************80

double plane_exp_point_dist_3d ( double p1[3], double p2[3], double p3[3],
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_EXP_POINT_DIST_3D: distance ( explicit plane, point ) in 3D.
//
//  Discussion:
//
//    The explicit form of a plane in 3D is
//
//      the plane through P1, P2 and P3.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], three points on the plane.
//
//    Input, double P[3], the coordinates of the point.
//
//    Output, double PLANE_EXP_POINT_DIST_3D, the distance from the
//    point to the plane.
//
{
  double a;
  double b;
  double c;
  double d;
  double dist;

  plane_exp2imp_3d ( p1, p2, p3, &a, &b, &c, &d );

  dist = plane_imp_point_dist_3d ( a, b, c, d, p );

  return dist;
}
//****************************************************************************80

void plane_exp_normal_3d ( double p1[3], double p2[3], double p3[3],
  double pn[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_EXP_NORMAL_3D finds the normal to an explicit plane in 3D.
//
//  Discussion:
//
//    The explicit form of a plane in 3D is
//
//      the plane through P1, P2 and P3.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], three points on the plane.
//
//    Output, double PN[3], the unit normal vector to the plane.
//
{
  double norm;
//
//  The cross product (P2-P1) x (P3-P1) is a vector normal to
//  (P2-P1) and (P3-P1).
//
  pn[0] = ( p2[1] - p1[1] ) * ( p3[2] - p1[2] )
        - ( p2[2] - p1[2] ) * ( p3[1] - p1[1] );

  pn[1] = ( p2[2] - p1[2] ) * ( p3[0] - p1[0] )
        - ( p2[0] - p1[0] ) * ( p3[2] - p1[2] );

  pn[2] = ( p2[0] - p1[0] ) * ( p3[1] - p1[1] )
        - ( p2[1] - p1[1] ) * ( p3[0] - p1[0] );

  norm = sqrt ( pow ( pn[0], 2 ) + pow ( pn[1], 2 ) + pow ( pn[2], 2 ) );

  if ( norm == 0.0 )
  {
    cerr << "\n";
    cerr << "PLANE_EXP_NORMAL_3D - Fatal error!\n";
    cerr << "  The plane is poorly defined.\n";
    exit ( 1 );
  }
  else
  {
    pn[0] = pn[0] / norm;
    pn[1] = pn[1] / norm;
    pn[2] = pn[2] / norm;
  }

  return;
}
//****************************************************************************80

void plane_exp_pro2 ( double p1[3], double p2[3], double p3[3], int n,
  double pp[], double alpha[], double beta[] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_EXP_PRO2 produces 2D coordinates of points that lie in a plane, in 3D.
//
//  Discussion:
//
//    The explicit form of a plane in 3D is
//
//      the plane through P1, P2 and P3.
//
//    The first thing to do is to compute two orthonormal vectors V1 and
//    V2, so that any point P that lies in the plane may be written as
//
//      P = P1 + alpha * V1 + beta * V2
//
//    The vector V1 lies in the direction P2-P1, and V2 lies in
//    the plane, is orthonormal to V1, and has a positive component
//    in the direction of P3-P1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], three points on the plane.
//
//    Input, int N, the number of points to project.
//
//    Input, double PP[3*N], the Cartesian coordinates of points which lie on
//    the plane spanned by the three points.  These points are not checked to
//    ensure that they lie on the plane.
//
//    Output, double ALPHA[N], BETA[N], the "in-plane" coordinates of
//    the points.
//
{
  double dot;
  int i;
  double v1[3];
  double v2[3];
//
//  Compute the two basis vectors for the affine plane.
//
  v1[0] = p2[0] - p1[0];
  v1[1] = p2[1] - p1[1];
  v1[2] = p2[2] - p1[2];

  vector_unit_nd ( 3, v1 );

  v2[0] = p3[0] - p1[0];
  v2[1] = p3[1] - p1[1];
  v2[2] = p3[2] - p1[2];

  dot = r8vec_dot_product ( 3, v1, v2 );

  for ( i = 0; i < 3; i++ )
  {
    v2[i] = v2[i] - dot * v1[i];
  }
  vector_unit_nd ( 3, v2 );
//
//  Now decompose each point.
//
  for ( i = 0; i < n; i++ )
  {
    alpha[i] = ( pp[0+i*3] - p1[0] ) * v1[0]
            +  ( pp[1+i*3] - p1[1] ) * v1[1]
            +  ( pp[2+i*3] - p1[2] ) * v1[2];

    beta[i] =  ( pp[0+i*3] - p1[0] ) * v2[0]
             + ( pp[1+i*3] - p1[1] ) * v2[1]
             + ( pp[2+i*3] - p1[2] ) * v2[2];
  }

  return;
}
//****************************************************************************80

void plane_exp_pro3 ( double p1[3], double p2[3], double p3[3], int n,
  double po[], double pp[] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_EXP_PRO3 projects points orthographically onto a plane, in 3D.
//
//  Discussion:
//
//    The explicit form of a plane in 3D is
//
//      the plane through P1, P2 and P3.
//
//    PP may share the same memory as PO, in
//    which case the projections will overwrite the original data.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], three points on the plane.
//
//    Input, int N, the number of points to project.
//
//    Input, double PO[3*N], the object points.
//
//    Output, double PP[3*N], the projections of the object points.
//
{
  double a;
  double b;
  double c;
  double d;
  int i;
//
//  Put the plane into ABCD form.
//
  plane_exp2imp_3d ( p1, p2, p3, &a, &b, &c, &d );
//
//  For each point, its image in the plane is the nearest point
//  in the plane.
//
  for ( i = 0; i < n; i++ )
  {
    plane_imp_point_near_3d ( a, b, c, d, po+i*3, pp+i*3 );
  }

  return;
}
//****************************************************************************80

void plane_exp_project_3d ( double p1[3], double p2[3], double p3[3],
  double pf[3], int n, double po[], double pp[], int ivis[] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_EXP_PROJECT_3D projects points through a point onto a plane in 3D.
//
//  Discussion:
//
//    The explicit form of a plane in 3D is
//
//      the plane through P1, P2 and P3.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], three points on the plane.
//
//    Input, double PF[3], the focus point.
//
//    Input, int N, the number of points to project.
//
//    Input, double PO[3*N], the object points.
//
//    Output, double PP[3*N], the projections of the object points through the
//    focus point onto the plane.  PP may share the same memory as PO,
//    in which case the projections will overwrite the original data.
//
//    Output, int IVIS[N], visibility indicator:
//    3, the object was behind the plane;
//    2, the object was already on the plane;
//    1, the object was between the focus and the plane;
//    0, the line from the object to the focus is parallel to the plane,
//    so the object is "invisible".
//    -1, the focus is between the object and the plane.  The object
//    might be considered invisible.
//
{
# define DIM_NUM 3

  double a;
  double alpha;
  double b;
  double beta;
  double c;
  double d;
  double disfo;
  double disfn;
  int i;
  double pn[DIM_NUM];
//
//  Put the plane into ABCD form.
//
  plane_exp2imp_3d ( p1, p2, p3, &a, &b, &c, &d );
//
//  Get the nearest point on the plane to the focus.
//
  plane_imp_point_near_3d ( a, b, c, d, pf, pn );
//
//  Get the distance from the focus to the plane.
//
  disfn = points_dist_3d ( pf, pn );
//
//  If the focus lies in the plane, this is bad.  We could still
//  project points that actually lie in the plane, but we'll
//  just bail out.
//
  if ( disfn == 0.0 )
  {
    for ( i = 0; i < n; i++ )
    {
      ivis[i] = 0;
      pp[0+i*3] = pf[0];
      pp[1+i*3] = pf[1];
      pp[2+i*3] = pf[2];
    }
    return;
  }
//
//  Process the points.
//
  for ( i = 0; i < n; i++ )
  {
//
//  Get the distance from the focus to the object.
//
    disfo = points_dist_3d ( pf, po+i*3 );

    if ( disfo == 0.0 )
    {
      ivis[i] = 0;
      pp[0+i*3] = pn[0];
      pp[1+i*3] = pn[1];
      pp[2+i*3] = pn[2];
    }
    else
    {
//
//  Compute ALPHA, the angle between (OBJECT-FOCUS) and (NEAREST-FOCUS).
//
      alpha = angle_rad_3d ( po+i*3, pf, pn );

      if ( cos ( alpha ) == 0.0 )
      {
        ivis[i] = 0;
        pp[0+i*3] = pn[0];
        pp[1+i*3] = pn[1];
        pp[2+i*3] = pn[2];
      }
      else
      {
//
//  BETA is Dist(NEAREST-FOCUS) / ( Cos(ALPHA)*Dist(OBJECT-FOCUS) )
//
        beta = disfn / ( cos ( alpha ) * disfo );

        if ( 1.0 < beta )
        {
          ivis[i] = 1;
        }
        else if ( beta == 1.0 )
        {
          ivis[i] = 2;
        }
        else if ( 0.0 < beta )
        {
          ivis[i] = 3;
        }
        else
        {
          ivis[i] = -1;
        }
//
//  Set the projected point.
//
        pp[0+i*3] = pf[0] + beta * ( po[0+i*3] - pf[0] );
        pp[1+i*3] = pf[1] + beta * ( po[1+i*3] - pf[1] );
        pp[2+i*3] = pf[2] + beta * ( po[2+i*3] - pf[2] );
      }
    }
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

void plane_exp2imp_3d ( double p1[3], double p2[3], double p3[3], double *a,
  double *b, double *c, double *d )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_EXP2IMP_3D converts an explicit plane to implicit form in 3D.
//
//  Discussion:
//
//    The explicit form of a plane in 3D is
//
//      the plane through P1, P2 and P3.
//
//    The implicit form of a plane in 3D is
//
//      A * X + B * Y + C * Z + D = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], three points on the plane.
//
//    Output, double *A, *B, *C, *D, coefficients which describe the plane.
//
{
  *a = ( p2[1] - p1[1] ) * ( p3[2] - p1[2] )
     - ( p2[2] - p1[2] ) * ( p3[1] - p1[1] );

  *b = ( p2[2] - p1[2] ) * ( p3[0] - p1[0] )
     - ( p2[0] - p1[0] ) * ( p3[2] - p1[2] );

  *c = ( p2[0] - p1[0] ) * ( p3[1] - p1[1] )
     - ( p2[1] - p1[1] ) * ( p3[0] - p1[0] );

  *d = - p2[0] * (*a) - p2[1] * (*b) - p2[2] * (*c);

  return;
}
//****************************************************************************80

void plane_exp2normal_3d ( double p1[3], double p2[3], double p3[3],
  double pp[3], double pn[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_EXP2NORMAL_3D converts an explicit plane to normal form in 3D.
//
//  Discussion;
//
//    The explicit form of a plane in 3D is
//
//      the plane through P1, P2 and P3.
//
//    The normal form of a plane in 3D is
//
//      PP, a point on the plane, and
//      PN, the unit normal to the plane.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], three points on the plane.
//
//    Output, double PP[3], a point on the plane.
//
//    Output, double PN[3], the unit normal vector to the plane.
//
{
# define DIM_NUM 3

  double norm;

  r8vec_copy ( DIM_NUM, p1, pp );

  pn[0] = ( p2[1] - p1[1] ) * ( p3[2] - p1[2] )
        - ( p2[2] - p1[2] ) * ( p3[1] - p1[1] );
  pn[1] = ( p2[2] - p1[2] ) * ( p3[0] - p1[0] )
        - ( p2[0] - p1[0] ) * ( p3[2] - p1[2] );
  pn[2] = ( p2[0] - p1[0] ) * ( p3[1] - p1[1] )
        - ( p2[1] - p1[1] ) * ( p3[0] - p1[0] );

  norm = sqrt ( pn[0] * pn[0] + pn[1] * pn[1] + pn[2] * pn[2] );

  if ( norm == 0.0 )
  {
    cerr << "\n";
    cerr << "PLANE_EXP2NORMAL_3D - Fatal error!\n";
    cerr << "  The normal vector is null.\n";
    cerr << "  Two points coincide, or nearly so.\n";
    exit ( 1 );
  }

  pn[0] = pn[0] / norm;
  pn[1] = pn[1] / norm;
  pn[2] = pn[2] / norm;

  return;
# undef DIM_NUM
}
//****************************************************************************80

bool plane_imp_is_degenerate_3d ( double a, double b, double c )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_IMP_IS_DEGENERATE_3D is TRUE if an implicit plane is degenerate.
//
//  Discussion:
//
//    The implicit form of a plane in 3D is:
//
//      A * X + B * Y + C * Z + D = 0
//
//    The implicit plane is degenerate if A = B = C = 0.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, C, the implicit plane coefficients.
//
//    Output, bool PLANE_IMP_IS_DEGENERATE_3D, is TRUE if the plane
//    is degenerate.
//
{
  if ( a == 0.0 && b == 0.0 && c == 0.0 )
  {
    return true;
  }
  else
  {
    return false;
  }
}
//****************************************************************************80

bool plane_imp_line_par_int_3d ( double a, double b, double c, double d,
  double x0, double y0, double z0, double f, double g, double h, double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_IMP_LINE_PAR_INT_3D: intersection ( implicit plane, parametric line ) in 3D.
//
//  Discussion:
//
//    The implicit form of a plane in 3D is:
//
//      A * X + B * Y + C * Z + D = 0
//
//    The parametric form of a line in 3D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//      Z = Z0 + H * T
//
//    For normalization, we choose F*F+G*G+H*H = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983, page 111.
//
//  Parameters:
//
//    Input, double A, B, C, D, parameters that define the implicit plane.
//
//    Input, double X0, Y0, Z0, F, G, H, parameters that define the
//    parametric line.
//
//    Output, double P[3], is a point of intersection of the line
//    and the plane, if the line and plane intersect.
//
//    Output, bool PLANE_IMP_LINE_PAR_INT_3D, is TRUE if the line and
//    the plane intersect, and false otherwise.
//
{
  double denom;
  double norm1;
  double norm2;
  double t;
  double TOL = 0.00001;
//
//  Check.
//
  norm1 = sqrt ( a * a + b * b + c * c );

  if ( norm1 == 0.0 )
  {
    cerr << "\n";
    cerr << "PLANE_IMP_LINE_PAR_INT_3D - Fatal error!\n";
    cerr << "  The plane normal vector is null.\n";
    exit ( 1 );
  }

  norm2 = sqrt ( f * f + g * g + h * h );

  if ( norm2 == 0.0 )
  {
    cerr << "\n";
    cerr << "PLANE_IMP_LINE_PAR_INT_3D - Fatal error!\n";
    cerr << "  The line direction vector is null.\n";
    exit ( 1 );
  }

  denom = a * f + b * g + c * h;
//
//  The line and the plane may be parallel.
//
  if ( fabs ( denom ) < TOL * norm1 * norm2 )
  {
    if ( a * x0 + b * y0 + c * z0 + d == 0.0 )
    {
      p[0] = x0;
      p[1] = y0;
      p[2] = z0;
      return true;
    }
    else
    {
      r8vec_zero ( 3, p );
      return false;
    }
  }
//
//  If they are not parallel, they must intersect.
//
  else
  {
    t = - ( a * x0 + b * y0 + c * z0 + d ) / denom;
    p[0] = x0 + t * f;
    p[1] = y0 + t * g;
    p[2] = z0 + t * h;
    return true;
  }
}
//****************************************************************************80

double plane_imp_point_dist_3d ( double a, double b, double c, double d,
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_IMP_POINT_DIST_3D: distance ( point, implicit plane ) in 3D.
//
//  Discussion:
//
//    The implicit form of a plane in 3D is:
//
//      A * X + B * Y + C * Z + D = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double A, B, C, D, coefficients that define the plane as
//    the set of points for which A*X+B*Y+C*Z+D = 0.
//
//    Input, double P[3], the coordinates of the point.
//
//    Output, double PLANE_IMP_POINT_DIST_3D, the distance from the point to
//    the plane.
//
{
  double dist;

  dist =
    fabs ( a * p[0] + b * p[1] + c * p[2] + d ) /
    sqrt ( a * a + b * b + c * c );

  return dist;
}
//****************************************************************************80

double plane_imp_point_dist_signed_3d ( double a, double b, double c, double d,
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_IMP_POINT_DIST_SIGNED_3D: signed distance ( implicit plane, point) in 3
//
//  Discussion:
//
//    The implicit form of a plane in 3D is:
//
//      A * X + B * Y + C * Z + D = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, C, D, determine the equation of the
//    plane, which is:
//
//      A*X + B*Y + C*Z + D = 0.
//
//    Input, double P[3], the coordinates of the point.
//
//    Output, double PLANE_IMP_POINT_DIST_SIGNED_3D, the signed distance from
//    the point to the plane.
//
{
  double dist;

  dist = - ( a * p[0] + b * p[1] + c * p[2] + d )
    / sqrt ( a * a + b * b + c * c );

  if ( d < 0.0 )
  {
    dist = -dist;
  }

  return dist;
}
//****************************************************************************80

void plane_imp_point_near_3d ( double a, double b, double c, double d,
  double p[3], double pn[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_IMP_POINT_NEAR_3D: nearest point on a implicit plane to a point in 3D.
//
//  Discussion:
//
//    The implicit form of a plane in 3D is:
//
//      A * X + B * Y + C * Z + D = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, C, D, coefficients that define the plane as
//    the set of points for which A*X+B*Y+C*Z+D = 0.
//
//    Input, double P[3], the coordinates of the point.
//
//    Output, double PN[3], the coordinates of the nearest point on
//    the plane.
//
{
  double t;

  if ( plane_imp_is_degenerate_3d ( a, b, c ) )
  {
    cerr << "\n";
    cerr << "PLANE_IMP_POINT_NEAR_3D - Fatal error!\n";
    cerr << "  A = B = C = 0.\n";
    exit ( 1 );
  }
//
//  The normal N to the plane is (A,B,C).
//
//  The line defined by (XN-X)/A = (YN-Y)/B = (ZN-Z)/C = T
//  goes through (X,Y,Z) and is parallel to N.
//
//  Solving for the point PN we get
//
//    XN = A*T+X
//    YN = B*T+Y
//    ZN = C*T+Z
//
//  Now place these values in the equation for the plane:
//
//    A*(A*T+X) + B*(B*T+Y) + C*(C*T+Z) + D = 0
//
//  and solve for T:
//
//    T = (-A*X-B*Y-C*Z-D) / (A * A + B * B + C * C )
//
  t = - ( a * p[0] + b * p[1] + c * p[2] + d ) / ( a * a + b * b + c * c );

  pn[0] = p[0] + a * t;
  pn[1] = p[1] + b * t;
  pn[2] = p[2] + c * t;

  return;
}
//****************************************************************************80

void plane_imp_segment_near_3d ( double p1[3], double p2[3], double a, double b,
  double c, double d, double *dist, double pnp[3], double pnl[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_IMP_SEGMENT_NEAR_3D: nearest ( implicit plane, line segment ) in 3D
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the endpoints of the line
//    segment.
//
//    Input, double A, B, C, D, the parameters that define the implicit
//    plane.
//
//    Output, double *DIST, the distance between the line segment and
//    the plane.
//
//    Output, double PNP[3], the nearest point on the plane.
//
//    Output, double PNL[3], the nearest point on the line segment
//    to the plane.  If DIST is zero, PNL is a point of
//    intersection of the plane and the line segment.
//
{
# define DIM_NUM 3

  double alpha;
  double an;
  double bn;
  double cn;
  double dn;
  double idiocy;
  double norm;
  double t1;
  double t2;

  r8vec_zero ( DIM_NUM, pnl );
  r8vec_zero ( DIM_NUM, pnp );

  norm = sqrt ( a * a + b * b + c * c );

  if ( norm == 0.0 )
  {
    cerr << "\n";
    cerr << "PLANE_IMP_SEGMENT_NEAR_3D - Fatal error!\n";
    cerr << "  Plane normal vector is null.\n";
    exit ( 1 );
  }
//
//  The normalized coefficients allow us to compute the (signed) distance.
//
  an = a / norm;
  bn = b / norm;
  cn = c / norm;
  dn = d / norm;
//
//  If the line segment is actually a point, then the answer is easy.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) )
  {
    t1 = an * p1[0] + bn * p1[1] + cn * p1[2] + dn;
    *dist = fabs ( t1 );
    r8vec_copy ( DIM_NUM, p1, pnl );

    pnp[0] = p1[0] - an * t1;
    pnp[1] = p1[1] - bn * t1;
    pnp[2] = p1[2] - cn * t1;

    return;
  }
//
//  Compute the projections of the two points onto the normal vector.
//
  t1 = an * p1[0] + bn * p1[1] + cn * p1[2] + dn;
  t2 = an * p2[0] + bn * p2[1] + cn * p2[2] + dn;
//
//  If these have the same sign, then the line segment does not
//  cross the plane, and one endpoint is the nearest point.
//
  idiocy = t1 * t2;
  if ( 0.0 < idiocy )
  {
    t1 = fabs ( t1 );
    t2 = fabs ( t2 );

    if ( t1 < t2 )
    {
      r8vec_copy ( DIM_NUM, p1, pnl );
      pnp[0] = p1[0] - an * t1;
      pnp[1] = p1[1] - bn * t1;
      pnp[2] = p1[2] - cn * t1;
      *dist = t1;
    }
    else
    {
      r8vec_copy ( DIM_NUM, p2, pnl );
      *dist = t2;
      pnp[0] = p2[0] - an * t2;
      pnp[1] = p2[1] - bn * t2;
      pnp[2] = p2[2] - cn * t2;
    }
//
//  If the projections differ in sign, the line segment crosses the plane.
//
  }
  else
  {

    if ( t1 == 0.0 )
    {
      alpha = 0.0;
    }
    else if ( t2 == 0.0 )
    {
      alpha = 1.0;
    }
    else
    {
      alpha = t2 / ( t2 - t1 );
    }

    pnl[0] = alpha * p1[0] + ( 1.0 - alpha ) * p2[0];
    pnl[1] = alpha * p1[1] + ( 1.0 - alpha ) * p2[1];
    pnl[2] = alpha * p1[2] + ( 1.0 - alpha ) * p2[2];
    r8vec_copy ( DIM_NUM, pnl, pnp );

    *dist = 0.0;
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

void plane_imp_triangle_int_3d ( double a, double b, double c, double d,
  double t[3*3], int *int_num, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_IMP_TRIANGLE_INT_3D: intersection ( implicit plane, triangle ) in 3D.
//
//  Discussion:
//
//    An implicit plane in 3D is the set of points satisfying
//      A * X + B * Y + C * Z + D = 0,
//    for a given set of parameters A, B, C, D.  At least one of
//    A, B and C must be nonzero.
//
//    There may be 0, 1, 2 or 3 points of intersection return;ed.
//
//    If two intersection points are return;ed, then the entire line
//    between them comprises points of intersection.
//
//    If three intersection points are return;ed, then all points of
//    the triangle intersect the plane.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, C, D, the parameters that define the implicit plane.
//
//    Input, double T[3*3], the vertices of the triangle.
//
//    Output, double DIST, the distance between the triangle and the plane.
//
//    Output, int *INT_NUM, the number of intersection points return;ed.
//
//    Output, double P[3*3], the coordinates of the intersection points.
//
{
  double dist1;
  double dist2;
  double dist3;
  int n;

  n = 0;
//
//  Compute the signed distances between the vertices and the plane.
//
  dist1 = a * t[0+0*3] + b * t[1+0*3] + c * t[2+0*3] + d;
  dist2 = a * t[0+1*3] + b * t[1+1*3] + c * t[2+1*3] + d;
  dist3 = a * t[0+2*3] + b * t[1+2*3] + c * t[2+2*3] + d;
//
//  Consider any zero distances.
//
  if ( dist1 == 0.0 )
  {
    p[0+n*3] = t[0+0*3];
    p[1+n*3] = t[1+0*3];
    p[2+n*3] = t[2+0*3];
    n = n + 1;
  }

  if ( dist2 == 0.0 )
  {
    p[0+n*3] = t[0+1*3];
    p[1+n*3] = t[1+1*3];
    p[2+n*3] = t[2+1*3];
    n = n + 1;
  }

  if ( dist3 == 0.0 )
  {
    p[0+n*3] = t[0+2*3];
    p[1+n*3] = t[1+2*3];
    p[2+n*3] = t[2+2*3];
    n = n + 1;
  }
//
//  If 2 or 3 of the nodes intersect, we're already done.
//
  if ( 2 <= n )
  {
    *int_num = n;
    return;
  }
//
//  If one node intersects, then we're done unless the other two
//  are of opposite signs.
//
  if ( n == 1 )
  {
    if ( dist1 == 0.0 )
    {
      plane_imp_triangle_int_add_3d ( t+1*3, t+2*3, dist2, dist3, &n, p );
    }
    else if ( dist2 == 0.0 )
    {
      plane_imp_triangle_int_add_3d ( t+0*3, t+2*3, dist1, dist3, &n, p );
    }
    else if ( dist3 == 0.0 )
    {
      plane_imp_triangle_int_add_3d ( t+0*3, t+1*3, dist1, dist2, &n, p );
    }
    return;
  }
//
//  All nodal distances are nonzero, and there is at least one
//  positive and one negative.
//
  if ( dist1 * dist2 < 0.0 && dist1 * dist3 < 0.0 )
  {
    plane_imp_triangle_int_add_3d ( t+0*3, t+1*3, dist1, dist2, &n, p );
    plane_imp_triangle_int_add_3d ( t+0*3, t+2*3, dist1, dist3, &n, p );
  }
  else if ( dist2 * dist1 < 0.0 && dist2 * dist3 < 0.0 )
  {
    plane_imp_triangle_int_add_3d ( t+1*3, t+0*3, dist2, dist1, &n, p );
    plane_imp_triangle_int_add_3d ( t+1*3, t+2*3, dist2, dist3, &n, p );
  }
  else if ( dist3 * dist1 < 0.0 && dist3 * dist2 < 0.0 )
  {
    plane_imp_triangle_int_add_3d ( t+2*3, t+0*3, dist3, dist1, &n, p );
    plane_imp_triangle_int_add_3d ( t+2*3, t+1*3, dist3, dist2, &n, p );
  }

  *int_num = n;

  return;
}
//****************************************************************************80

void plane_imp_triangle_int_add_3d ( double p1[3], double p2[3], double dist1,
  double dist2, int *int_num, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_IMP_TRIANGLE_INT_ADD_3D is a utility for PLANE_IMP_TRIANGLE_INT_3D.
//
//  Discussion:
//
//    This routine is called to consider the value of the signed distance
//    from a plane of two nodes of a triangle.  If the two values
//    have opposite signs, then there is a point of intersection between
//    them.  The routine computes this point and adds it to the list.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], two vertices of a triangle.
//
//    Input, double DIST1, DIST2, the signed distances of the two vertices
//    from a plane.
//
//    Input/output, int *INT_NUM, the number of intersection points.
//
//    Input/output, double P[3*(*INT_NUM)], the coordinates
//    of the intersection points.
//
{
  double alpha;
  int n;

  n = *int_num;

  if ( dist1 == 0.0 )
  {
    p[0+n*3] = p1[0];
    p[1+n*3] = p1[1];
    p[2+n*3] = p1[2];
    n = n + 1;
  }
  else if ( dist2 == 0.0 )
  {
    p[0+n*3] = p2[0];
    p[1+n*3] = p2[1];
    p[2+n*3] = p2[2];
    n = n + 1;
  }
  else if ( dist1 * dist2 < 0.0 )
  {
    alpha = dist2 / ( dist2 - dist1 );
    p[0+n*3] = alpha * p1[0] + ( 1.0 - alpha ) * p2[0];
    p[1+n*3] = alpha * p1[1] + ( 1.0 - alpha ) * p2[1];
    p[2+n*3] = alpha * p1[2] + ( 1.0 - alpha ) * p2[2];
    n = n + 1;
  }

  *int_num = n;

  return;
}
//****************************************************************************80

int plane_imp_triangle_near_3d ( double t[3*3], double a, double b, double c,
  double d, double *dist, double pn[] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_IMP_TRIANGLE_NEAR_3D: nearest ( implicit plane, triangle ) in 3D.
//
//  Discussion:
//
//    The implicit form of a plane in 3D is:
//
//      A * X + B * Y + C * Z + D = 0
//
//    If DIST = 0, then each point is a point of intersection, and there
//    will be at most 3 such points returned.
//
//    If 0 < DIST, then the points are listed in pairs, with the first
//    being on the triangle, and the second on the plane.  Two points will
//    be listed in the most common case, but possibly 4 or 6.
//
//    Please see to it that the underlying distance routine always returns
//    one of the endpoints if the entire line segment is at zero distance.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[3*3], the vertices of the triangle.
//
//    Input, double A, B, C, D, the parameters that define the implicit plane.
//
//    Output, double *DIST, the distance between the triangle and the plane.
//
//    Output, double PN[3*6], a collection of nearest points.
//
//    Output, int PLANE_IMP_TRIANGLE_NEAR_3D, the number of nearest points
//    returned.
//
{
# define DIM_NUM 3

  double dist12;
  double dist23;
  double dist31;
  int near_num;
  double pp[DIM_NUM];
  double pt[DIM_NUM];

  near_num = 0;
//
//  Consider the line segment P1 - P2.
//
  plane_imp_segment_near_3d ( t+0*3, t+1*3, a, b, c, d, &dist12, pp, pt );

  *dist = dist12;
  r8vec_copy ( DIM_NUM, pt, pn+near_num*3 );
  near_num = near_num + 1;

  if ( 0.0 < dist12 )
  {
    r8vec_copy ( DIM_NUM, pp, pn+near_num*3 );
    near_num = near_num + 1;
  }
//
//  Consider the line segment P2 - P3.
//
  plane_imp_segment_near_3d ( t+1*3, t+2*3, a, b, c, d, &dist23, pp, pt );

  if ( dist23 < *dist )
  {
    near_num = 0;
    *dist = dist23;

    r8vec_copy ( DIM_NUM, pt, pn+near_num*3 );
    near_num = near_num + 1;

    if ( 0.0 < dist23 )
    {
      r8vec_copy ( DIM_NUM, pp, pn+near_num*3 );
      near_num = near_num + 1;
    }
  }
  else if ( dist23 == *dist )
  {
    r8vec_copy ( DIM_NUM, pt, pn+near_num*3 );
    near_num = near_num + 1;

    if ( 0.0 < dist23 )
    {
      r8vec_copy ( DIM_NUM, pp, pn+near_num*3 );
      near_num = near_num + 1;
    }
  }
//
//  Consider the line segment P3 - P1.
//
  plane_imp_segment_near_3d ( t+2*3, t+0*3, a, b, c, d, &dist31, pp, pt );

  if ( dist31 < *dist )
  {
    near_num = 0;
    *dist = dist31;

    r8vec_copy ( DIM_NUM, pt, pn+near_num*3 );
    near_num = near_num + 1;

    if ( 0.0 < dist31 )
    {
      r8vec_copy ( DIM_NUM, pp, pn+near_num*3 );
      near_num = near_num + 1;
    }
  }
  else if ( dist31 == *dist )
  {
    r8vec_copy ( DIM_NUM, pt, pn+near_num*3 );
    near_num = near_num + 1;

    if ( 0.0 < dist31 )
    {
      r8vec_copy ( DIM_NUM, pp, pn+near_num*3 );
      near_num = near_num + 1;
    }
  }

  return near_num;
# undef DIM_NUM
}
//****************************************************************************80

void plane_imp2exp_3d ( double a, double b, double c, double d, double p1[3],
  double p2[3], double p3[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_IMP2EXP_3D converts an implicit plane to explicit form in 3D.
//
//  Discussion:
//
//    The implicit form of a plane in 3D is
//
//      A * X + B * Y + C * Z + D = 0.
//
//    The explicit form of a plane in 3D is
//
//      the plane through P1, P2 and P3.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, C, D, parameters that define the implicit plane.
//
//    Output, double P1[3], P2[3], P3[3], three points on the plane.
//
{
# define DIM_NUM 3

  double pn[DIM_NUM];
  double pp[DIM_NUM];

  plane_imp2normal_3d ( a, b, c, d, pp, pn );

  plane_normal2exp_3d ( pp, pn, p1, p2, p3 );

  return;
# undef DIM_NUM
}
//****************************************************************************80

void plane_imp2normal_3d ( double a, double b, double c, double d,
  double pp[3], double pn[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_IMP2NORMAL_3D converts an implicit plane to normal form in 3D.
//
//  Discussion:
//
//    The implicit form of a plane in 3D is
//
//      A * X + B * Y + C * Z + D = 0.
//
//    The normal form of a plane in 3D is
//
//      PP, a point on the plane, and
//      PN, the unit normal to the plane.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, C, D, parameters that define the implicit plane.
//
//    Output, double PP[3] point on the plane.
//
//    Output, double PN[3], the unit normal vector to the plane.
//
{
  double norm;

  norm = sqrt ( a * a + b * b + c * c );

  if ( norm == 0.0 )
  {
    cerr << "\n";
    cerr << "PLANE_IMP2NORMAL_3D - Fatal error!\n";
    cerr << "  The normal vector is null.\n";
    cerr << "  Two points coincide, or nearly so.\n";
    exit ( 1 );
  }

  pn[0] = a / norm;
  pn[1] = b / norm;
  pn[2] = c / norm;

  if ( a != 0.0 )
  {
    pp[0] = - d / a;
    pp[1] = 0.0;
    pp[2] = 0.0;
  }
  else if ( b != 0.0 )
  {
    pp[0] = 0.0;
    pp[1] = - d / b;
    pp[2] = 0.0;
  }
  else if ( c != 0.0 )
  {
    pp[0] = 0.0;
    pp[1] = 0.0;
    pp[2] = - d / c;
  }
  else
  {
    cerr << "\n";
    cerr << "PLANE_IMP2NORMAL_3D - Fatal error!\n";
    cerr << "  The (A,B,C) vector is null.\n";
    exit ( 1 );
  }

  return;
}
//****************************************************************************80

void plane_normal_basis_3d ( double pp[3], double pn[3], double pq[3],
  double pr[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_NORMAL_BASIS_3D finds two perpendicular vectors in a plane in 3D.
//
//  Discussion:
//
//    The normal form of a plane in 3D is:
//
//      PP is a point on the plane,
//      N is a normal vector to the plane.
//
//    The two vectors to be computed, PQ and PR, can be regarded as
//    the basis of a Cartesian coordinate system for points in the plane.
//    Any point in the plane can be described in terms of the "origin"
//    point PP plus a weighted sum of the two vectors PQ and PR:
//
//      P = PP + a * PQ + b * PR.
//
//    The vectors PQ and PR have unit length, and are perpendicular to N
//    and to each other.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PP[3], a point on the plane.
//
//    Input, double PN[3], a normal vector to the plane.  The
//    vector must not have zero length, but it is not necessary for PN
//    to have unit length.
//
//    Output, double PQ[3], a vector of unit length, perpendicular
//    to the vector PN and the vector PR.
//
//    Output, double PR[3], a vector of unit length, perpendicular
//    to the vector PN and the vector PQ.
//
{
# define DIM_NUM 3

  int i;
  double normal_norm;
  double pr_norm;
  double *temp;
//
//  Compute the length of NORMAL.
//
  normal_norm = r8vec_norm ( DIM_NUM, pn );

  if ( normal_norm == 0.0 )
  {
    cerr << "\n";
    cerr << "PLANE_NORMAL_BASIS_3D - Fatal error!\n";
    cerr << "  The normal vector is 0.\n";
    exit ( 1 );
  }
//
//  Find a vector PQ that is normal to PN and has unit length.
//
  temp = r8vec_any_normal ( DIM_NUM, pn );
  r8vec_copy ( DIM_NUM, temp, pq );
  delete [] temp;
//
//  Now just take the cross product PN x PQ to get the PR vector.
//
  temp = r8vec_cross_product_3d ( pn, pq );

  pr_norm = r8vec_norm ( DIM_NUM, temp );

  for ( i = 0; i < DIM_NUM; i++ )
  {
    pr[i] = temp[i] / pr_norm;
  }
  delete [] temp;

  return;
# undef DIM_NUM
}
//****************************************************************************80

int plane_normal_line_exp_int_3d ( double pp[3], double normal[3],
  double p1[3], double p2[3], double pint[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_NORMAL_LINE_EXP_INT_3D: intersection of plane and line in 3D.
//
//  Discussion:
//
//    The normal form of a plane in 3D is:
//
//      PP is a point on the plane,
//      N is a normal vector to the plane.
//
//    The explicit form of a line in 3D is:
//
//      P1, P2 are two points on the line.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PP[3], a point on the plane.
//
//    Input, double NORMAL[3], a normal vector to the plane.
//
//    Input, double P1[3], P2[3], two distinct points on the line.
//
//    Output, double PINT[3], the coordinates of a
//    common point of the plane and line, when IVAL is 1 or 2.
//
//    Output, integer PLANE_NORMAL_LINE_EXP_INT_3D, the kind of intersection;
//    0, the line and plane seem to be parallel and separate;
//    1, the line and plane intersect at a single point;
//    2, the line and plane seem to be parallel and joined.
//
{
# define DIM_NUM 3

  double direction[DIM_NUM];
  int i;
  int ival;
  double temp;
  double temp2;
//
//  Make sure the line is not degenerate.
//
  if ( line_exp_is_degenerate_nd ( DIM_NUM, p1, p2 ) )
  {
    cerr << "\n";
    cerr << "PLANE_NORMAL_LINE_EXP_INT_3D - Fatal error!\n";
    cerr << "  The line is degenerate.\n";
    exit ( 1 );
  }
//
//  Make sure the plane normal vector is a unit vector.
//
  temp = r8vec_norm ( DIM_NUM, normal );

  if ( temp == 0.0 )
  {
    cerr << "\n";
    cerr << "PLANE_NORMAL_LINE_EXP_INT_3D - Fatal error!\n";
    cerr << "  The normal vector of the plane is degenerate.\n";
    exit ( 1 );
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    normal[i] = normal[i] / temp;
  }
//
//  Determine the unit direction vector of the line.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    direction[i] = p2[i] - p1[i];
  }
  temp = r8vec_norm ( DIM_NUM, direction );

  for ( i = 0; i < DIM_NUM; i++ )
  {
    direction[i] = direction[i] / temp;
  }
//
//  If the normal and direction vectors are orthogonal, then
//  we have a special case to deal with.
//
  if ( r8vec_dot_product ( DIM_NUM, normal, direction ) == 0.0 )
  {
    temp = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      temp = temp + normal[i] * ( p1[i] - pp[i] );
    }
    if ( temp == 0.0 )
    {
      ival = 2;
      r8vec_copy ( DIM_NUM, p1, pint );
    }
    else
    {
      ival = 0;
      for ( i = 0; i < DIM_NUM; i++ )
      {
        pint[i] = HUGE_VAL;
      }
    }

    return ival;
  }
//
//  Determine the distance along the direction vector to the intersection point.
//
  temp = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    temp = temp + normal[i] * ( pp[i] - p1[i] );
  }
  temp2 = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    temp2 = temp2 + normal[i] * direction[i];
  }

  ival = 1;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    pint[i] = p1[i] + temp * direction[i] / temp2;
  }

  return ival;
# undef DIM_NUM
}
//****************************************************************************80

double *plane_normal_qr_to_xyz ( double pp[], double normal[], double pq[],
  double pr[], int n, double qr[] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_NORMAL_QR_TO_XYZ: QR_TO_XYZ coordinates for a normal form plane.
//
//  Discussion:
//
//    The normal form of a plane in 3D is:
//
//      PP is a point on the plane,
//      NORMAL is a normal vector to the plane.
//
//    Two vectors PQ and PR can be computed with the properties that
//    * NORMAL, PQ and PR are pairwise orthogonal;
//    * PQ and PR have unit length;
//    * every point P in the plane has a "QR" representation
//      as P = PP + q * PQ + r * PR.
//
//    This function is given the QR coordinates of a set of points on the
//    plane, and returns the XYZ coordinates.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 November 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PP[3], a point on the plane.
//
//    Input, double NORMAL[3], a normal vector N to the plane.  The
//    vector must not have zero length, but it is not necessary for N
//    to have unit length.
//
//    Input, double PQ[3], a vector of unit length,
//    perpendicular to the vector N and the vector PR.
//
//    Input, double PR[3], a vector of unit length,
//    perpendicular to the vector N and the vector PQ.
//
//    Input, integer N, the number of points on the plane.
//
//    Input, double QR[2*N], the QR coordinates of the points.
//
//    Output, double PLANE_NORMAL_QR_TO_XYZ[3*N], the XYZ coordinates of the points.
//
{
  int i;
  int j;
  double *xyz;

  xyz = new double[3*n];

  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < 3; i++ )
    {
      xyz[i+j*3] = pp[i] + pq[i] * qr[0+j*2] + pr[i] * qr[1+j*2];
    }
  }
  return xyz;
}
//****************************************************************************80

void plane_normal_tetrahedron_intersect ( double pp[3],
  double normal[3], double t[3*4], int *int_num, double pint[3*4] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_NORMAL_TETRAHEDRON_INTERSECT intersects a plane and a tetrahedron.
//
//  Discussion:
//
//    The intersection of a plane and a tetrahedron is one of:
//    0) empty
//    1) a single point
//    2) a single line segment
//    3) a triangle
//    4) a quadrilateral.
//
//    In each case, the region of intersection can be described by the
//    corresponding number of points.  In particular, cases 2, 3 and 4
//    are described by the vertices that bound the line segment, triangle,
//    or quadrilateral.
//
//    The normal form of a plane is:
//
//      PP is a point on the plane,
//      N is a normal vector to the plane.
//
//    The form of a tetrahedron is
//
//      T(1:3,1:4) contains the coordinates of the vertices.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    24 June 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PP[3], a point on the plane.
//
//    Input, double NORMAL[3], a normal vector to the plane.
//
//    Input, double T[3*4], the tetrahedron vertices.
//
//    Output, int *INT_NUM, the number of intersection
//    points returned.  This will be 0, 1, 2, 3 or 4.
//
//    Output, double PINT[3*4], the coordinates of the
//    intersection points.
//
{
  double area1;
  double area2;
  double d[4];
  double dn;
  double dpp;
  int i;
  int j;
  int j1;
  int j2;
  double temp;

  *int_num = 0;
  for ( j = 0; j < 4; j++ )
  {
    for ( i = 0; i < 3; i++ )
    {
      pint[i+j*3] = 0.0;
    }
  }
//
//  DN is the length of the normal vector.
//
  dn = sqrt ( r8vec_dot_product ( 3, normal, normal ) );
//
//  DPP is the distance between the origin and the projection of the
//  point PP onto the normal vector.
//
  dpp = dn - r8vec_dot_product ( 3, normal, pp ) / dn;
//
//  D(I) is positive, zero, or negative if vertex I is above,
//  on, or below the plane.
//
  for ( j = 0; j < 4; j++ )
  {
    d[j] = dn - dpp;
    for ( i = 0; i < 3; i++ )
    {
      d[j] = d[j] - normal[i] * t[i+j*3];
    }
  }
//
//  If all D are positive or negative, no intersection.
//
  if ( r8vec_negative_strict ( 4, d ) || r8vec_positive_strict ( 4, d ) )
  {
    *int_num = 0;
    return;
  }
//
//  Points with zero distance are automatically added to the list.
//
//  For each point with nonzero distance, seek another point
//  with opposite sign and higher index, and compute the intersection
//  of the line between those points and the plane.
//
  for ( j1 = 0; j1 < 4; j1++ )
  {
    if ( d[j1] == 0.0 )
    {
      for ( i = 0; i < 3; i++ )
      {
        pint[i+(*int_num)*3] = t[i+j1*3];
      }
      *int_num = *int_num + 1;
    }
    else
    {
      for ( j2 = j1 + 1; j2 < 4; j2++ )
      {
        if ( r8_sign_opposite_strict ( d[j1], d[j2] ) )
        {
          for ( i = 0; i < 3; i++ )
          {
            pint[i+(*int_num)*3] = ( d[j1]         * t[i+j2*3]
                                           - d[j2] * t[i+j1*3] )
                                 / ( d[j1] - d[j2] );
          }
          *int_num = *int_num + 1;
        }
      }
    }
  }
//
//  If four points were found, try to order them properly.
//
  if ( *int_num == 4 )
  {
    area1 = quad_area_3d ( pint );
    for ( i = 0; i < 3; i++ )
    {
      temp        = pint[i+3*3];
      pint[i+3*3] = pint[i+4*3];
      pint[i+4*3] = temp;
    }
    area2 = quad_area_3d ( pint );
    if ( area2 < area1 )
    {
      temp        = pint[i+3*3];
      pint[i+3*3] = pint[i+4*3];
      pint[i+4*3] = temp;
    }
  }
  return;
}
//****************************************************************************80

int plane_normal_triangle_int_3d ( double pp[3], double pn[3], double t[3*3],
  double p[3*3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_NORMAL_TRIANGLE_INT_3D: intersection ( normal plane, triangle ) in 3D.
//
//  Discussion:
//
//    The normal form of a plane in 3D is:
//
//      PP is a point on the plane,
//      PN is a normal vector to the plane.
//
//    There may be 0, 1, 2 or 3 points of intersection returned.
//
//    If two intersection points are returned, then the entire line
//    between them comprises points of intersection.
//
//    If three intersection points are returned, then all points of
//    the triangle intersect the plane.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PP[3], a point on the plane.
//
//    Input, double PN[3], a normal vector to the plane.  The
//    vector must not have zero length, but it is not necessary for PN
//    to have unit length.
//
//    Input, double T[3*3], the vertices of the triangle.
//
//    Output, double P[3*3], the coordinates of the intersection points.
//
//    Output, int PLANE_NORMAL_TRIANGLE_INT_3D, the number of intersection
//    points returned.
//
{
# define DIM_NUM 3

  double d;
  double dist1;
  double dist2;
  double dist3;
  int int_num;

  int_num = 0;
//
//  Compute the signed distances between the vertices and the plane.
//
  d = - r8vec_dot_product ( DIM_NUM, pn, pp );

  dist1 = r8vec_dot_product ( DIM_NUM, pn, t+0*3 ) + d;
  dist2 = r8vec_dot_product ( DIM_NUM, pn, t+1*3 ) + d;
  dist3 = r8vec_dot_product ( DIM_NUM, pn, t+2*3 ) + d;
//
//  Consider any zero distances.
//
  if ( dist1 == 0.0 )
  {
    r8vec_copy ( DIM_NUM, t+0*3, p+int_num*3 );
    int_num = int_num + 1;
  }

  if ( dist2 == 0.0 )
  {
    r8vec_copy ( DIM_NUM, t+1*3, p+int_num*3 );
    int_num = int_num + 1;
  }

  if ( dist3 == 0.0 )
  {
    r8vec_copy ( DIM_NUM, t+2*3, p+int_num*3 );
    int_num = int_num + 1;
  }
//
//  If 2 or 3 of the nodes intersect, we're already done.
//
  if ( 2 <= int_num )
  {
    return int_num;
  }
//
//  If one node intersects, then we're done unless the other two
//  are of opposite signs.
//
  if ( int_num == 1 )
  {
    if ( dist1 == 0.0 )
    {
      plane_imp_triangle_int_add_3d ( t+1*3, t+2*3, dist2, dist3, &int_num, p );
    }
    else if ( dist2 == 0.0 )
    {
      plane_imp_triangle_int_add_3d ( t+0*3, t+2*3, dist1, dist3, &int_num, p );
    }
    else if ( dist3 == 0.0 )
    {
      plane_imp_triangle_int_add_3d ( t+0*3, t+1*3, dist1, dist2, &int_num, p );
    }
    return int_num;
  }
//
//  All nodal distances are nonzero, and there is at least one
//  positive and one negative.
//
  if ( dist1 * dist2 < 0.0 && dist1 * dist3 < 0.0 )
  {
    plane_imp_triangle_int_add_3d ( t+0*3, t+1*3, dist1, dist2, &int_num, p );

    plane_imp_triangle_int_add_3d ( t+0*3, t+2*3, dist1, dist3, &int_num, p );
  }
  else if ( dist2 * dist1 < 0.0 && dist2 * dist3 < 0.0 )
  {
    plane_imp_triangle_int_add_3d ( t+1*3, t+0*3, dist2, dist1, &int_num, p );

    plane_imp_triangle_int_add_3d ( t+1*3, t+2*3, dist2, dist3, &int_num, p );
  }
  else if ( dist3 * dist1 < 0.0 && dist3 * dist2 < 0.0 )
  {
    plane_imp_triangle_int_add_3d ( t+2*3, t+0*3, dist3, dist1, &int_num, p );

    plane_imp_triangle_int_add_3d ( t+2*3, t+1*3, dist3, dist2, &int_num, p );
  }

  return int_num;
# undef DIM_NUM
}
//****************************************************************************80

void plane_normal_uniform_3d ( int &seed, double pp[3], double normal[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_NORMAL_UNIFORM_3D generates a random normal plane in 3D.
//
//  Discussion:
//
//    The normal form of a plane is:
//
//      PP is a point on the plane,
//      N is a normal vector to the plane.
//
//    The point PP will be chosen at random inside the unit sphere.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    26 November 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double PP[3], a point on the plane.
//
//    Output, double NORMAL[3], the unit normal vector.
//
{
# define DIM_NUM 3

  int i;
  double norm;
  double *v;
//
//  Pick PP as a random point inside the unit sphere in ND.
//
  v = ball01_sample_3d ( seed );
  r8vec_copy ( DIM_NUM, v, pp );
  delete [] v;
//
//  Get values from a standard normal distribution.
//
  v = r8vec_normal_01_new ( DIM_NUM, seed );
  r8vec_copy ( DIM_NUM, v, normal );
  delete [] v;
//
//  Compute the length of the vector.
//
  norm = r8vec_norm ( DIM_NUM, normal );
//
//  Normalize the vector.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    normal[i] = normal[i] / norm;
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

void plane_normal_uniform_nd ( int dim_num, int &seed, double pp[],
  double normal[] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_NORMAL_UNIFORM_ND generates a random normal plane in ND.
//
//  Discussion:
//
//    The normal form of a plane is:
//
//      PP is a point on the plane,
//      N is a normal vector to the plane.
//
//    The point PP will be chosen at random inside the unit sphere.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    26 November 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double PP[DIM_NUM], a point on the plane.
//
//    Output, double NORMAL[DIM_NUM], the unit normal vector.
//
{
  int i;
  double norm;
  double *v;
//
//  Pick PP as a random point inside the unit sphere in ND.
//
  v = ball01_sample_nd ( dim_num, seed );
  r8vec_copy ( dim_num, v, pp );
  delete [] v;
//
//  Get values from a standard normal distribution.
//
  v = r8vec_normal_01_new ( dim_num, seed );
  r8vec_copy ( dim_num, v, normal );
  delete [] v;
//
//  Compute the length of the vector.
//
  norm = r8vec_norm ( dim_num, normal );
//
//  Normalize the vector.
//
  for ( i = 0; i < dim_num; i++ )
  {
    normal[i] = normal[i] / norm;
  }

  return;
}
//****************************************************************************80

double *plane_normal_xyz_to_qr ( double pp[], double normal[], double pq[],
  double pr[], int n, double xyz[] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_NORMAL_XYZ_TO_QR: XYZ to QR coordinates for a normal form plane.
//
//  Discussion:
//
//    The normal form of a plane in 3D is:
//
//      PP is a point on the plane,
//      NORMAL is a normal vector to the plane.
//
//    Two vectors PQ and PR can be computed with the properties that
//    * NORMAL, PQ and PR are pairwise orthogonal;
//    * PQ and PR have unit length;
//    * every point P in the plane has a "QR" representation
//      as P = PP + q * PQ + r * PR.
//
//    This function is given the XYZ coordinates of a set of points on the
//    plane, and returns the QR coordinates.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 November 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PP[3], a point on the plane.
//
//    Input, double NORMAL[3], a normal vector N to the plane.  The
//    vector must not have zero length, but it is not necessary for N
//    to have unit length.
//
//    Input, double PQ[3], a vector of unit length,
//    perpendicular to the vector N and the vector PR.
//
//    Input, double PR[3], a vector of unit length,
//    perpendicular to the vector N and the vector PQ.
//
//    Input, int N, the number of points on the plane.
//
//    Input, double XYZ[3*N], the XYZ coordinates of the points.
//
//    Output, double PLANE_NORMAL_XYZ_TO_QR[2*N], the QR coordinates
//    of the points.
//
{
  int j;
  double *qr;

  qr = new double[2*n];

  for ( j = 0; j < n; j++ )
  {
    qr[0+j*2] = pq[0] * ( xyz[0+j*3] - pp[0] )
              + pq[1] * ( xyz[1+j*3] - pp[1] )
              + pq[2] * ( xyz[2+j*3] - pp[2] );
    qr[1+j*2] = pr[0] * ( xyz[0+j*2] - pp[0] )
              + pr[1] * ( xyz[1+j*3] - pp[1] )
              + pr[2] * ( xyz[2+j*3] - pp[2] );
  }

  return qr;
}
//****************************************************************************80

void plane_normal2exp_3d ( double pp[3], double pn[3], double p1[3],
  double p2[3], double p3[3] )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_NORMAL2EXP_3D converts a normal plane to explicit form in 3D.
//
//  Discussion:
//
//    The normal form of a plane in 3D is
//
//      PP, a point on the plane, and
//      PN, the unit normal to the plane.
//
//    The explicit form of a plane in 3D is
//
//      the plane through P1, P2 and P3.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PP(3), a point on the plane.
//
//    Input, double PN[3], a normal vector N to the plane.  The
//    vector must not have zero length, but it is not necessary for N
//    to have unit length.
//
//    Output, double P1[3], P2[3], P3[3], three points that lie on the plane.
//
{
# define DIM_NUM 3

  double pq[DIM_NUM];
  double pr[DIM_NUM];

  plane_normal_basis_3d ( pp, pn, pq, pr );

  p1[0] = pp[0];
  p1[1] = pp[1];
  p1[2] = pp[2];

  p2[0] = pp[0] + pq[0];
  p2[1] = pp[1] + pq[1];
  p2[2] = pp[2] + pq[2];

  p3[0] = pp[0] + pr[0];
  p3[1] = pp[1] + pr[1];
  p3[2] = pp[2] + pr[2];

  return;
# undef DIM_NUM
}
//****************************************************************************80

void plane_normal2imp_3d ( double pp[3], double pn[3], double *a, double *b,
  double *c, double *d )

//****************************************************************************80
//
//  Purpose:
//
//    PLANE_NORMAL2IMP_3D converts a normal form plane to implicit form in 3D.
//
//  Discussion:
//
//    The normal form of a plane in 3D is
//
//      PP, a point on the plane, and
//      PN, the unit normal to the plane.
//
//    The implicit form of a plane in 3D is
//
//      A * X + B * Y + C * Z + D = 0.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PP[3], a point on the plane.
//
//    Input, double PN[3], the unit normal vector to the plane.
//
//    Output, double *A, *B, *C, *D, parameters that define the implicit plane.
//
{
# define DIM_NUM 3

  *a = pn[0];
  *b = pn[1];
  *c = pn[2];

  *d = -r8vec_dot_product ( DIM_NUM, pn, pp );

  return;
# undef DIM_NUM
}
//****************************************************************************80

double planes_imp_angle_3d ( double a1, double b1, double c1, double d1,
  double a2, double b2, double c2, double d2 )

//****************************************************************************80
//
//  Purpose:
//
//    PLANES_IMP_ANGLE_3D: dihedral angle between implicit planes in 3D.
//
//  Discussion:
//
//    The implicit form of a plane in 3D is:
//
//      A * X + B * Y + C * Z + D = 0
//
//    If two planes P1 and P2 intersect in a nondegenerate way, then there is a
//    line of intersection L0.  Consider any plane perpendicular to L0.  The
//    dihedral angle of P1 and P2 is the angle between the lines L1 and L2, where
//    L1 is the intersection of P1 and P0, and L2 is the intersection of P2
//    and P0.
//
//    The dihedral angle may also be calculated as the angle between the normal
//    vectors of the two planes.  Note that if the planes are parallel or
//    coincide, the normal vectors are identical, and the dihedral angle is 0.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Daniel Zwillinger, editor,
//    CRC Standard Math Tables and Formulae, 30th edition,
//    Section 4.13, "Planes",
//    CRC Press, 1996, pages 305-306.
//
//  Parameters:
//
//    Input, double A1, B1, C1, D1, coefficients that define the first plane.
//
//    Input, double A2, B2, C2, D2, coefficients that define the second plane.
//
//    Output, double PLANES_IMP_ANGLE_3D, the dihedral angle, in radians,
//    defined by the two planes.  If either plane is degenerate, or they do
//    not intersect, or they coincide, then the angle is set to HUGE_VAL.
//    Otherwise, the angle is between 0 and PI.
//
{
  double cosine;
  double norm1;
  double norm2;
  double value;

  norm1 = sqrt ( a1 * a1 + b1 * b1 + c1 * c1 );

  if ( norm1 == 0.0 )
  {
    value = HUGE_VAL;
    return value;
  }

  norm2 = sqrt ( a2 * a2 + b2 * b2 + c2 * c2 );

  if ( norm2 == 0.0 )
  {
    value = HUGE_VAL;
    return value;
  }

  cosine = ( a1 * a2 + b1 * b2 + c1 * c2 ) / ( norm1 * norm2 );

  value = acos ( cosine );
  return value;
}
//****************************************************************************80

bool points_avoid_point_naive_2d ( int n, double pset[], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_AVOID_POINT_NAIVE_2D finds if a point is "far enough" from a set of points in 2D.
//
//  Discussion:
//
//    The routine discards points that are too close to other points.
//    The method used to check this is quadratic in the number of points,
//    and may take an inordinate amount of time if there are a large
//    number of points.  But in that case, what do you want?  If you want
//    lots of points, you don't want to delete any because it won't matter.
//
//    The test point is "far enough" from an accepted point if
//    the Euclidean distance is at least 100 times EPSILON.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of accepted points.
//
//    Input, double PSET[2*N], the accepted points.  The points are stored
//    in a one dimensional array, beginning with the X and Y coordinates of
//    the first point, and so on.
//
//    Input, double P[2], a point to be tested.
//
//    Output, bool POINTS_AVOID_POINT_NAIVE_2D, is TRUE if P is
//    "far enough" from all the accepted points.
//
{
  int j;
  double normsq;
  double tolsq;

  tolsq = 100.0 * DBL_EPSILON;
  tolsq = tolsq * tolsq;

  for ( j = 0; j < n; j++ )
  {
    normsq = ( pset[0+j*2] - p[0] ) * ( pset[0+j*2] - p[0] )
           + ( pset[1+j*2] - p[1] ) * ( pset[1+j*2] - p[1] );

    if ( normsq < tolsq )
    {
      return false;
    }
  }

  return true;
}
//****************************************************************************80

void points_bisect_line_imp_2d ( double p1[2], double p2[2], double *a,
  double *b, double *c )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_BISECT_LINE_IMP_2D finds the implicit line bisecting the line between two points in 2D.
//
//  Discussion:
//
//    The implicit form of a line in 2D is:
//
//      A * X + B * Y + C = 0
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double P1[2], P2[2], the coordinates of two points.
//
//    Output, double *A, *B, *C, the parameters of the implicit line
//    equidistant from both points.
//
{
  *a = p1[0] - p2[0];
  *b = p1[1] - p2[1];
  *c = - 0.5 * ( ( p1[0] * p1[0] + p1[1] * p1[1] )
               - ( p2[0] * p2[0] + p2[1] * p2[1] ) );

  return;
}
//****************************************************************************80

void points_bisect_line_par_2d ( double p1[2], double p2[2], double *f,
  double *g, double *x, double *y )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_BISECT_LINE_PAR_2D finds the parametric line bisecting the line between two points in 2D.
//
//  Discussion:
//
//    The parametric form of a line in 2D is:
//
//      X = X0 + F * T
//      Y = Y0 + G * T
//
//    For normalization, we choose F*F+G*G = 1 and 0 <= F.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double P1[2], P2[2], the coordinates of two points.
//
//    Output, double *F, *G, *X, *Y, the parameters of the parametric line
//    equidistant from both points.
//
{
  double norm;

  *f = 0.5 * ( p1[0] + p2[0] );
  *g = 0.5 * ( p1[1] + p2[1] );

  norm = sqrt ( pow ( *f, 2 ) + pow ( *g, 2 ) );

  if ( 0.0 < norm )
  {
    *f = *f / norm;
    *g = *g / norm;
  }

  if ( *f < 0.0 )
  {
    *f = - ( *f );
    *g = - ( *g );
  }
  *x = - ( p2[1] - p1[1] );
  *y =   ( p2[0] - p1[0] );

  return;
}
//****************************************************************************80

int points_centroid_2d ( int n, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_CENTROID_2D computes the discrete centroid of a point set in 2D.
//
//  Discussion:
//
//    Given a discrete set of points S, the discrete centroid z is defined by
//
//                           Sum ( x in S ) ( x - z )^2
//        = min ( y in S ) { Sum ( x in S ) ( x - y )^2
//
//    In other words, the discrete centroid is a point in the set whose distance
//    to the other points is minimized.  The discrete centroid of a point set
//    need not be unique.  Consider a point set that comprises the
//    vertices of an equilateral triangle.
//
//    This discrete centroid may also be referred to as the K-means cluster.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of points.
//
//    Input, double P[2*N], the coordinates of the points.
//
//    Output, int POINTS_CENTROID_2D, the index of a discrete
//    centroid of the set, between 0 and N-1.
//
{
  int cent;
  double dist;
  double dist_min;
  int i;
  int j;

  dist_min = 0.0;
  cent = -1;

  for ( i = 0; i < n; i++ )
  {
    dist = 0.0;
    for ( j = 0; j < n; j++ )
    {
      dist = dist + ( p[0+i*2] - p[0+j*2] ) * ( p[0+i*2] - p[0+j*2] )
                  + ( p[1+i*2] - p[1+j*2] ) * ( p[1+i*2] - p[1+j*2] );
    }

    if ( i == 0 )
    {
      dist_min = dist;
      cent = i;
    }
    else if ( dist < dist_min )
    {
      dist_min = dist;
      cent = i;
    }
  }

  return cent;
}
//****************************************************************************80

double points_colin_2d ( double p1[2], double p2[2], double p3[2] )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_COLIN_2D estimates the colinearity of 3 points in 2D.
//
//  Discussion:
//
//    The estimate of collinearity is the ratio of the area of the triangle
//    spanned by the points to the area of the equilateral triangle with the
//    same perimeter.
//
//    This is 1.0 if the points are maximally noncolinear, 0.0 if the
//    points are exactly colinear, and otherwise is closer to 1 or 0 depending
//    on whether the points are far or close to colinearity.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], P3[2], the coordinates of the points.
//
//    Output, double POINTS_COLIN_2D, an estimate of colinearity,
//
{
# define DIM_NUM 2

  double area_triangle;
  double area2;
  double colin;
  double perim;
  double s12;
  double s23;
  double s31;
  double side;
  double t[DIM_NUM*3];

  t[0+0*2] = p1[0];
  t[1+0*2] = p1[1];
  t[0+1*2] = p2[0];
  t[1+1*2] = p2[1];
  t[0+2*2] = p3[0];
  t[1+2*2] = p3[1];

  area_triangle = triangle_area_2d ( t );

  if ( area_triangle == 0.0 )
  {
    colin = 0.0;
  }
  else
  {
    s12 = sqrt ( pow ( p2[0] - p1[0], 2 ) + pow ( p2[1] - p1[1], 2 ) );
    s23 = sqrt ( pow ( p3[0] - p2[0], 2 ) + pow ( p3[1] - p2[1], 2 ) );
    s31 = sqrt ( pow ( p1[0] - p3[0], 2 ) + pow ( p1[1] - p3[1], 2 ) );

    perim = s12 + s23 + s31;

    side = perim / 3.0;

    area2 = 0.25 * sqrt ( 3.0 ) * side * side;

    colin = fabs ( area_triangle ) / area2;
  }

  return colin;
# undef DIM_NUM
}
//****************************************************************************80

double points_colin_3d ( double p1[3], double p2[3], double p3[3] )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_COLIN_3D estimates the colinearity of 3 points in 3D.
//
//  Discussion:
//
//    The estimate of collinearity is the ratio of the area of the triangle
//    spanned by the points to the area of the equilateral triangle with the
//    same perimeter.
//
//    This is 1.0 if the points are maximally noncolinear, 0.0 if the
//    points are exactly colinear, and otherwise is closer to 1 or 0 depending
//    on whether the points are far or close to colinearity.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], the points.
//
//    Output, double POINTS_COLIN_3D, an estimate of colinearity.
//
{
# define DIM_NUM 3

  double area_triangle;
  double area2;
  double colin;
  double perim;
  double s12;
  double s23;
  double s31;
  double side;
  double t[DIM_NUM*3];

  t[0+0*3] = p1[0];
  t[1+0*3] = p1[1];
  t[2+0*3] = p1[2];
  t[0+1*3] = p2[0];
  t[1+1*3] = p2[1];
  t[2+1*3] = p2[2];
  t[0+2*3] = p3[0];
  t[1+2*3] = p3[1];
  t[2+2*3] = p3[2];

  area_triangle = triangle_area_3d ( t );

  if ( area_triangle == 0.0 )
  {
    colin = 0.0;
  }
  else
  {
    s12 = sqrt ( pow ( p2[0] - p1[0], 2 )
               + pow ( p2[1] - p1[1], 2 )
               + pow ( p2[2] - p1[2], 2 ) );
    s23 = sqrt ( pow ( p3[0] - p2[0], 2 )
               + pow ( p3[1] - p2[1], 2 )
               + pow ( p3[2] - p2[2], 2 ) );
    s31 = sqrt ( pow ( p1[0] - p3[0], 2 )
               + pow ( p1[1] - p3[1], 2 )
               + pow ( p1[2] - p3[2], 2 ) );

    perim = s12 + s23 + s31;

    side = perim / 3.0;

    area2 = 0.25 * sqrt ( 3.0 ) * side * side;

    colin = fabs ( area_triangle ) / area2;
  }

  return colin;
# undef DIM_NUM
}
//****************************************************************************80

double points_dist_2d ( double p1[2], double p2[2] )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_DIST_2D finds the distance between two points in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], two points.
//
//    Output, double POINTS_DIST_2D, the distance between the points.
//
{
  double dist;

  dist = sqrt ( pow ( p1[0] - p2[0], 2 )
              + pow ( p1[1] - p2[1], 2 ) );

  return dist;
}
//****************************************************************************80

double points_dist_3d ( double p1[3], double p2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_DIST_3D finds the distance between two points in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], two points.
//
//    Output, double POINTS_DIST_3D, the distance between the points.
//
{
  double dist;

  dist = sqrt ( pow ( p1[0] - p2[0], 2 )
              + pow ( p1[1] - p2[1], 2 )
              + pow ( p1[2] - p2[2], 2 ) );

  return dist;
}
//****************************************************************************80

double points_dist_nd ( int dim_num, double p1[], double p2[] )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_DIST_ND finds the distance between two points in ND.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input, double P1[DIM_NUM], P2[DIM_NUM], the coordinates of two points.
//
//    Output, double POINTS_DIST_ND, the distance between the points.
//
{
  double dist;
  int i;

  dist = 0.0;
  for ( i = 0; i < dim_num; i++ )
  {
    dist = dist + ( p1[i] - p2[i] ) * ( p1[i] - p2[i] );
  }

  dist = sqrt ( dist );

  return dist;
}
//****************************************************************************80

void points_hull_2d ( int node_num, double node_xy[], int *hull_num,
  int hull[] )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_HULL_2D computes the convex hull of a set of nodes in 2D.
//
//  Discussion:
//
//    The work involved is N*log(H), where N is the number of points, and H is
//    the number of points that are on the hull.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    25 June 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int NODE_NUM, the number of nodes.
//
//    Input, double NODE_XY[2*NODE_NUM], the coordinates of the nodes.
//
//    Output, int *HULL_NUM, the number of nodes that lie on the convex hull.
//
//    Output, int HULL[NODE_NUM].  The first HULL_NUM entries contain
//    the indices of the nodes that form the convex hull, in order.
//    These indices are 1-based, not 0-based!
//
{
  double angle;
  double angle_max;
  double di;
  double dr;
  int first;
  int i;
  double p_xy[2];
  int q;
  double q_xy[2];
  int r;
  double r_xy[2];

  *hull_num = 0;

  if ( node_num < 1 )
  {
    return;
  }
//
//  If NODE_NUM = 1, the hull is the node.
//
  if ( node_num == 1 )
  {
    hull[*hull_num] = 1;
    *hull_num = *hull_num + 1;
    return;
  }
//
//  If NODE_NUM = 2, then the convex hull is either the two distinct nodes,
//  or possibly a single (repeated) node.
//
  if ( node_num == 2 )
  {
    hull[*hull_num] = 1;
    *hull_num = *hull_num + 1;

    if ( node_xy[0+0*2] != node_xy[0+1*2] || node_xy[1+0*2] != node_xy[1+1*2] )
    {
      hull[*hull_num] = 2;
      *hull_num = *hull_num + 1;
    }

    return;
  }
//
//  Find the leftmost point, and take the bottom-most in a tie.
//  Call it "Q".
//
  q = 1;
  for ( i = 2; i <= node_num; i++ )
  {
    if ( node_xy[0+(i-1)*2] < node_xy[0+(q-1)*2] ||
      ( node_xy[0+(i-1)*2] == node_xy[0+(q-1)*2] &&
        node_xy[1+(i-1)*2] < node_xy[1+(q-1)*2] ) )
    {
      q = i;
    }
  }

  q_xy[0] = node_xy[0+(q-1)*2];
  q_xy[1] = node_xy[1+(q-1)*2];
//
//  Remember the starting point.
//
  first = q;
  hull[*hull_num] = q;
  *hull_num = *hull_num + 1;
//
//  For the first point, make a dummy previous point, 1 unit south,
//  and call it "P".
//
  p_xy[0] = q_xy[0];
  p_xy[1] = q_xy[1] - 1.0;
//
//  Now, having old point P, and current point Q, find the new point R
//  so the angle PQR is maximal.
//
//  Watch out for the possibility that the two nodes are identical.
//
  for ( ; ; )
  {
    r = 0;
    angle_max = 0.0;

    for ( i = 1; i <= node_num; i++ )
    {
      if ( i != q && ( node_xy[0+(i-1)*2] != q_xy[0] || node_xy[1+(i-1)*2] != q_xy[1] ) )
      {
        angle = angle_rad_2d ( p_xy, q_xy, node_xy+(i-1)*2 );

        if ( r == 0 || angle_max < angle )
        {
          r = i;
          r_xy[0] = node_xy[0+(r-1)*2];
          r_xy[1] = node_xy[1+(r-1)*2];
          angle_max = angle;
        }
//
//  In case of ties, choose the nearer point.
//
        else if ( r != 0 && angle == angle_max )
        {
          di = sqrt ( pow ( node_xy[0+(i-1)*2] - q_xy[0], 2 )
                    + pow ( node_xy[1+(i-1)*2] - q_xy[1], 2 ) );

          dr = sqrt ( pow ( r_xy[0] - q_xy[0], 2 )
                    + pow ( r_xy[1] - q_xy[1], 2 ) );

          if ( di < dr )
          {
            r = i;
            r_xy[0] = node_xy[0+(r-1)*2];
            r_xy[1] = node_xy[1+(r-1)*2];
            angle_max = angle;
          }
        }
      }
    }
//
//  If we've returned to our starting node, exit.
//
    if ( r == first )
    {
      break;
    }

    if ( node_num < *hull_num + 1 )
    {
      cerr << "\n";
      cerr << "POINTS_HULL_2D - Fatal error!\n";
      cerr << "  The algorithm failed.\n";
      exit ( 1 );
    }
//
//  Add point R to the convex hull.
//
    hull[*hull_num] = r;
    *hull_num = *hull_num + 1;
//
//  Set Q := P, P := R, and repeat.
//
    q = r;

    p_xy[0] = q_xy[0];
    p_xy[1] = q_xy[1];

    q_xy[0] = r_xy[0];
    q_xy[1] = r_xy[1];
  }

  return;
}
//****************************************************************************80

void points_plot ( string file_name, int node_num, double node_xy[],
  bool node_label )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_PLOT plots a pointset.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 March 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, string FILE_NAME, the name of the file to create.
//
//    Input, int NODE_NUM, the number of nodes.
//
//    Input, double NODE_XY[2*NODE_NUM], the nodes.
//
//    Input, bool NODE_LABEL, is TRUE if the nodes are to be labeled.
//
//  Local parameters:
//
//    int CIRCLE_SIZE, controls the size of the circles depicting
//    the nodes.  Currently set to 5.  3 is pretty small, and 1 is
//    barely visible.
//
{
  int circle_size = 3;
  int delta;
  ofstream file_unit;
  int node;
  double x_max;
  double x_min;
  int x_ps;
  int x_ps_max = 576;
  int x_ps_max_clip = 594;
  int x_ps_min = 36;
  int x_ps_min_clip = 18;
  double x_scale;
  double y_max;
  double y_min;
  int y_ps;
  int y_ps_max = 666;
  int y_ps_max_clip = 684;
  int y_ps_min = 126;
  int y_ps_min_clip = 108;
  double y_scale;
//
//  We need to do some figuring here, so that we can determine
//  the range of the data, and hence the height and width
//  of the piece of paper.
//
  x_max = -HUGE_VAL;
  for ( node = 0; node < node_num; node++ )
  {
     if ( x_max < node_xy[0+node*2] )
     {
       x_max = node_xy[0+node*2];
     }
  }
  x_min = HUGE_VAL;
  for ( node = 0; node < node_num; node++ )
  {
     if ( node_xy[0+node*2] < x_min )
     {
       x_min = node_xy[0+node*2];
     }
  }
  x_scale = x_max - x_min;

  x_max = x_max + 0.05 * x_scale;
  x_min = x_min - 0.05 * x_scale;
  x_scale = x_max - x_min;

  y_max = -HUGE_VAL;
  for ( node = 0; node < node_num; node++ )
  {
     if ( y_max < node_xy[1+node*2] )
     {
       y_max = node_xy[1+node*2];
     }
  }
  y_min = HUGE_VAL;
  for ( node = 0; node < node_num; node++ )
  {
     if ( node_xy[1+node*2] < y_min )
     {
       y_min = node_xy[1+node*2];
     }
  }
  y_scale = y_max - y_min;

  y_max = y_max + 0.05 * y_scale;
  y_min = y_min - 0.05 * y_scale;
  y_scale = y_max - y_min;

  if ( x_scale < y_scale )
  {
    delta = r8_nint ( ( double ) ( x_ps_max - x_ps_min )
      * ( y_scale - x_scale ) / ( 2.0 * y_scale ) );

    x_ps_max = x_ps_max - delta;
    x_ps_min = x_ps_min + delta;

    x_ps_max_clip = x_ps_max_clip - delta;
    x_ps_min_clip = x_ps_min_clip + delta;

    x_scale = y_scale;
  }
  else if ( y_scale < x_scale )
  {
    delta = r8_nint ( ( double ) ( y_ps_max - y_ps_min )
      * ( x_scale - y_scale ) / ( 2.0 * x_scale ) );

    y_ps_max = y_ps_max - delta;
    y_ps_min = y_ps_min + delta;

    y_ps_max_clip = y_ps_max_clip - delta;
    y_ps_min_clip = y_ps_min_clip + delta;

    y_scale = x_scale;
  }

  file_unit.open ( file_name.c_str ( ) );

  if ( !file_unit )
  {
    cerr << "\n";
    cerr << "POINTS_PLOT - Fatal error!\n";
    cerr << "  Could not open the output EPS file.\n";
    exit ( 1 );
  }

  file_unit << "%!PS-Adobe-3.0 EPSF-3.0\n";
  file_unit << "%%Creator: points_plot.C\n";
  file_unit << "%%Title: " << file_name << "\n";

  file_unit << "%%Pages: 1\n";
  file_unit << "%%BoundingBox:  "
    << x_ps_min << "  "
    << y_ps_min << "  "
    << x_ps_max << "  "
    << y_ps_max << "\n";
  file_unit << "%%Document-Fonts: Times-Roman\n";
  file_unit << "%%LanguageLevel: 1\n";
  file_unit << "%%EndComments\n";
  file_unit << "%%BeginProlog\n";
  file_unit << "/inch {72 mul} def\n";
  file_unit << "%%EndProlog\n";
  file_unit << "%%Page:      1     1\n";
  file_unit << "save\n";
  file_unit << "%\n";
  file_unit << "% Set the RGB line color to very light gray.\n";
  file_unit << "%\n";
  file_unit << " 0.9000 0.9000 0.9000 setrgbcolor\n";
  file_unit << "%\n";
  file_unit << "% Draw a gray border around the page.\n";
  file_unit << "%\n";
  file_unit << "newpath\n";
  file_unit << x_ps_min << "  "
            << y_ps_min << "  moveto\n";
  file_unit << x_ps_max << "  "
            << y_ps_min << "  lineto\n";
  file_unit << x_ps_max << "  "
            << y_ps_max << "  lineto\n";
  file_unit << x_ps_min << "  "
            << y_ps_max << "  lineto\n";
  file_unit << x_ps_min << "  "
            << y_ps_min << "  lineto\n";
  file_unit << "stroke\n";
  file_unit << "%\n";
  file_unit << "% Set RGB line color to black.\n";
  file_unit << "%\n";
  file_unit << " 0.0000 0.0000 0.0000 setrgbcolor\n";
  file_unit << "%\n";
  file_unit << "%  Set the font and its size:\n";
  file_unit << "%\n";
  file_unit << "/Times-Roman findfont\n";
  file_unit << "0.50 inch scalefont\n";
  file_unit << "setfont\n";
  file_unit << "%\n";
  file_unit << "%  Print a title:\n";
  file_unit << "%\n";
  file_unit << "%  210  702 moveto\n";
  file_unit << "%(Pointset) show\n";
  file_unit << "%\n";
  file_unit << "% Define a clipping polygon\n";
  file_unit << "%\n";
  file_unit << "newpath\n";
  file_unit << x_ps_min_clip << "  "
            << y_ps_min_clip << "  moveto\n";
  file_unit << x_ps_max_clip << "  "
            << y_ps_min_clip << "  lineto\n";
  file_unit << x_ps_max_clip << "  "
            << y_ps_max_clip << "  lineto\n";
  file_unit << x_ps_min_clip << "  "
            << y_ps_max_clip << "  lineto\n";
  file_unit << x_ps_min_clip << "  "
            << y_ps_min_clip << "  lineto\n";
  file_unit << "clip newpath\n";
//
//  Draw the nodes.
//
  file_unit << "%\n";
  file_unit << "%  Draw filled dots at each node:\n";
  file_unit << "%\n";
  file_unit << "%  Set the color to blue:\n";
  file_unit << "%\n";
  file_unit << "0.000  0.150  0.750  setrgbcolor\n";
  file_unit << "%\n";

  for ( node = 0; node < node_num; node++ )
  {
    x_ps = ( int ) (
      ( ( x_max - node_xy[0+node*2]         ) * ( double ) ( x_ps_min )
      + (       + node_xy[0+node*2] - x_min ) * ( double ) ( x_ps_max ) )
      / ( x_max                     - x_min ) );

    y_ps = ( int ) (
      ( ( y_max - node_xy[1+node*2]         ) * ( double ) ( y_ps_min )
      + (         node_xy[1+node*2] - y_min ) * ( double ) ( y_ps_max ) )
      / ( y_max                     - y_min ) );

    file_unit << "newpath  "
      << x_ps << "  "
      << y_ps << "  "
      << circle_size << " 0 360 arc closepath fill\n";
  }
//
//  Label the nodes.
//
  file_unit << "%\n";
  file_unit << "%  Label the nodes:\n";
  file_unit << "%\n";
  file_unit << "%  Set the color to darker blue:\n";
  file_unit << "%\n";
  file_unit << "0.000  0.250  0.850  setrgbcolor\n";
  file_unit << "/Times-Roman findfont\n";
  file_unit << "0.20 inch scalefont\n";
  file_unit << "setfont\n";

  file_unit << "%\n";

  for ( node = 0; node < node_num; node++ )
  {
    x_ps = ( int ) (
      ( ( x_max - node_xy[0+node*2]         ) * ( double ) ( x_ps_min )
      + (       + node_xy[0+node*2] - x_min ) * ( double ) ( x_ps_max ) )
      / ( x_max                     - x_min ) );

    y_ps = ( int ) (
      ( ( y_max - node_xy[1+node*2]         ) * ( double ) ( y_ps_min )
      + (         node_xy[1+node*2] - y_min ) * ( double ) ( y_ps_max ) )
      / ( y_max                     - y_min ) );

    file_unit << "newpath  "
      << x_ps     << "  "
      << y_ps + 5 << "  moveto ("
      << node+1   << ") show\n";
  }

  file_unit << "%\n";
  file_unit << "restore showpage\n";
  file_unit << "%\n";
  file_unit << "% End of page\n";
  file_unit << "%\n";
  file_unit << "%%Trailer\n";
  file_unit << "%%EOF\n";

  file_unit.close ( );

  return;
}
//****************************************************************************80

int points_point_near_naive_2d ( int nset, double pset[], double ptest[],
  double *d_min )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_POINT_NEAR_NAIVE_2D finds the nearest point to a given point in 2D.
//
//  Discussion:
//
//    A naive algorithm is used.  The distance to every point is calculated,
//    in order to determine the smallest.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 October 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int NSET, the number of points in the set.
//
//    Input, double PSET[2*NSET], the coordinates of the points in the set.
//
//    Input, double PTEST[2], the point whose nearest neighbor is sought.
//
//    Output, double *D_MIN, the distance between P and PSET(*,I_MIN).
//
//    Output, int POINTS_POINT_NEAR_NAIVE_2D, the index of the nearest
//    point in PSET to P.
//
{
# define DIM_NUM 2

  double d;
  int i;
  int j;
  int p_min;

  *d_min = HUGE_VAL;
  p_min = 0;

  for ( j = 0; j < nset; j++ )
  {
    d = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      d = d + pow ( ptest[i] - pset[i+j*DIM_NUM], 2 );
    }
    if ( d < *d_min )
    {
      *d_min = d;
      p_min = j;
    }
  }

  *d_min = sqrt ( *d_min );

  return p_min;

# undef DIM_NUM
}
//****************************************************************************80

int points_point_near_naive_3d ( int nset, double pset[], double ptest[],
  double *d_min )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_POINT_NEAR_NAIVE_3D finds the nearest point to a given point in 3D.
//
//  Discussion:
//
//    A naive algorithm is used.  The distance to every point is calculated,
//    in order to determine the smallest.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 October 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int NSET, the number of points in the set.
//
//    Input, double PSET[3*NSET], the coordinates of the points in the set.
//
//    Input, double PTEST[3], the point whose nearest neighbor is sought.
//
//    Output, double *D_MIN, the distance between P and PSET(*,I_MIN).
//
//    Output, int POINTS_POINT_NEAR_NAIVE_3D, the index of the nearest
//    point in PSET to P.
//
{
# define DIM_NUM 3

  double d;
  int i;
  int j;
  int p_min;

  *d_min = HUGE_VAL;
  p_min = 0;

  for ( j = 0; j < nset; j++ )
  {
    d = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      d = d + pow ( ptest[i] - pset[i+j*DIM_NUM], 2 );
    }
    if ( d < *d_min )
    {
      *d_min = d;
      p_min = j;
    }
  }

  *d_min = sqrt ( *d_min );

  return p_min;

# undef DIM_NUM
}
//****************************************************************************80

int points_point_near_naive_nd ( int dim_num, int nset, double pset[],
  double ptest[], double *d_min )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_POINT_NEAR_NAIVE_ND finds the nearest point to a given point in ND.
//
//  Discussion:
//
//    A naive algorithm is used.  The distance to every point is calculated,
//    in order to determine the smallest.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 October 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int NSET, the number of points in the set.
//
//    Input, double PSET[DIM_NUM*NSET], the coordinates of the points in the set.
//
//    Input, double PTEST[DIM_NUM], the point whose nearest neighbor is sought.
//
//    Output, double *D_MIN, the distance between P and PSET(*,I_MIN).
//
//    Output, int POINTS_POINT_NEAR_NAIVE_ND, the index of the nearest
//    point in PSET to P.
//
{
  double d;
  int i;
  int j;
  int p_min;

  *d_min = HUGE_VAL;
  p_min = 0;

  for ( j = 0; j < nset; j++ )
  {
    d = 0.0;
    for ( i = 0; i < dim_num; i++ )
    {
      d = d + pow ( ptest[i] - pset[i+j*dim_num], 2 );
    }
    if ( d < *d_min )
    {
      *d_min = d;
      p_min = j;
    }
  }

  *d_min = sqrt ( *d_min );

  return p_min;
}
//****************************************************************************80

int *points_points_near_naive_2d ( int nset, double pset[], int ntest,
  double ptest[] )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_POINTS_NEAR_NAIVE_2D finds the nearest point to given points in 2D.
//
//  Discussion:
//
//    A naive algorithm is used.  The distance to every point is calculated,
//    in order to determine the smallest.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 January 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int NSET, the number of points in the set.
//
//    Input, double PSET[2*NSET], the coordinates of the points in the set.
//
//    Input, int NTEST, the number of test points.
//
//    Input, double PTEST[2*NTEST], the coordinates of the test points.
//
//    Output, int POINTS_POINTS_NEAR_NAIVE_2D[NTEST], the index of the
//    nearest point in PSET to each point in PTEST.
//
{
# define DIM_NUM 2

  double d;
  double d_min;
  int i;
  int *nearest;
  int set;
  int test;

  nearest = new int[ntest];

  for ( test = 0; test < ntest; test++ )
  {
    d_min = HUGE_VAL;
    nearest[test] = -1;

    for ( set = 0; set < nset; set++ )
    {
      d = 0.0;
      for ( i = 0; i < DIM_NUM; i++ )
      {
        d = d + pow ( ptest[i+test*DIM_NUM] - pset[i+set*DIM_NUM], 2 );
      }

      if ( d < d_min )
      {
        d_min = d;
        nearest[test] = set;
      }
    }
  }

  return nearest;
# undef DIM_NUM
}
//****************************************************************************80

int *points_points_near_naive_3d ( int nset, double pset[], int ntest,
  double ptest[] )

//****************************************************************************80
//
//  Purpose:
//
//    POINTS_POINTS_NEAR_NAIVE_3D finds the nearest point to given points in 3D.
//
//  Discussion:
//
//    A naive algorithm is used.  The distance to every point is calculated,
//    in order to determine the smallest.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 January 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int NSET, the number of points in the set.
//
//    Input, double PSET[3*NSET], the coordinates of the points in the set.
//
//    Input, int NTEST, the number of test points.
//
//    Input, double PTEST[3*NTEST], the coordinates of the test points.
//
//    Output, int POINTS_POINTS_NEAR_NAIVE_3D[NTEST], the index of the
//    nearest point in PSET to each point in PTEST.
//
{
# define DIM_NUM 3

  double d;
  double d_min;
  int i;
  int *nearest;
  int set;
  int test;

  nearest = new int[ntest];

  for ( test = 0; test < ntest; test++ )
  {
    d_min = HUGE_VAL;
    nearest[test] = -1;

    for ( set = 0; set < nset; set++ )
    {
      d = 0.0;
      for ( i = 0; i < DIM_NUM; i++ )
      {
        d = d + pow ( ptest[i+test*DIM_NUM] - pset[i+set*DIM_NUM], 2 );
      }

      if ( d < d_min )
      {
        d_min = d;
        nearest[test] = set;
      }
    }
  }

  return nearest;
# undef DIM_NUM
}
//****************************************************************************80

void polar_to_xy ( double r, double t, double xy[2] )

//****************************************************************************80
//
//  Purpose:
//
//    POLAR_TO_XY converts polar coordinates to XY coordinates.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 January 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, T, the radius and angle (in radians).
//
//    Output, double XY[2], the Cartesian coordinates.
//
{
  xy[0] = r * cos ( t );
  xy[1] = r * sin ( t );

  return;
}
//****************************************************************************80

double polygon_area_2d ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_area_2d() computes the area of a polygon in 2D.
//
//  Discussion:
//
//    AREA = ABS ( 0.5 * SUM ( 1 <= I <= N ) X(I) * ( Y(I+1)-Y(I-1) ) )
//    where Y[N] should be replaced by Y[0], and Y[N+1] by Y[1].
//
//    If the vertices are given in counter clockwise order, the area
//    will be positive.  If the vertices are given in clockwise order,
//    the area will be negative.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of vertices of the polygon.
//
//    Input, double V[2*N], the coordinates of the vertices.
//
//    Output, double POLYGON_AREA_2D, the area of the polygon.
//
{
  double area;
  int i;
  int im1;
  int ip1;

  area = 0.0;

  for ( i = 0; i < n; i++ )
  {
    im1 = i - 1;
    ip1 = i + 1;

    if ( im1 < 0 )
    {
      im1 = n - 1;
    }

    if ( n <= ip1 )
    {
      ip1 = 0;
    }

    area = area + v[0+i*2] * ( v[1+ip1*2] - v[1+im1*2] );
  }

  area = 0.5 * area;

  return area;
}
//****************************************************************************80

double polygon_area_3d ( int n, double v[], double normal[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_area_3d() computes the area of a polygon in 3D.
//
//  Discussion:
//
//    The computation is not valid unless the vertices really do lie
//    in a plane, so that the polygon that is defined is "flat".
//
//    The polygon does not have to be "regular", that is, neither its
//    sides nor its angles need to be equal.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    18 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Allen Van Gelder,
//    Efficient Computation of Polygon Area and Polyhedron Volume,
//    Graphics Gems V, edited by Alan Paeth,
//    AP Professional, 1995, T385.G6975.
//
//  Parameters:
//
//    Input, int N, the number of vertices.
//
//    Input, double V[3*N], the coordinates of the vertices.
//    The vertices should be listed in neighboring order.
//
//    Output, double NORMAL[3], the unit normal vector to the polygon.
//
//    Output, double POLYGON_AREA_3D, the area of the polygon.
//
{
# define DIM_NUM 3

  double area;
  int i;
  int ip1;

  normal[0] = 0.0;
  normal[1] = 0.0;
  normal[2] = 0.0;

  for ( i = 0; i < n; i++ )
  {
    if ( i < n - 1 )
    {
      ip1 = i + 1;
    }
    else
    {
      ip1 = 0;
    }
//
//  Compute the cross product and add it to NORMAL.
//
    normal[0] = normal[0] + v[1+i*3] * v[2+ip1*3] - v[2+i*3] * v[1+ip1*3];
    normal[1] = normal[1] + v[2+i*3] * v[0+ip1*3] - v[0+i*3] * v[2+ip1*3];
    normal[2] = normal[2] + v[0+i*3] * v[1+ip1*3] - v[1+i*3] * v[0+ip1*3];
  }

  area = r8vec_norm ( DIM_NUM, normal );

  if ( area != 0.0 )
  {
    normal[0] = normal[0] / area;
    normal[1] = normal[1] / area;
    normal[2] = normal[2] / area;
  }
  else
  {
    normal[0] = 1.0;
    normal[1] = 0.0;
    normal[2] = 0.0;
  }

  area = 0.5 * area;

  return area;
# undef DIM_NUM
}
//****************************************************************************80

double *polygon_centroid_3d ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_centroid_3d() computes the centroid of a polygon in 3D.
//
//  Discussion:
//
//    The centroid is the area-weighted sum of the centroids of
//    disjoint triangles that make up the polygon.
//
//    Thanks to Jeremy Jarrett for pointing out a typographical error
//    in an earlier version of this code.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    18 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, int N, the number of vertices of the polygon.
//
//    Input, double V[3*N], the coordinates of the vertices.
//
//    Output, double POLYGON_CENTROID_3D[3], the coordinates of the centroid.
//
{
# define DIM_NUM 3

  double area;
  double area_triangle;
  double *centroid;
  int i;
  int j;
  double t[DIM_NUM*3];

  area = 0.0;
  centroid = new double[DIM_NUM];

  for ( j = 0; j < DIM_NUM; j++ )
  {
    centroid[j] = 0.0;
  }

  for ( i = 0; i < n - 2; i++ )
  {
    t[0+0*3] = v[0+i*3];
    t[1+0*3] = v[1+i*3];
    t[2+0*3] = v[2+i*3];

    t[0+1*3] = v[0+(i+1)*3];
    t[1+1*3] = v[1+(i+1)*3];
    t[2+1*3] = v[2+(i+1)*3];

    t[0+2*3] = v[0+(n-1)*3];
    t[1+2*3] = v[1+(n-1)*3];
    t[2+2*3] = v[2+(n-1)*3];

    area_triangle = triangle_area_3d ( t );

    area = area + area_triangle;

    centroid[0] = centroid[0]
      + area_triangle * ( v[0+i*3] + v[0+(i+1)*3] + v[0+(n-1)*3] ) / 3.0;
    centroid[1] = centroid[1]
      + area_triangle * ( v[1+i*3] + v[1+(i+1)*3] + v[1+(n-1)*3] ) / 3.0;
    centroid[2] = centroid[2]
      + area_triangle * ( v[2+i*3] + v[2+(i+1)*3] + v[2+(n-1)*3] ) / 3.0;
  }

  for ( j = 0; j < DIM_NUM; j++ )
  {
    centroid[j] = centroid[j] / area;
  }

  return centroid;
# undef DIM_NUM
}
//****************************************************************************80

double *polygon_normal_3d ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_normal_3d() computes the normal vector to a polygon in 3D.
//
//  Discussion:
//
//    If the polygon is planar, then this calculation is correct.
//
//    Otherwise, the normal vector calculated is the simple average
//    of the normals defined by the planes of successive triples
//    of vertices.
//
//    If the polygon is "almost" planar, this is still acceptable.
//    But as the polygon is less and less planar, so this averaged normal
//    vector becomes more and more meaningless.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paulo Cezar Pinto Carvalho, Paulo Roma Cavalcanti,
//    Point in Polyhedron Testing Using Spherical Polygons,
//    in Graphics Gems V,
//    edited by Alan Paeth,
//    Academic Press, 1995, T385.G6975.
//
//  Parameters:
//
//    Input, int N, the number of vertices.
//
//    Input, double V[3*N], the coordinates of the vertices.
//
//    Output, double POLYGON_NORMAL_3D[3], the averaged normal vector
//    to the polygon.
//
{
# define DIM_NUM 3

  int i;
  int j;
  double *normal;
  double normal_norm;
  double *p;
  double *v1;
  double *v2;

  normal = new double[DIM_NUM];
  v1 = new double[DIM_NUM];
  v2 = new double[DIM_NUM];

  r8vec_zero ( DIM_NUM, normal );

  for ( i = 0; i < DIM_NUM; i++ )
  {
    v1[i] = v[i+1*DIM_NUM] - v[i+0*DIM_NUM];
  }

  for ( j = 2; j < n; j++ )
  {
    for ( i = 0; i < DIM_NUM; i++ )
    {
      v2[i] = v[i+j*DIM_NUM] - v[i+0*DIM_NUM];
    }

    p = r8vec_cross_product_3d ( v1, v2 );
    for ( i = 0; i < DIM_NUM; i++ )
    {
      normal[i] = normal[i] + p[i];
    }
    r8vec_copy ( DIM_NUM, v2, v1 );

    delete [] p;
  }
//
//  Normalize.
//
  normal_norm = r8vec_norm ( DIM_NUM, normal );

  if ( normal_norm != 0.0 )
  {
    for ( i = 0; i < DIM_NUM; i++ )
    {
      normal[i] = normal[i] / normal_norm;
    }
  }

  delete [] v1;
  delete [] v2;

  return normal;
# undef DIM_NUM
}
//****************************************************************************80

double polygon_solid_angle_3d ( int n, double v[], double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    POLYGON_SOLID_ANGLE_3D calculates the projected solid angle of a 3D plane polygon.
//
//  Discussion:
//
//    A point P is at the center of a unit sphere.  A planar polygon
//    is to be projected onto the surface of this sphere, by drawing
//    the ray from P to each polygonal vertex, and noting where this ray
//    intersects the sphere.
//
//    We compute the area on the sphere of the projected polygon.
//
//    Since we are projecting the polygon onto a unit sphere, the area
//    of the projected polygon is equal to the solid angle subtended by
//    the polygon.
//
//    The value returned by this routine will include a sign.  The
//    angle subtended will be NEGATIVE if the normal vector defined by
//    the polygon points AWAY from the viewing point, and will be
//    POSITIVE if the normal vector points towards the viewing point.
//
//    If the orientation of the polygon is of no interest to you,
//    then you can probably simply take the absolute value of the
//    solid angle as the information you want.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 October 2007
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paulo Cezar Pinto Carvalho, Paulo Roma Cavalcanti,
//    Point in Polyhedron Testing Using Spherical Polygons,
//    in Graphics Gems V,
//    edited by Alan Paeth,
//    Academic Press, 1995,
//    ISBN: 0125434553,
//    LC: T385.G6975.
//
//  Parameters:
//
//    Input, int N, the number of vertices.
//
//    Input, double V[3*N], the coordinates of the vertices.
//
//    Input, double P[3], the point at the center of the unit sphere.
//
//    Output, double POLYGON_SOLID_ANGLE_3D, the solid angle subtended
//    by the polygon, as projected onto the unit sphere around the point P.
//
{
# define DIM_NUM 3

  double a[DIM_NUM];
  double angle;
  double area = 0.0;
  double b[DIM_NUM];
  int j;
  int jp1;
  double *normal1;
  double normal1_norm;
  double *normal2;
  double normal2_norm;
  double r8_pi = 3.141592653589793;
  double *plane;
  double r1[DIM_NUM];
  double s;
  double value;

  if ( n < 3 )
  {
    return 0.0;
  }

  plane = polygon_normal_3d ( n, v );

  a[0] = v[0+(n-1)*DIM_NUM] - v[0+0*DIM_NUM];
  a[1] = v[1+(n-1)*DIM_NUM] - v[1+0*DIM_NUM];
  a[2] = v[2+(n-1)*DIM_NUM] - v[2+0*DIM_NUM];

  for ( j = 0; j < n; j++ )
  {
    r1[0] = v[0+j*DIM_NUM] - p[0];
    r1[1] = v[1+j*DIM_NUM] - p[1];
    r1[2] = v[2+j*DIM_NUM] - p[2];

    jp1 = i4_wrap ( j + 1, 0, n - 1 );

    b[0] = v[0+jp1*DIM_NUM] - v[0+j*DIM_NUM];
    b[1] = v[1+jp1*DIM_NUM] - v[1+j*DIM_NUM];
    b[2] = v[2+jp1*DIM_NUM] - v[2+j*DIM_NUM];

    normal1 = r8vec_cross_product_3d ( a, r1 );

    normal1_norm = r8vec_norm ( DIM_NUM, normal1 );

    normal2 = r8vec_cross_product_3d ( r1, b );

    normal2_norm = r8vec_norm ( DIM_NUM, normal2 );

    s = r8vec_dot_product ( DIM_NUM, normal1, normal2 )
      / ( normal1_norm * normal2_norm );

    angle = r8_acos ( s );

    s = r8vec_scalar_triple_product ( b, a, plane );

    if ( 0.0 < s )
    {
      area = area + r8_pi - angle;
    }
    else
    {
      area = area + r8_pi + angle;
    }
    a[0] = - b[0];
    a[1] = - b[1];
    a[2] = - b[2];

    delete [] normal1;
    delete [] normal2;
  }

  area = area - r8_pi * ( double ) ( n - 2 );

  if ( 0.0 < r8vec_dot_product ( DIM_NUM, plane, r1 ) )
  {
    value = - area;
  }
  else
  {
    value = area;
  }

  delete [] plane;

  return value;
# undef DIM_NUM
}
//****************************************************************************80

double polyhedron_area_3d ( double coord[], int order_max, int face_num,
  int node[], int node_num, int order[] )

//****************************************************************************80
//
//  Purpose:
//
//    POLYHEDRON_AREA_3D computes the surface area of a polyhedron in 3D.
//
//  Restriction:
//
//    The computation is not valid unless the faces of the polyhedron
//    are planar polygons.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Allen Van Gelder,
//    Efficient Computation of Polygon Area and Polyhedron Volume,
//    Graphics Gems V, edited by Alan Paeth,
//    AP Professional, 1995, T385.G6975.
//
//  Parameters:
//
//    Input, double COORD[NODE_NUM*3], the 3D coordinates of the vertices.
//
//    Input, int ORDER_MAX, the maximum number of vertices that make
//    up a face of the polyhedron.
//
//    Input, int FACE_NUM, the number of faces of the polyhedron.
//
//    Input, int NODE[FACE_NUM*ORDER_MAX].  Face I is defined by
//    the vertices NODE(I,0) through NODE(I,ORDER(I)-1).  These vertices
//    are listed in neighboring order.
//
//    Input, int NODE_NUM, the number of points stored in COORD.
//
//    Input, int ORDER[FACE_NUM], the number of vertices making up each face.
//
//    Output, double POLYHEDRON_AREA_3D, the surface area of the polyhedron.
//
{
# define DIM_NUM 3

  double ainc;
  double area;
  int face;
  int j;
  int k;
  double p1[DIM_NUM];
  double p2[DIM_NUM];
  double *p3;
  double p4[DIM_NUM];

  area = 0.0;
//
//  For each face
//
  for ( face = 0; face < face_num; face++ )
  {
    r8vec_zero ( DIM_NUM, p4 );
//
//  For each triangle in the face, compute the normal vector.
//
    for ( j = 0; j < order[face]; j++ )
    {
      k = node[j+face*order_max];
      p1[0] = coord[0+k*3];
      p1[1] = coord[1+k*3];
      p1[2] = coord[2+k*3];

      if ( j + 1 < order[face] )
      {
        k = node[j+1+face*order_max];
      }
      else
      {
        k = node[0+face*order_max];
      }

      p2[0] = coord[0+k*3];
      p2[1] = coord[1+k*3];
      p2[2] = coord[2+k*3];

      p3 = r8vec_cross_product_3d ( p1, p2 );

      p4[0] = p4[0] + p3[0];
      p4[1] = p4[1] + p3[1];
      p4[2] = p4[2] + p3[2];

      delete [] p3;
    }
//
//  Add the magnitude of the normal vector to the sum.
//
    ainc = r8vec_norm ( DIM_NUM, p4 );

    area = area + ainc;
  }

  area = 0.5 * area;

  return area;
# undef DIM_NUM
}
//****************************************************************************80

double *polyhedron_centroid_3d ( double coord[], int order_max, int face_num,
  int node[], int node_num, int order[] )

//****************************************************************************80
//
//  Purpose:
//
//    POLYHEDRON_CENTROID_3D computes the centroid of a polyhedron in 3D.
//
//  Discussion:
//
//    The centroid can be computed as the volume-weighted average of
//    the centroids of the tetrahedra defined by choosing a point in
//    the interior of the polyhedron, and using as a base every triangle
//    created by triangulating the faces of the polyhedron.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double COORD[NODE_NUM*3], the 3D coordinates of the vertices.
//    The vertices may be listed in any order.
//
//    Input, int ORDER_MAX, the maximum number of vertices that make
//    up a face of the polyhedron.
//
//    Input, int FACE_NUM, the number of faces of the polyhedron.
//
//    Input, int NODE[FACE_NUM*ORDER_MAX].  Face I is defined by
//    the vertices NODE(I,1) through NODE(I,ORDER(I)).  These vertices
//    are listed in neighboring order.
//
//    Input, int NODE_NUM, the number of points stored in COORD.
//
//    Input, int ORDER[FACE_NUM], the number of vertices making up
//    each face.
//
//    Output, double POLYHEDRON_CENTROID_3D[3], the centroid of the polyhedron.
//
{
# define DIM_NUM 3

  double area;
  double *centroid;
  int face;
  int i;
  int j;
  int n;
  int n1;
  int n2;
  int n3;
  double normal[DIM_NUM];
  double point[DIM_NUM];
  double polygon_area;
  double *polygon_centroid;
  double tet[DIM_NUM*4];
  double *tetrahedron_centroid;
  double tetrahedron_volume;
  int v;
  double *vert;
  int vert_num;
  double volume;
//
//  Compute a point in the interior.
//  We take the area-weighted centroid of each face.
//
  r8vec_zero ( DIM_NUM, point );

  vert = new double[DIM_NUM*order_max];

  area = 0.0;

  for ( face = 0; face < face_num; face++ )
  {
    vert_num = order[face];

    for ( j = 0; j < vert_num; j++ )
    {
      n = node[j+face*order_max];

      vert[0+j*DIM_NUM] = coord[0+(n-1)*DIM_NUM];
      vert[1+j*DIM_NUM] = coord[1+(n-1)*DIM_NUM];
      vert[2+j*DIM_NUM] = coord[2+(n-1)*DIM_NUM];
    }

    polygon_area = polygon_area_3d ( vert_num, vert, normal );

    polygon_centroid = polygon_centroid_3d ( vert_num, vert );

    for ( i = 0; i < DIM_NUM; i++ )
    {
      point[i] = point[i] + polygon_area * polygon_centroid[i];
    }
    area = area + polygon_area;

    delete [] polygon_centroid;
  }

  delete [] vert;

  point[0] = point[0] / area;
  point[1] = point[1] / area;
  point[2] = point[2] / area;
//
//  Now triangulate each face.
//  For each triangle, consider the tetrahedron created by including POINT.
//
  centroid = new double[DIM_NUM];

  r8vec_zero ( DIM_NUM, centroid );

  volume = 0.0;

  for ( face = 0; face < face_num; face++ )
  {
    n3 = node[order[face]-1+face*order_max];

    r8vec_copy ( DIM_NUM, coord+(n3-1)*3, tet+2*3 );

    for ( v = 0; v < order[face] - 2; v++ )
    {
      n1 = node[v+face*order_max];
      n2 = node[v+1+face*order_max];

      r8vec_copy ( DIM_NUM, coord+(n1-1)*3, tet+0*3 );
      r8vec_copy ( DIM_NUM, coord+(n2-1)*3, tet+1*3 );
      r8vec_copy ( DIM_NUM, point,          tet+3*3 );

      tetrahedron_volume = tetrahedron_volume_3d ( tet );

      tetrahedron_centroid = tetrahedron_centroid_3d ( tet );

      for ( i = 0; i < DIM_NUM; i++ )
      {
        centroid[i] = centroid[i]
                    + tetrahedron_volume * tetrahedron_centroid[i];
      }

      volume = volume + tetrahedron_volume;

      delete [] tetrahedron_centroid;
    }
  }

  for ( i = 0; i < 3; i++ )
  {
    centroid[i] = centroid[i] / volume;
  }

  return centroid;
# undef DIM_NUM
}
//****************************************************************************80

bool polyhedron_contains_point_3d ( int node_num, int face_num,
  int face_order_max, double v[], int face_order[], int face_point[],
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    POLYHEDRON_CONTAINS_POINT_3D determines if a point is inside a polyhedron.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paulo Cezar Pinto Carvalho, Paulo Roma Cavalcanti,
//    Point in Polyhedron Testing Using Spherical Polygons,
//    in Graphics Gems V,
//    edited by Alan Paeth,
//    Academic Press, 1995, T385.G6975.
//
//  Parameters:
//
//    Input, int NODE_NUM, the number of vertices.
//
//    Input, int FACE_NUM, the number of faces.
//
//    Input, int FACE_ORDER_MAX, the maximum order of any face.
//
//    Input, double V[3*NODE_NUM], the coordinates of the vertices.
//
//    Input, int FACE_ORDER[FACE_NUM], the order of each face.
//
//    Input, int FACE_POINT[FACE_ORDER_MAX*FACE_NUM], the indices of the
//    nodes that make up each face.
//
//    Input, double P[3], the point to be tested.
//
//    Output, bool POLYHEDRON_CONTAINS_POINT_3D, is true if the point
//    is inside the polyhedron.
//
{
# define DIM_NUM 3

  double area;
  int face;
  int i;
  bool inside;
  int k;
  int node;
  int node_num_face;
  double r8_pi = 3.141592653589793;
  double *v_face;

  v_face = new double[DIM_NUM*face_order_max];

  area = 0.0;

  for ( face = 0; face < face_num; face++ )
  {
    node_num_face = face_order[face];

    for ( k = 0; k < node_num_face; k++ )
    {
      node = face_point[k+face*face_order_max];

      for ( i = 0; i < DIM_NUM; i++ )
      {
        v_face[i+k*DIM_NUM] = v[i+(node-1)*DIM_NUM];
      }
    }

    area = area + polygon_solid_angle_3d ( node_num_face, v_face, p );
  }
//
//  AREA should be -4*PI, 0, or 4*PI.
//  So this test should be quite safe!
//
  if ( area < -2.0 * r8_pi || 2.0 * r8_pi < area )
  {
    inside = true;
  }
  else
  {
    inside = false;
  }

  delete [] v_face;

  return inside;
# undef DIM_NUM
}
//****************************************************************************80

double polyhedron_volume_3d ( double coord[], int order_max, int face_num,
  int node[], int node_num, int order[] )

//****************************************************************************80
//
//  Purpose:
//
//    POLYHEDRON_VOLUME_3D computes the volume of a polyhedron in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    21 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double COORD[NODE_NUM*3], the 3D coordinates of the vertices.
//    The vertices may be listed in any order.
//
//    Input, int ORDER_MAX, the maximum number of vertices that make
//    up a face of the polyhedron.
//
//    Input, int FACE_NUM, the number of faces of the polyhedron.
//
//    Input, int NODE[FACE_NUM*ORDER_MAX].  Face I is defined by
//    the vertices NODE(I,1) through NODE(I,ORDER(I)).  These vertices
//    are listed in neighboring order.
//
//    Input, int NODE_NUM, the number of points stored in COORD.
//
//    Input, int ORDER[FACE_NUM], the number of vertices making up
//    each face.
//
//    Output, double POLYHEDRON_VOLUME_3D, the volume of the polyhedron.
//
{
# define DIM_NUM 3

  int face;
  int n1;
  int n2;
  int n3;
  double term;
  int v;
  double volume;
  double x1;
  double x2;
  double x3;
  double y1;
  double y2;
  double y3;
  double z1;
  double z2;
  double z3;
//
  volume = 0.0;
//
//  Triangulate each face.
//
  for ( face = 0; face < face_num; face++ )
  {
    n3 = node[order[face]-1+face*order_max];
    x3 = coord[0+n3*3];
    y3 = coord[1+n3*3];
    z3 = coord[2+n3*3];

    for ( v = 0; v < order[face] - 2; v++ )
    {
      n1 = node[v+face*order_max];
      x1 = coord[0+n1*3];
      y1 = coord[1+n1*3];
      z1 = coord[2+n1*3];

      n2 = node[v+1+face*order_max];
      x2 = coord[0+n2*3];
      y2 = coord[1+n2*3];
      z2 = coord[2+n2*3];

      term = x1 * y2 * z3 - x1 * y3 * z2
           + x2 * y3 * z1 - x2 * y1 * z3
           + x3 * y1 * z2 - x3 * y2 * z1;

      volume = volume + term;
    }

  }

  volume = volume / 6.0;

  return volume;
# undef DIM_NUM
}
//****************************************************************************80

double polyhedron_volume_3d_2 ( double coord[], int order_max, int face_num,
  int node[], int node_num, int order[] )

//****************************************************************************80
//
//  Purpose:
//
//    POLYHEDRON_VOLUME_3D_2 computes the volume of a polyhedron in 3D.
//
//  Discussion:
//
//    The computation is not valid unless the faces of the polyhedron
//    are planar polygons.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Allen Van Gelder,
//    Efficient Computation of Polygon Area and Polyhedron Volume,
//    Graphics Gems V, edited by Alan Paeth,
//    AP Professional, 1995, T385.G6975.
//
//  Parameters:
//
//    Input, double COORD[3*NODE_NUM], the 3D coordinates of the vertices.
//    The vertices may be listed in any order.
//
//    Input, int ORDER_MAX, the maximum number of vertices that make
//    up a face of the polyhedron.
//
//    Input, int FACE_NUM, the number of faces of the polyhedron.
//
//    Input, int NODE[FACE_NUM*ORDER_MAX].  Face I is defined by
//    the vertices NODE(I,1) through NODE(I,ORDER(I)).  These vertices
//    are listed in neighboring order.
//
//    Input, int NODE_NUM, the number of points stored in COORD.
//
//    Input, int ORDER[FACE_NUM], the number of vertices making up
//    each face.
//
//    Output, double POLYHEDRON_VOLUME_3D_2, the volume of the polyhedron.
//
{
# define DIM_NUM 3

  int face;
  int j;
  int k;
  double volume;
  double v1[DIM_NUM];
  double v2[DIM_NUM];
  double *v3;
  double v4[DIM_NUM];

  volume = 0.0;

  for ( face = 0; face < face_num; face++ )
  {
    r8vec_zero ( DIM_NUM, v4 );
//
//  Compute the area vector for this face.
//
    for ( j = 0; j < order[face]; j++ )
    {
      k = node[j+face*order_max];
      v1[0] = coord[0+k*3];
      v1[1] = coord[1+k*3];
      v1[2] = coord[2+k*3];

      if ( j + 1 < order[face] )
      {
        k = node[j+1+face*order_max];
      }
      else
      {
        k = node[0+face*order_max];
      }

      v2[0] = coord[0+k*3];
      v2[1] = coord[1+k*3];
      v2[2] = coord[2+k*3];

      v3 = r8vec_cross_product_3d ( v1, v2 );

      v4[0] = v4[0] + v3[0];
      v4[1] = v4[1] + v3[1];
      v4[2] = v4[2] + v3[2];

      delete [] v3;
    }
//
//  Area vector dot any vertex.
//
    k = node[0+face*order_max];

    volume = volume + v4[0] * coord[0+k*3]
                    + v4[1] * coord[1+k*3]
                    + v4[2] * coord[2+k*3];

  }

  volume = volume / 6.0;

  return volume;
# undef DIM_NUM
}
//****************************************************************************80

double *polyline_arclength_nd ( int dim_num, int n, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    POLYLINE_ARCLENGTH_ND computes the arclength of points on a polyline in ND.
//
//  Discussion:
//
//    A polyline of order N is the geometric structure consisting of
//    the N-1 line segments that lie between successive elements of a list
//    of N points.
//
//    An ordinary line segment is a polyline of order 2.
//    The letter "V" is a polyline of order 3.
//    The letter "N" is a polyline of order 4, and so on.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 February 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, int N, the number of points defining the polyline.
//
//    Input, double P[DIM_NUM*N], the points defining the polyline.
//
//    Output, double POLYLINE_ARCLENGTH_ND[N], the arclength coordinates
//    of each point.  The first point has arclength 0 and the
//    last point has arclength equal to the length of the entire polyline.
//
{
  int i;
  int j;
  double *s;
  double temp;

  s = new double[n];

  s[0] = 0.0;

  for ( j = 1; j < n; j++ )
  {
    temp = 0.0;
    for ( i = 0; i < dim_num; i++ )
    {
      temp = temp + pow ( p[i+j*dim_num] - p[i+(j-1)*dim_num], 2 );
    }
    temp = sqrt ( temp );
    s[j] = s[j-1] + temp;
  }

  return s;
}
//****************************************************************************80

double *polyline_index_point_nd ( int dim_num, int n, double p[], double t )

//****************************************************************************80
//
//  Purpose:
//
//    POLYLINE_INDEX_POINT_ND evaluates a polyline at a given arclength in ND.
//
//  Discussion:
//
//    The polyline is defined as the set of M-1 line segments lying
//    between a sequence of M points.  The arclength of a point lying
//    on the polyline is simply the length of the broken line from the
//    initial point.  Any point on the polyline can be found by
//    specifying its arclength.
//
//    If the given arclength coordinate is less than 0, or greater
//    than the arclength coordinate of the last given point, then
//    extrapolation is used, that is, the first and last line segments
//    are extended as necessary.
//
//    The arclength coordinate system measures the distance between
//    any two points on the polyline as the length of the segment of the
//    line that joins them.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    21 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space in which
//    the points lie.  The second dimension of XPTS.
//
//    Input, int N, the number of points.
//
//    Input, double P[DIM_NUM*N], a set of N coordinates
//    in DIM_NUM space, describing a set of points that define
//    a polyline.
//
//    Input, double T, the desired arclength coordinate.
//
//    Output, double POLYLINE_INDEX_POINT_ND[DIM_NUM], a point lying on the
//    polyline defined by P, and having arclength coordinate T.
//
{
  int i;
  int j;
  double s;
  double temp;
  double tleft;
  double trite;
  double *pt;

  if ( n <= 0 )
  {
    cerr << "\n";
    cerr << "POLYLINE_INDEX_POINT_ND - Fatal error!\n";
    cerr << "  The input quantity N is nonpositive.\n";
    cerr << "  N = " << n << "\n";
    exit ( 1 );
  }

  pt = new double[dim_num];

  if ( n == 1 )
  {
    for ( i = 0; i < dim_num; i++ )
    {
      pt[i] = p[i+0*dim_num];
    }
  }
  else
  {
    trite = 0.0;
    for ( i = 1; i <= n - 1; i++ )
    {
//
//  Find the distance between points I and I+1.
//
      tleft = trite;
      temp = 0.0;
      for ( j = 0; j < dim_num; j++ )
      {
        temp = temp + ( p[j+i*dim_num] - p[j+(i-1)*dim_num] )
                    * ( p[j+i*dim_num] - p[j+(i-1)*dim_num] );
      }
      trite = trite + sqrt ( temp );
//
//  Interpolate or extrapolate in an interval.
//
      if ( t <= trite || i == n - 1 )
      {
        s = ( t - tleft ) / ( trite - tleft );
        for ( j = 0; j < dim_num; j++ )
        {
          pt[j] = ( 1.0 - s ) * p[j+(i-1)*dim_num]
                        + s   * p[j+i*dim_num];
        }
        break;
      }
    }
  }

  return pt;
}
//****************************************************************************80

double polyline_length_nd ( int dim_num, int n, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    POLYLINE_LENGTH_ND computes the length of a polyline in ND.
//
//  Discussion:
//
//    A polyline of order M is the geometric structure consisting of
//    the M-1 line segments that lie between successive elements of a list
//    of M points.
//
//    An ordinary line segment is a polyline of order 2.
//    The letter "V" is a polyline of order 3.
//    The letter "N" is a polyline of order 4, and so on.
//
//    DIST(I+1,I) = sqrt ( sum ( 1 <= J <= DIM_NUM ) ( X(I+1) - X(I) )^2 )
//
//    LENGTH = sum ( 1 <= I <= NPOINT-1 ) DIST(I+1,I)
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the number of dimensions of the points.
//
//    Input, int N, the number of points.
//
//    Input, double P[DIM_NUM*N], the coordinates of the points.
//
//    Output, double POLYLINE_LENGTH_ND, the arclength of the polyline.
//
{
  int i;
  int j;
  double length;
  double step;

  length = 0.0;

  for ( j = 1; j < n; j++ )
  {
    step = 0.0;
    for ( i = 0; i < dim_num; i++ )
    {
       step = step + ( p[i+j*dim_num] - p[i+(j-1)*dim_num] )
                   * ( p[i+j*dim_num] - p[i+(j-1)*dim_num] ) ;
    }
    length = length + sqrt ( step );
  }

  return length;
}
//****************************************************************************80

double *polyline_points_nd ( int dim_num, int n, double p[], int nt )

//****************************************************************************80
//
//  Purpose:
//
//    POLYLINE_POINTS_ND computes equally spaced points on a polyline in ND.
//
//  Discussion:
//
//    A polyline of order N is the geometric structure consisting of
//    the N-1 line segments that lie between successive elements of a list
//    of N points.
//
//    An ordinary line segment is a polyline of order 2.
//    The letter "V" is a polyline of order 3.
//    The letter "N" is a polyline of order 4, and so on.
//
//    Thanks to Rick Richardson for pointing out an indexing error in the
//    storage of the values.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    20 February 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, int N, the number of points defining the polyline.
//
//    Input, double P[DIM_NUM*N], the points defining the polyline.
//
//    Input, int NT, the number of points to be sampled.
//
//    Output, double POLYLINE_POINTS_ND[DIM_NUM*NT], equally spaced points
//    on the polyline.
//
{
  int i;
  int it;
  int j;
  double *pt;
  double *s;
  double st;

  pt = new double[dim_num*nt];

  s = polyline_arclength_nd ( dim_num, n, p );

  j = 1;

  for ( it = 1; it <= nt; it++ )
  {
    st = ( ( double ) ( nt - it     ) * 0.0 +
           ( double ) (      it - 1 ) * s[n-1] )
         / ( double ) ( nt      - 1 );

    for ( ; ; )
    {
      if ( s[j-1] <= st && st <= s[j] )
      {
        break;
      }

      if ( n - 1 <= j )
      {
        break;
      }

      j = j + 1;
    }

    for ( i = 0; i < dim_num; i++ )
    {
      pt[i+(it-1)*dim_num] = ( ( s[j] - st          ) * p[i+(j-1)*dim_num]
                           + (          st - s[j-1] ) * p[i+j*dim_num] )
                             / ( s[j]      - s[j-1] );
    }
  }

  delete [] s;

  return pt;
}
//****************************************************************************80

double *polyloop_arclength_nd ( int dim_num, int nk, double pk[] )

//****************************************************************************80
//
//  Purpose:
//
//    POLYLOOP_ARCLENGTH_ND computes the arclength of points on a polyloop in ND.
//
//  Discussion:
//
//    A polyloop of order NK is the geometric structure consisting of
//    the NK line segments that lie between successive elements of a list
//    of NK points, with the last point joined to the first.
//
//    Warning: I just made up the word "polyloop".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    20 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, int NK, the number of points defining the polyloop.
//
//    Input, double PK[DIM_NUM*NK], the points defining the polyloop.
//
//    Output, double POLYLOOP_ARCLENGTH_ND[NK+1], the arclength coordinates
//    of each point.  The first point has two arc length values,
//    namely SK(1) = 0 and SK(NK+1) = LENGTH.
//
{
  int i;
  int j;
  int j2;
  double *sk;
  double temp;

  sk = new double[nk+1];

  sk[0] = 0.0;

  for ( j = 1; j <= nk; j++ )
  {
    if ( j == nk )
    {
      j2 = 0;
    }
    else
    {
      j2 = j;
    }

    temp = 0.0;
    for ( i = 0; i < dim_num; i++ )
    {
      temp = temp + pow ( pk[i+j2*dim_num] - pk[i+(j-1)*dim_num], 2 );
    }
    sk[j] = sk[j-1] + sqrt ( temp );
  }

  return sk;
}
//****************************************************************************80

double *polyloop_points_nd ( int dim_num, int nk, double pk[], int nt )

//****************************************************************************80
//
//  Purpose:
//
//    POLYLOOP_POINTS_ND computes equally spaced points on a polyloop in ND.
//
//  Discussion:
//
//    A polyloop of order NK is the geometric structure consisting of
//    the NK line segments that lie between successive elements of a list
//    of NK points, including a segment from the last point to the first.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    20 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, int NK, the number of points defining the polyloop.
//
//    Input, double PK[DIM_NUM*NK], the points defining the polyloop.
//
//    Input, int NT, the number of points to be sampled.
//
//    Input, double POLYLOOP_POINTS_ND[DIM_NUM*NT], equally spaced points
//    on the polyloop.
//
{
  int i;
  int it;
  int j;
  int jp1;
  double *pt;
  double *sk;
  double st;

  pt = new double[dim_num*nt];

  sk = polyloop_arclength_nd ( dim_num, nk, pk );

  j = 1;

  for ( it = 1; it <= nt; it++ )
  {
    st = ( ( double ) ( nt - it     ) * 0.0 +
           ( double ) (      it - 1 ) * sk[nk] )
         / ( double ) ( nt      - 1 );

    for ( ; ; )
    {
      if ( sk[j-1] <= st && st <= sk[j] )
      {
        break;
      }

      if ( nk <= j )
      {
        break;
      }
      j = j + 1;
    }

    jp1 = i4_wrap ( j + 1, 1, nk );

    for ( i = 0; i < dim_num; i++ )
    {
      pt[i+(it-1)*dim_num] =
        ( ( sk[j] - st           ) * pk[i+(j-1)*dim_num]
      + (           st - sk[j-1] ) * pk[i+(jp1-1)*dim_num] )
        / ( sk[j]      - sk[j-1] );
    }
  }

  delete [] sk;
  return pt;
}
//****************************************************************************80

void provec ( int m, int n, double base[], double vecm[], double vecn[],
  double vecnm[] )

//****************************************************************************80
//
//  Purpose:
//
//    PROVEC projects a vector from M space into N space.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    20 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the dimension of the higher order space.
//
//    Input, int N, the dimension of the lower order space.
//
//    Input, double BASE[M*N].  The columns of BASE contain
//    N vectors, each of length M, which form the basis for
//    a space of dimension N.
//
//    Input, double VECM[M], is an M dimensional vector.
//
//    Output, double VECN[N], the projection of VECM into the
//    lower dimensional space.  These values represent
//    coordinates in the lower order space.
//
//    Output, double VECNM[M], the projection of VECM into the
//    lower dimensional space, but using coordinates in
//    the higher dimensional space.
//
{
  int i;
  int j;
  int k;
  double temp;
//
//  For each vector, remove all projections onto previous vectors,
//  and then normalize.  This should result in a matrix BASE
//  whose columns are orthonormal.
//
  for ( j = 0; j < n; j++ )
  {
    for ( k = 0; k < j; k++ )
    {
      temp = r8vec_dot_product ( m, base+k*m, base+j*m );

      for ( i = 0; i < m; i++ )
      {
        base[i+j*m] = base[i+j*m] - temp * base[i+k*m];
      }
    }

    temp = 0.0;
    for ( i = 0; i < m; i++ )
    {
      temp = temp + pow ( base[i+j*m], 2 );
    }
    temp = sqrt ( temp );

    if ( 0.0 < temp )
    {
      for ( i = 0; i < m; i++ )
      {
        base[i+j*m] = base[i+j*m] / temp;
      }
    }
  }
//
//  Compute the coordinates of the projection of the vector
//  simply by taking dot products.
//
  for ( j = 0; j < n; j++ )
  {
    vecn[j] = r8vec_dot_product ( m, vecm, base+j*m );
  }
//
//  Compute the coordinates of the projection in terms of
//  the original space.
//
  for ( i = 0; i < m; i++ )
  {
    vecnm[i] = 0.0;
    for ( j = 0; j < n; j++ )
    {
      vecnm[i] = vecnm[i] + base[i+j*n] * vecn[j];
    }
  }

  return;
}
//****************************************************************************80

double pyramid_volume_3d ( double h, double s )

//****************************************************************************80
//
//  Purpose:
//
//    PYRAMID_VOLUME_3D computes the volume of a pyramid with square base in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double H, R, the height of the pyramid, and the length of one
//    side of the square base.
//
//    Output, double PYRAMID_VOLUME_3D, the volume of the pyramid.
//
{
  double value;

  value = s * s * h / 3.0;

  return value;
}
//****************************************************************************80

double quad_area_2d ( double q[2*4] )

//****************************************************************************80
//
//  Purpose:
//
//    QUAD_AREA_2D computes the area of a quadrilateral in 2D.
//
//  Discussion:
//
//    This algorithm should be able to handle nonconvex quadrilaterals.
//
//    3----2
//    |   /|
//    |  / |    We subdivide the quadrilateral into triangles (0,1,2)
//    | /  |    and (2,3,0), computer their areas, and add.
//    |/   |
//    0----1
//
//    Thanks to Eduardo Olmedo of Universidad Politecnica de Madrid for
//    pointing out an error in a previous version of this routine!
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 December 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double Q[2*4], the vertices of the quadrilateral,
//    in counter clockwise order.
//
//    Output, double QUAD_AREA_2D, the area of the quadrilateral.
//
{
# define DIM_NUM 2

  double area;
  double t[DIM_NUM*3];

  area = 0.0;

  t[0+0*2] = q[0+0*2];
  t[1+0*2] = q[1+0*2];
  t[0+1*2] = q[0+1*2];
  t[1+1*2] = q[1+1*2];
  t[0+2*2] = q[0+2*2];
  t[1+2*2] = q[1+2*2];

  area = area + triangle_area_2d ( t );

  t[0+0*2] = q[0+2*2];
  t[1+0*2] = q[1+2*2];
  t[0+1*2] = q[0+3*2];
  t[1+1*2] = q[1+3*2];
  t[0+2*2] = q[0+0*2];
  t[1+2*2] = q[1+0*2];

  area = area + triangle_area_2d ( t );

  return area;
# undef DIM_NUM
}
//****************************************************************************80

double quad_area2_2d ( double q[] )

//****************************************************************************80
//
//  Purpose:
//
//    QUAD_AREA2_2D computes the area of a quadrilateral in 2D.
//
//  Discussion:
//
//    A quadrilateral is a polygon defined by 4 vertices.
//
//    This algorithm computes the area of the related
//    Varignon parallelogram first.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double Q[2*4], the vertices, specified in
//    counter clockwise order.
//
//    Output, double QUAD_AREA2_2D, the area of the quadrilateral.
//
{
  double area;
  int i;
  int j;
  double *p;

  p = new double[2*4];
//
//  Define a parallelogram by averaging consecutive vertices.
//
  for ( j = 0; j < 3; j++ )
  {
    for ( i = 0; i < 2; i++ )
    {
      p[i+j*2] = ( q[i+j*2] + q[i+(j+1)*2] ) / 2.0;
    }
  }
  for ( i = 0; i < 2; i++ )
  {
    p[i+3*2] = ( q[i+3*2] + q[i+0*2] ) / 2.0;
  }
//
//  Compute the area.
//
  area = parallelogram_area_2d ( p );
//
//  The quadrilateral's area is twice that of the parallelogram.
//
  area = 2.0 * area;

  delete [] p;

  return area;
}
//****************************************************************************80

double quad_area_3d ( double q[] )

//****************************************************************************80
//
//  Purpose:
//
//    QUAD_AREA_3D computes the area of a quadrilateral in 3D.
//
//  Discussion:
//
//    A quadrilateral is a polygon defined by 4 vertices.
//
//    It is assumed that the four vertices of the quadrilateral
//    are coplanar.
//
//    This algorithm computes the area of the related
//    Varignon parallelogram first.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double Q[3*4], the vertices, specified in
//    counter clockwise order.
//
//    Output, double QUAD_AREA_3D, the area of the quadrilateral.
//
{
  double area;
  int i;
  int j;
  double *p;

  p = new double[3*4];
//
//  Define a parallelogram by averaging consecutive vertices.
//
  for ( j = 0; j < 3; j++ )
  {
    for ( i = 0; i < 3; i++ )
    {
      p[i+j*3] = ( q[i+j*3] + q[i+(j+1)*3] ) / 2.0;
    }
  }
  for ( i = 0; i < 3; i++ )
  {
    p[i+3*3] = ( q[i+3*3] + q[i+0*3] ) / 2.0;
  }
//
//  Compute the area.
//
  area = parallelogram_area_3d ( p );
//
//  The quadrilateral's area is twice that of the parallelogram.
//
  area = 2.0 * area;

  delete [] p;

  return area;
}
//****************************************************************************80

bool quad_contains_point_2d ( double q[2*4], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    QUAD_CONTAINS_POINT_2D finds if a point is inside a convex quadrilateral in 2D.
//
//  Discussion:
//
//    This method will only handle convex quadrilaterals.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double Q[2*4], the vertices of the quadrilateral, in counter clockwise order.
//
//    Input, double P[2], the point to be checked.
//
//    Output, bool QUAD_CONTAINS_POINT, is TRUE if the point is inside
//    the quadrilateral or on its boundary, and FALSE otherwise.
//
{
# define DIM_NUM 2

  if ( anglei_rad_2d ( q+0*2, q+1*2, q+2*2 ) <
       anglei_rad_2d ( q+0*2, q+1*2, p  ) )
  {
    return false;
  }
  if ( anglei_rad_2d ( q+1*2, q+2*2, q+3*2 ) <
       anglei_rad_2d ( q+1*2, q+2*2, p  ) )
  {
    return false;
  }
  if ( anglei_rad_2d ( q+2*2, q+3*2, q+0*2 ) <
       anglei_rad_2d ( q+2*2, q+3*2, p  ) )
  {
    return false;
  }
  if ( anglei_rad_2d ( q+3*2, q+0*2, q+1*2 ) <
       anglei_rad_2d ( q+3*2, q+0*2, p  ) )
  {
    return false;
  }

  return true;
# undef DIM_NUM
}
//****************************************************************************80

void quad_convex_random ( int &seed, double xy[] )

//****************************************************************************80
//
//  Purpose:
//
//    QUAD_CONVEX_RANDOM returns a random convex quadrilateral.
//
//  Description:
//
//    The quadrilateral is constrained in that the vertices must all lie
//    with the unit square.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    26 June 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int &SEED, a seed for the random number
//    generator.
//
//    Output, double XY[2*NODE_NUM], the coordinates of the
//    nodes of the quadrilateral, given in counterclockwise order.
//
{
  int hull[4];
  int hull_num;
  int i;
  int j;
  double *xy_random;

  for ( ; ; )
  {
//
//  Generate 4 random points.
//
    xy_random = r8mat_uniform_01_new ( 2, 4, seed );
//
//  Determine the convex hull.
//
    points_hull_2d ( 4, xy_random, &hull_num, hull );
//
//  If HULL_NUM < 4, then our convex hull is a triangle.
//  Try again.
//
    if ( hull_num == 4 )
    {
      break;
    }
  }
//
//  Make an ordered copy of the random points.
//
  for ( j = 0; j < 4; j++ )
  {
    for ( i = 0; i < 2; i++ )
    {
      xy[i+j*2] = xy_random[i+(hull[j]-1)*2];
    }
  }
  delete [] xy_random;

  return;
}
//****************************************************************************80

double quad_point_dist_2d ( double q[2*4], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    QUAD_POINT_DIST_2D finds the distance from a point to a quadrilateral in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double Q[2*4], the vertices of the quadrilateral.
//
//    Input, double P[2], the point to be checked.
//
//    Output, double QUAD_POINT_DIST_2D, the distance from the point to the quadrilateral.
//    DIST is zero if the point lies exactly on the quadrilateral.
//
{
# define DIM_NUM 2

  double dist;
  double dist2;
  int j;
  int jp1;
  int side_num = 4;
//
//  Find the distance to each of the line segments.
//
  dist = HUGE_VAL;

  for ( j = 0; j < side_num; j++ )
  {
    jp1 = i4_wrap ( j+1, 0, side_num-1 );

    dist2 = segment_point_dist_2d ( q+j*2, q+jp1*2, p );

    if ( dist2 < dist )
    {
      dist = dist2;
    }
  }

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

double quad_point_dist_signed_2d ( double q[2*4], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    QUAD_POINT_DIST_SIGNED_2D: signed distanct ( quadrilateral, point ) in 2D.
//
//  Discussion:
//
//    The quadrilateral must be convex.  DIST_SIGNED is actually the maximum
//    of the signed distances from the point to each of the four lines that
//    make up the quadrilateral.
//
//    Essentially, if the point is outside the convex quadrilateral,
//    only one of the signed distances can be positive, or two can
//    be positive and equal.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double Q[2*4], the vertices of the quadrilateral.
//
//    Input, double P[2], the point which is to be checked.
//
//    Output, double QUAD_POINT_DIST_SIGNED_2D, the signed distance from
//    the point to the convex quadrilateral.  If the distance is
//    0.0, the point is on the boundary;
//    negative, the point is in the interior;
//    positive, the point is in the exterior.
//
{
# define DIM_NUM 2

  double dis1;
  double dis2;
  double dist_signed;
  double pm[DIM_NUM];
//
//  Compare the signed distance from each line segment to the point,
//  with the signed distance to the midpoint of the opposite line.
//
//  The signed distances should all be negative if the point is inside.
//
//  Side 12
//
  dis1 = line_exp_point_dist_signed_2d ( q+0*2, q+1*2, p );

  pm[0] = 0.5 * ( q[0+2*2] + q[0+3*2] );
  pm[1] = 0.5 * ( q[1+2*2] + q[1+3*2] );

  dis2 = line_exp_point_dist_signed_2d ( q+0*2, q+1*2, pm );

  if ( 0.0 < dis2 )
  {
    dis1 = -dis1;
  }
  dist_signed = dis1;
//
//  Side 23
//
  dis1 = line_exp_point_dist_signed_2d ( q+1*2, q+2*2, p );

  pm[0] = 0.5 * ( q[0+3*2] + q[0+0*2] );
  pm[1] = 0.5 * ( q[1+3*2] + q[1+0*2] );

  dis2 = line_exp_point_dist_signed_2d ( q+1*2, q+2*2, pm );

  if ( 0.0 < dis2 )
  {
    dis1 = -dis1;
  }
  dist_signed = fmax ( dist_signed, dis1 );
//
//  Side 34
//
  dis1 = line_exp_point_dist_signed_2d ( q+2*2, q+3*2, p );

  pm[0] = 0.5 * ( q[0+0*2] + q[0+1*2] );
  pm[1] = 0.5 * ( q[1+0*2] + q[1+1*2] );

  dis2 = line_exp_point_dist_signed_2d ( q+2*2, q+3*2, pm );

  if ( 0.0 < dis2 )
  {
    dis1 = -dis1;
  }
  dist_signed = fmax ( dist_signed, dis1 );
//
//  Side 41
//
  dis1 = line_exp_point_dist_signed_2d ( q+3*2, q+0*2, p );

  pm[0] = 0.5 * ( q[0+1*2] + q[0+2*2] );
  pm[1] = 0.5 * ( q[1+1*2] + q[1+2*2] );

  dis2 = line_exp_point_dist_signed_2d ( q+3*2, q+0*2, pm );

  if ( 0.0 < dis2 )
  {
    dis1 = -dis1;
  }
  dist_signed = fmax ( dist_signed, dis1 );

  return dist_signed;
# undef DIM_NUM
}
//****************************************************************************80

void quad_point_near_2d ( double q[2*4], double p[2], double pn[2],
  double *dist )

//****************************************************************************80
//
//  Purpose:
//
//    QUAD_POINT_NEAR_2D computes the nearest point on a quadrilateral in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double Q[2*4], the quadrilateral vertices.
//
//    Input, double P[2], the point whose nearest quadrilateral point
//    is to be determined.
//
//    Output, double PN[2], the nearest point to P.
//
//    Output, double *DIST, the distance from the point to the
//    quadrilateral.
//
{
# define DIM_NUM 2

  double dist2;
  int j;
  int jp1;
  double pn2[DIM_NUM];
  int side_num = 4;
  double tval;

  *dist = HUGE_VAL;
  r8vec_zero ( DIM_NUM, pn );

  for ( j = 0; j < side_num; j++ )
  {
    jp1 = i4_wrap ( j+1, 0, side_num-1 );

    segment_point_near_2d ( q+j*2, q+jp1*2, p, pn2, &dist2, &tval );

    if ( dist2 < *dist )
    {
      *dist = dist2;
      r8vec_copy ( DIM_NUM, pn2, pn );
    }
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

int r4_nint ( float x )

//****************************************************************************80
//
//  Purpose:
//
//    R4_NINT returns the nearest integer to an R4.
//
//  Example:
//
//        X         R4_NINT
//
//      1.3         1
//      1.4         1
//      1.5         1 or 2
//      1.6         2
//      0.0         0
//     -0.7        -1
//     -1.1        -1
//     -1.6        -2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    14 November 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, float X, the value.
//
//    Output, int R4_NINT, the nearest integer to X.
//
{
  int value;

  if ( x < 0.0 )
  {
    value = - ( int ) ( fabs ( x ) + 0.5 );
  }
  else
  {
    value =   ( int ) ( fabs ( x ) + 0.5 );
  }

  return value;
}
//****************************************************************************80

double r8_acos ( double c )

//****************************************************************************80
//
//  Purpose:
//
//    R8_ACOS computes the arc cosine function, with argument truncation.
//
//  Discussion:
//
//    If you call your system ACOS routine with an input argument that is
//    outside the range [-1.0, 1.0 ], you may get an unpleasant surprise.
//    This routine truncates arguments outside the range.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 June 2002
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double C, the argument, the cosine of an angle.
//
//    Output, double R8_ACOS, an angle whose cosine is C.
//
{
  const double r8_pi = 3.141592653589793;
  double value;

  if ( c <= -1.0 )
  {
    value = r8_pi;
  }
  else if ( 1.0 <= c )
  {
    value = 0.0;
  }
  else
  {
    value = acos ( c );
  }
  return value;
}
//****************************************************************************80

double r8_asin ( double s )

//****************************************************************************80
//
//  Purpose:
//
//    R8_ASIN computes the arc sine function, with argument truncation.
//
//  Discussion:
//
//    If you call your system ASIN routine with an input argument that is
//    outside the range [-1.0, 1.0 ], you may get an unpleasant surprise.
//    This routine truncates arguments outside the range.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 June 2002
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double S, the argument, the sine of an angle.
//
//    Output, double R8_ASIN, an angle whose sine is S.
//
{
  double angle;
  const double r8_pi = 3.141592653589793;

  if ( s <= - 1.0 )
  {
    angle = - r8_pi / 2.0;
  }
  else if ( 1.0 <= s )
  {
    angle = r8_pi / 2.0;
  }
  else
  {
    angle = asin ( s );
  }
  return angle;
}
//****************************************************************************80

double r8_atan ( double y, double x )

//****************************************************************************80
//
//  Purpose:
//
//    r8_atan computes the inverse tangent of the ratio Y / X.
//
//  Discussion:
//
//    r8_atan returns an angle whose tangent is ( Y / X ), a job which
//    the built in functions ATAN and ATAN2 already do.
//
//    However:
//
//    * r8_atan always returns a positive angle, between 0 and 2 PI,
//      while ATAN and ATAN2 return angles in the interval [-PI/2,+PI/2]
//      and [-PI,+PI] respectively;
//
//    * r8_atan accounts for the signs of X and Y, (as does ATAN2).  The ATAN
//     function by contrast always returns an angle in the first or fourth
//     quadrants.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 June 2002
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double Y, X, two quantities which represent the tangent of
//    an angle.  If Y is not zero, then the tangent is (Y/X).
//
//    Output, double r8_atan, an angle between 0 and 2 * PI, whose tangent is
//    (Y/X), and which lies in the appropriate quadrant so that the signs
//    of its cosine and sine match those of X and Y.
//
{
  double r8_pi = 3.141592653589793;
//
//  Special cases:
//
  if ( x == 0.0 )
  {
    if ( 0.0 < y )
    {
      return ( r8_pi / 2.0 );
    }
    else if ( y < 0.0 )
    {
      return ( 3.0 * r8_pi / 2.0 );
    }
    else if ( y == 0.0 )
    {
      return ( 0.0 );
    }
  }
  else if ( y == 0.0 )
  {
    if ( 0.0 < x )
    {
      return 0.0;
    }
    else if ( x < 0.0 )
    {
      return r8_pi;
    }
  }
//
//  We assume that ATAN2 is reliable when both arguments are positive.
//
  if ( 0.0 < x && 0.0 < y )
  {
    return                  atan2 (  y,  x );
  }
  else if ( x < 0.0 && 0.0 < y )
  {
    return ( r8_pi - atan2 (  y, -x ) );
  }
  else if ( x < 0.0 && y < 0.0 )
  {
    return ( r8_pi + atan2 ( -y, -x ) );
  }
  else if ( 0.0 < x && y < 0.0 )
  {
    return ( 2.0 * r8_pi - atan2 ( -y,  x ) );
  }

  return 0.0;
}
//****************************************************************************80

double r8_cosd ( double degrees )

//****************************************************************************80
//
//  Purpose:
//
//    R8_COSD returns the cosine of an angle given in degrees.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    27 July 2014
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double DEGREES, the angle in degrees.
//
//    Output, double R8_COSD, the cosine of the angle.
//
{
  const double r8_pi = 3.141592653589793;
  double radians;
  double value;

  radians = r8_pi * ( degrees / 180.0 );

  value = cos ( radians );

  return value;
}
//****************************************************************************80

double r8_cotd ( double degrees )

//****************************************************************************80
//
//  Purpose:
//
//    R8_COTD returns the cotangent of an angle given in degrees.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    27 July 2014
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double DEGREES, the angle in degrees.
//
//    Output, double R8_COTD, the cotangent of the angle.
//
{
  const double r8_pi = 3.141592653589793;
  double radians;
  double value;

  radians = r8_pi * ( degrees / 180.0 );

  value = cos ( radians ) / sin ( radians );

  return value;
}
//****************************************************************************80

double r8_cscd ( double degrees )

//****************************************************************************80
//
//  Purpose:
//
//    R8_CSCD returns the cosecant of an angle given in degrees.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    27 July 2014
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double DEGREES, the angle in degrees.
//
//    Output, double R8_CSCD, the cosecant of the angle.
//
{
  const double r8_pi = 3.141592653589793;
  double radians;
  double value;

  radians = r8_pi * ( degrees / 180.0 );

  value = 1.0 / sin ( radians );

  return value;
}
//****************************************************************************80

double r8_modp ( double x, double y )

//****************************************************************************80
//
//  Purpose:
//
//    R8_MODP returns the nonnegative remainder of R8 division.
//
//  Discussion:
//
//    If
//      REM = R8_MODP ( X, Y )
//      RMULT = ( X - REM ) / Y
//    then
//      X = Y * RMULT + REM
//    where REM is always nonnegative.
//
//    The MOD function computes a result with the same sign as the
//    quantity being divided.  Thus, suppose you had an angle A,
//    and you wanted to ensure that it was between 0 and 360.
//    Then mod(A,360.0) would do, if A was positive, but if A
//    was negative, your result would be between -360 and 0.
//
//    On the other hand, R8_MODP(A,360.0) is between 0 and 360, always.
//
//  Example:
//
//        I         J     MOD R8_MODP  R8_MODP Factorization
//
//      107        50       7       7    107 =  2 *  50 + 7
//      107       -50       7       7    107 = -2 * -50 + 7
//     -107        50      -7      43   -107 = -3 *  50 + 43
//     -107       -50      -7      43   -107 =  3 * -50 + 43
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X, the number to be divided.
//
//    Input, double Y, the number that divides X.
//
//    Output, double R8_MODP, the nonnegative remainder when X is divided by Y.
//
{
  double value;

  if ( y == 0.0 )
  {
    cerr << "\n";
    cerr << "R8_MODP - Fatal error!\n";
    cerr << "  R8_MODP ( X, Y ) called with Y = " << y << "\n";
    exit ( 1 );
  }

  value = x - ( ( double ) ( ( int ) ( x / y ) ) ) * y;

  if ( value < 0.0 )
  {
    value = value + fabs ( y );
  }

  return value;
}
//****************************************************************************80

int r8_nint ( double x )

//****************************************************************************80
//
//  Purpose:
//
//    R8_NINT returns the nearest integer to an R8.
//
//  Example:
//
//        X        R8_NINT
//
//      1.3         1
//      1.4         1
//      1.5         1 or 2
//      1.6         2
//      0.0         0
//     -0.7        -1
//     -1.1        -1
//     -1.6        -2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X, the real value.
//
//    Output, int R8_NINT, the nearest integer to X.
//
{
  int s;

  if ( x < 0.0 )
  {
    s = - 1;
  }
  else
  {
    s = + 1;
  }

  return ( s * ( int ) ( fabs ( x ) + 0.5 ) );
}
//****************************************************************************80

double r8_normal_01 ( int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    R8_NORMAL_01 returns a unit pseudonormal R8.
//
//  Discussion:
//
//    The standard normal probability distribution function (PDF) has
//    mean 0 and standard deviation 1.
//
//    The Box-Muller method is used, which is efficient, but
//    generates two values at a time.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 November 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double R8_NORMAL_01, a normally distributed random value.
//
{
  double r8_pi = 3.141592653589793;
  double r1;
  double r2;
  static int used = -1;
  static int seed2 = 0;
  double x;
  static double y = 0.0;

  if ( used == -1 )
  {
    used = 0;
  }
//
//  If we've used an even number of values so far, generate two more, return one,
//  and save one.
//
  if ( ( used % 2 )== 0 )
  {
    r1 = r8_uniform_01 ( seed );

    if ( r1 == 0.0 )
    {
      cerr << "\n";
      cerr << "R8_NORMAL_01 - Fatal error!\n";
      cerr << "  R8_UNIFORM_01 returned a value of 0.\n";
      exit ( 1 );
    }

    seed2 = seed;
    r2 = r8_uniform_01 ( seed2 );

    x = sqrt ( -2.0 * log ( r1 ) ) * cos ( 2.0 * r8_pi * r2 );
    y = sqrt ( -2.0 * log ( r1 ) ) * sin ( 2.0 * r8_pi * r2 );
  }
//
//  Otherwise, return the second, saved, value and the corresponding
//  value of SEED.
//
  else
  {
    x = y;
    seed = seed2;
  }

  used = used + 1;

  return x;
}
//****************************************************************************80

double r8_pi ( )

//****************************************************************************80
//
//  Purpose:
//
//    R8_PI returns the value of pi.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, double R8_PI, the value of pi.
//
{
  double pi = 3.141592653589793;

  return pi;
}
//****************************************************************************80

double r8_secd ( double degrees )

//****************************************************************************80
//
//  Purpose:
//
//    R8_SECD returns the secant of an angle given in degrees.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    27 July 2014
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double DEGREES, the angle in degrees.
//
//    Output, double R8_SECD, the secant of the angle.
//
{
  const double r8_pi = 3.141592653589793;
  double radians;
  double value;

  radians = r8_pi * ( degrees / 180.0 );

  value = 1.0 / cos ( radians );

  return value;
}
//****************************************************************************80

double r8_sign ( double x )

//****************************************************************************80
//
//  Purpose:
//
//    R8_SIGN returns the "sign" of an R8.
//
//  Discussion:
//
//    Negative arguments return -1, positive arguments +1.
//    There are reasonable claims that an argument of 0 should
//    return a 0 but instead they return +1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X, the argument.
//
//    Output, double R8_SIGN, the sign of X.
//
{
  double value;

  if ( x < 0.0 )
  {
    value = - 1.0;
  }
  else
  {
    value = + 1.0;
  }
  return value;
}
//****************************************************************************80

bool r8_sign_opposite_strict ( double r1, double r2 )

//****************************************************************************80
//
//  Purpose:
//
//    R8_SIGN_OPPOSITE_STRICT is TRUE if two R8's are strictly of opposite sign.
//
//  Discussion:
//
//    This test could be coded numerically as
//
//      if ( r1 * r2 < 0.0 ) ...
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 June 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R1, R2, the values to check.
//
//    Output, bool R8_SIGN_OPPOSITE_STRICT, is TRUE if ( R1 < 0 and 0 < R2 )
//    or ( R2 < 0 and 0 < R1 ).
//
{
  bool value;

  value = ( r1 < 0.0 && 0.0 < r2 ) || ( r2 < 0.0 && 0.0 < r1 );

  return value;
}
//****************************************************************************80

double r8_sind ( double degrees )

//****************************************************************************80
//
//  Purpose:
//
//    R8_SIND returns the sine of an angle given in degrees.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    27 July 2014
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double DEGREES, the angle in degrees.
//
//    Output, double R8_SIND, the sine of the angle.
//
{
  const double r8_pi = 3.141592653589793;
  double radians;
  double value;

  radians = r8_pi * ( degrees / 180.0 );

  value = sin ( radians );

  return value;
}
//****************************************************************************80

void r8_swap ( double *x, double *y )

//****************************************************************************80
//
//  Purpose:
//
//    R8_SWAP switches two R8s.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 January 2002
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, double *X, *Y.  On output, the values of X and
//    Y have been interchanged.
//
{
  double z;

  z = *x;
  *x = *y;
  *y = z;

  return;
}
//****************************************************************************80

double r8_tand ( double degrees )

//****************************************************************************80
//
//  Purpose:
//
//    R8_TAND returns the tangent of an angle given in degrees.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    27 July 2014
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double DEGREES, the angle in degrees.
//
//    Output, double R8_TAND, the tangent of the angle.
//
{
  const double r8_pi = 3.141592653589793;
  double radians;
  double value;

  radians = r8_pi * ( degrees / 180.0 );

  value = sin ( radians ) / cos ( radians );

  return value;
}
//****************************************************************************80

double r8_uniform_01 ( int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    R8_UNIFORM_01 returns a unit pseudorandom R8.
//
//  Discussion:
//
//    This routine implements the recursion
//
//      seed = 16807 * seed mod ( 2^31 - 1 )
//      R8_uniform_01 = seed / ( 2^31 - 1 )
//
//    The integer arithmetic never requires more than 32 bits,
//    including a sign bit.
//
//    If the initial seed is 12345, then the first three computations are
//
//      Input     Output      R8_UNIFORM_01
//      SEED      SEED
//
//         12345   207482415  0.096616
//     207482415  1790989824  0.833995
//    1790989824  2035175616  0.947702
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 August 2004
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paul Bratley, Bennett Fox, Linus Schrage,
//    A Guide to Simulation,
//    Springer Verlag, pages 201-202, 1983.
//
//    Pierre L'Ecuyer,
//    Random Number Generation,
//    in Handbook of Simulation
//    edited by Jerry Banks,
//    Wiley Interscience, page 95, 1998.
//
//    Bennett Fox,
//    Algorithm 647:
//    Implementation and Relative Efficiency of Quasirandom
//    Sequence Generators,
//    ACM Transactions on Mathematical Software,
//    Volume 12, Number 4, pages 362-376, 1986.
//
//    Peter Lewis, Allen Goodman, James Miller,
//    A Pseudo-Random Number Generator for the System/360,
//    IBM Systems Journal,
//    Volume 8, pages 136-143, 1969.
//
//  Parameters:
//
//    Input/output, int &SEED, the "seed" value.  Normally, this
//    value should not be 0.  On output, SEED has been updated.
//
//    Output, double R8_UNIFORM_01, a new pseudorandom variate, strictly between
//    0 and 1.
//
{
  int k;
  double r;

  k = seed / 127773;

  seed = 16807 * ( seed - k * 127773 ) - k * 2836;

  if ( seed < 0 )
  {
    seed = seed + 2147483647;
  }
//
//  Although SEED can be represented exactly as a 32 bit integer,
//  it generally cannot be represented exactly as a 32 bit real number!
//
  r = ( double ) ( seed ) * 4.656612875E-10;

  return r;
}
//****************************************************************************80

double r8_uniform_ab ( double a, double b, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    R8_UNIFORM_AB returns a scaled pseudorandom R8.
//
//  Discussion:
//
//    The pseudorandom number should be uniformly distributed
//    between A and B.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    09 April 2012
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, the limits of the interval.
//
//    Input/output, int &SEED, the "seed" value, which should NOT be 0.
//    On output, SEED has been updated.
//
//    Output, double R8_UNIFORM_AB, a number strictly between A and B.
//
{
  const int i4_huge = 2147483647;
  int k;
  double value;

  if ( seed == 0 )
  {
    cerr << "\n";
    cerr << "R8_UNIFORM_AB - Fatal error!\n";
    cerr << "  Input value of SEED = 0.\n";
    exit ( 1 );
  }

  k = seed / 127773;

  seed = 16807 * ( seed - k * 127773 ) - k * 2836;

  if ( seed < 0 )
  {
    seed = seed + i4_huge;
  }

  value = ( double ) ( seed ) * 4.656612875E-10;

  value = a + ( b - a ) * value;

  return value;
}
//****************************************************************************80

void r82vec_part_quick_a ( int n, double a[], int *l, int *r )

//****************************************************************************80
//
//  Purpose:
//
//    R82VEC_PART_QUICK_A reorders an R82VEC as part of a quick sort.
//
//  Discussion:
//
//    The routine reorders the entries of A.  Using A(1:2,1) as a
//    key, all entries of A that are less than or equal to the key will
//    precede the key, which precedes all entries that are greater than the key.
//
//  Example:
//
//    Input:
//
//      N = 8
//
//      A = ( (2,4), (8,8), (6,2), (0,2), (10,6), (10,0), (0,6), (4,8) )
//
//    Output:
//
//      L = 2, R = 4
//
//      A = ( (0,2), (0,6), (2,4), (8,8), (6,2), (10,6), (10,0), (4,8) )
//             -----------          ----------------------------------
//             LEFT          KEY    RIGHT
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries of A.
//
//    Input/output, double A[N*2].  On input, the array to be checked.
//    On output, A has been reordered as described above.
//
//    Output, int *L, *R, the indices of A that define the three segments.
//    Let KEY = the input value of A(1:2,1).  Then
//    I <= L                 A(1:2,I) < KEY;
//         L < I < R         A(1:2,I) = KEY;
//                 R <= I    A(1:2,I) > KEY.
//
{
  int i;
  int j;
  double key[2];
  int ll;
  int m;
  int rr;
//
  if ( n < 1 )
  {
    cerr << "\n";
    cerr << "R82VEC_PART_QUICK_A - Fatal error!\n";
    cerr << "  N < 1.\n";
    exit ( 1 );
  }

  if ( n == 1 )
  {
    *l = 0;
    *r = 2;
    return;
  }

  key[0] = a[0+0*2];
  key[1] = a[1+0*2];
  m = 1;
//
//  The elements of unknown size have indices between L+1 and R-1.
//
  ll = 1;
  rr = n + 1;

  for ( i = 2; i <= n; i++ )
  {
    if ( r8vec_gt ( 2, a+2*ll, key ) )
    {
      rr = rr - 1;
      r8vec_swap ( 2, a+2*(rr-1), a+2*ll );
    }
    else if ( r8vec_eq ( 2, a+2*ll, key ) )
    {
      m = m + 1;
      r8vec_swap ( 2, a+2*(m-1), a+2*ll );
      ll = ll + 1;
    }
    else if ( r8vec_lt ( 2, a+2*ll, key ) )
    {
      ll = ll + 1;
    }

  }
//
//  Now shift small elements to the left, and KEY elements to center.
//
  for ( i = 0; i < ll - m; i++ )
  {
    for ( j = 0; j < 2; j++ )
    {
      a[2*i+j] = a[2*(i+m)+j];
    }
  }

  ll = ll - m;

  for ( i = ll; i < ll+m; i++ )
  {
    for ( j = 0; j < 2; j++ )
    {
      a[2*i+j] = key[j];
    }
  }

  *l = ll;
  *r = rr;

  return;
}
//****************************************************************************80

void r82vec_permute ( int n, double a[], int p[] )

//****************************************************************************80
//
//  Purpose:
//
//    R82VEC_PERMUTE permutes an R82VEC in place.
//
//  Discussion:
//
//    This routine permutes an array of real "objects", but the same
//    logic can be used to permute an array of objects of any arithmetic
//    type, or an array of objects of any complexity.  The only temporary
//    storage required is enough to store a single object.  The number
//    of data movements made is N + the number of cycles of order 2 or more,
//    which is never more than N + N/2.
//
//  Example:
//
//    Input:
//
//      N = 5
//      P = (   2,    4,    5,    1,    3 )
//      A = ( 1.0,  2.0,  3.0,  4.0,  5.0 )
//          (11.0, 22.0, 33.0, 44.0, 55.0 )
//
//    Output:
//
//      A    = (  2.0,  4.0,  5.0,  1.0,  3.0 )
//             ( 22.0, 44.0, 55.0, 11.0, 33.0 ).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 February 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of objects.
//
//    Input/output, double A[2*N], the array to be permuted.
//
//    Input, int P[N], the permutation.  P(I) = J means
//    that the I-th element of the output array should be the J-th
//    element of the input array.  P must be a legal permutation
//    of the integers from 1 to N, otherwise the algorithm will
//    fail catastrophically.
//
{
  double a_temp[2];
  int i;
  int iget;
  int iput;
  int istart;

  if ( !perm_check ( n, p ) )
  {
    cerr << "\n";
    cerr << "R82VEC_PERMUTE - Fatal error!\n";
    cerr << "  The input array does not represent\n";
    cerr << "  a proper permutation.\n";
    i4vec_print ( n, p, "  The faulty permutation:" );
    exit ( 1 );
  }
//
//  Search for the next element of the permutation that has not been used.
//
  for ( istart = 1; istart <= n; istart++ )
  {
    if ( p[istart-1] < 0 )
    {
      continue;
    }
    else if ( p[istart-1] == istart )
    {
      p[istart-1] = -p[istart-1];
      continue;
    }
    else
    {
      a_temp[0] = a[0+(istart-1)*2];
      a_temp[1] = a[1+(istart-1)*2];
      iget = istart;
//
//  Copy the new value into the vacated entry.
//
      for ( ; ; )
      {
        iput = iget;
        iget = p[iget-1];

        p[iput-1] = -p[iput-1];

        if ( iget < 1 || n < iget )
        {
          cerr << "\n";
          cerr << "R82VEC_PERMUTE - Fatal error!\n";
          cerr << "  Entry IPUT = " << iput << " of the permutation has\n";
          cerr << "  an illegal value IGET = " << iget << ".\n";
          exit ( 1 );
        }

        if ( iget == istart )
        {
          a[0+(iput-1)*2] = a_temp[0];
          a[1+(iput-1)*2] = a_temp[1];
          break;
        }
        a[0+(iput-1)*2] = a[0+(iget-1)*2];
        a[1+(iput-1)*2] = a[1+(iget-1)*2];
      }
    }
  }
//
//  Restore the signs of the entries.
//
  for ( i = 0; i < n; i++ )
  {
    p[i] = -p[i];
  }

  return;
}
//****************************************************************************80

void r82vec_print ( int n, double a[], string title )

//****************************************************************************80
//
//  Purpose:
//
//    R82VEC_PRINT prints an R82VEC.
//
//  Discussion:
//
//    A is a two dimensional array of order N by 2, stored as a vector
//    of rows: A(0,0), A(0,1), // A(1,0), A(1,1) // ...
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of components of the vector.
//
//    Input, double A[2*N], the vector to be printed.
//
//    Input, string TITLE, a title.
//
{
  int i;

  cout << "\n";
  cout << title << "\n";
  cout << "\n";
  for ( i = 0; i <= n-1; i++ )
  {
    cout << setw(6)  << i        << ": "
         << setw(14) << a[0+i*2] << "  "
         << setw(14) << a[1+i*2] << "\n";
  }

  return;
}
//****************************************************************************80

int *r82vec_sort_heap_index_a ( int n, double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    R82VEC_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an R82VEC.
//
//  Discussion:
//
//    The sorting is not actually carried out.  Rather an index array is
//    created which defines the sorting.  This array may be used to sort
//    or index the array, or to sort or index related arrays keyed on the
//    original array.
//
//    Once the index array is computed, the sorting can be carried out
//    "implicitly:
//
//      A(1:2,INDX(I)), I = 1 to N is sorted,
//
//    or explicitly, by the call
//
//      call R82VEC_PERMUTE ( N, A, INDX )
//
//    after which A(1:2,I), I = 1 to N is sorted.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 January 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the array.
//
//    Input, double A[2*N], an array to be index-sorted.
//
//    Output, int R82VEC_SORT_HEAP_INDEX_A[N], the sort index.  The
//    I-th element of the sorted array is A(0:1,R82VEC_SORT_HEAP_INDEX_A(I-1)).
//
{
  double aval[2];
  int i;
  int *indx;
  int indxt;
  int ir;
  int j;
  int l;

  if ( n < 1 )
  {
    return NULL;
  }

  if ( n == 1 )
  {
    indx = new int[1];
    indx[0] = 1;
    return indx;
  }

  indx = i4vec_indicator_new ( n );

  l = n / 2 + 1;
  ir = n;

  for ( ; ; )
  {
    if ( 1 < l )
    {
      l = l - 1;
      indxt = indx[l-1];
      aval[0] = a[0+(indxt-1)*2];
      aval[1] = a[1+(indxt-1)*2];
    }
    else
    {
      indxt = indx[ir-1];
      aval[0] = a[0+(indxt-1)*2];
      aval[1] = a[1+(indxt-1)*2];
      indx[ir-1] = indx[0];
      ir = ir - 1;

      if ( ir == 1 )
      {
        indx[0] = indxt;
        break;
      }

    }

    i = l;
    j = l + l;

    while ( j <= ir )
    {
      if ( j < ir )
      {
        if (   a[0+(indx[j-1]-1)*2] <  a[0+(indx[j]-1)*2] ||
             ( a[0+(indx[j-1]-1)*2] == a[0+(indx[j]-1)*2] &&
               a[1+(indx[j-1]-1)*2] <  a[1+(indx[j]-1)*2] ) )
        {
          j = j + 1;
        }
      }

      if (   aval[0] <  a[0+(indx[j-1]-1)*2] ||
           ( aval[0] == a[0+(indx[j-1]-1)*2] &&
             aval[1] <  a[1+(indx[j-1]-1)*2] ) )
      {
        indx[i-1] = indx[j-1];
        i = j;
        j = j + j;
      }
      else
      {
        j = ir + 1;
      }
    }
    indx[i-1] = indxt;
  }

  return indx;
}
//****************************************************************************80

void r82vec_sort_quick_a ( int n, double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    R82VEC_SORT_QUICK_A ascending sorts an R82VEC using quick sort.
//
//  Discussion:
//
//    A is a two dimensional array of order N by 2, stored as a vector
//    of rows: A(0,0), A(0,1), // A(1,0), A(1,1) // ...
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the array.
//
//    Input/output, double A[N*2].
//    On input, the array to be sorted.
//    On output, the array has been sorted.
//
{
# define LEVEL_MAX 25

  int base;
  int l_segment;
  int level;
  int n_segment;
  int rsave[LEVEL_MAX];
  int r_segment;

  if ( n < 1 )
  {
    cerr << "\n";
    cerr << "R82VEC_SORT_QUICK_A - Fatal error!\n";
    cerr << "  N < 1.\n";
    exit ( 1 );
  }

  if ( n == 1 )
  {
    return;
  }

  level = 1;
  rsave[level-1] = n + 1;
  base = 1;
  n_segment = n;

  while ( 0 < n_segment )
  {
//
//  Partition the segment.
//
    r82vec_part_quick_a ( n_segment, a+2*(base-1)+0, &l_segment, &r_segment );
//
//  If the left segment has more than one element, we need to partition it.
//
    if ( 1 < l_segment )
    {
      if ( LEVEL_MAX < level )
      {
        cerr << "\n";
        cerr << "R82VEC_SORT_QUICK_A - Fatal error!\n";
        cerr << "  Exceeding recursion maximum of " << LEVEL_MAX << "\n";
        exit ( 1 );
      }

      level = level + 1;
      n_segment = l_segment;
      rsave[level-1] = r_segment + base - 1;
    }
//
//  The left segment and the middle segment are sorted.
//  Must the right segment be partitioned?
//
    else if ( r_segment < n_segment )
    {
      n_segment = n_segment + 1 - r_segment;
      base = base + r_segment - 1;
    }
//
//  Otherwise, we back up a level if there is an earlier one.
//
    else
    {
      for ( ; ; )
      {
        if ( level <= 1 )
        {
          n_segment = 0;
          break;
        }

        base = rsave[level-1];
        n_segment = rsave[level-2] - rsave[level-1];
        level = level - 1;

        if ( 0 < n_segment )
        {
          break;
        }

      }

    }

  }
  return;
# undef LEVEL_MAX
}
//****************************************************************************80

void r8mat_copy ( int m, int n, double a1[], double a2[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_COPY copies one R8MAT to another.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns.
//
//    Input, double A1[M*N], the matrix to be copied.
//
//    Input, double A2[M*N], the copy of A1.
//
{
  int i;
  int j;

  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < m; i++ )
    {
      a2[i+j*m] = a1[i+j*m];
    }
  }

  return;
}
//****************************************************************************80

double r8mat_det_2d ( double a[2*2] )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_DET_2D computes the determinant of a 2 by 2 R8MAT.
//
//  Discussion:
//
//    The two dimensional array is stored as a one dimensional vector,
//    by COLUMNS.
//
//    The determinant of a 2 by 2 matrix is
//
//      a11 * a22 - a12 * a21.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A[2*2], the matrix whose determinant is desired.
//
//    Output, double R8MAT_DET_2D, the determinant of the matrix.
//
{
  double det;

  det = a[0+0*2] * a[1+1*2] - a[0+1*2] * a[1+0*2];

  return det;
}
//****************************************************************************80

double r8mat_det_3d ( double a[3*3] )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_DET_3D computes the determinant of a 3 by 3 R8MAT.
//
//  Discussion:
//
//    The two dimensional array is stored as a one dimensional vector,
//    by COLUMNS.
//
//    The determinant of a 3 by 3 matrix is
//
//        a11 * a22 * a33 - a11 * a23 * a32
//      + a12 * a23 * a31 - a12 * a21 * a33
//      + a13 * a21 * a32 - a13 * a22 * a31
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A[3*3], the matrix whose determinant is desired.
//
//    Output, double R8MAT_DET_3D, the determinant of the matrix.
//
{
  double det;

  det =
      a[0+0*3] * ( a[1+1*3] * a[2+2*3] - a[1+2*3] * a[2+1*3] )
    + a[0+1*3] * ( a[1+2*3] * a[2+0*3] - a[1+0*3] * a[2+2*3] )
    + a[0+2*3] * ( a[1+0*3] * a[2+1*3] - a[1+1*3] * a[2+0*3] );

  return det;
}
//****************************************************************************80

double r8mat_det_4d ( double a[4*4] )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_DET_4D computes the determinant of a 4 by 4 R8MAT.
//
//  Discussion:
//
//    The two dimensional array is stored as a one dimensional vector,
//    by COLUMNS.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A[4*4], the matrix whose determinant is desired.
//
//    Output, double R8MAT_DET_4D, the determinant of the matrix.
//
{
  double det;

  det =
      a[0+0*4] * (
          a[1+1*4] * ( a[2+2*4] * a[3+3*4] - a[2+3*4] * a[3+2*4] )
        - a[1+2*4] * ( a[2+1*4] * a[3+3*4] - a[2+3*4] * a[3+1*4] )
        + a[1+3*4] * ( a[2+1*4] * a[3+2*4] - a[2+2*4] * a[3+1*4] ) )
    - a[0+1*4] * (
          a[1+0*4] * ( a[2+2*4] * a[3+3*4] - a[2+3*4] * a[3+2*4] )
        - a[1+2*4] * ( a[2+0*4] * a[3+3*4] - a[2+3*4] * a[3+0*4] )
        + a[1+3*4] * ( a[2+0*4] * a[3+2*4] - a[2+2*4] * a[3+0*4] ) )
    + a[0+2*4] * (
          a[1+0*4] * ( a[2+1*4] * a[3+3*4] - a[2+3*4] * a[3+1*4] )
        - a[1+1*4] * ( a[2+0*4] * a[3+3*4] - a[2+3*4] * a[3+0*4] )
        + a[1+3*4] * ( a[2+0*4] * a[3+1*4] - a[2+1*4] * a[3+0*4] ) )
    - a[0+3*4] * (
          a[1+0*4] * ( a[2+1*4] * a[3+2*4] - a[2+2*4] * a[3+1*4] )
        - a[1+1*4] * ( a[2+0*4] * a[3+2*4] - a[2+2*4] * a[3+0*4] )
        + a[1+2*4] * ( a[2+0*4] * a[3+1*4] - a[2+1*4] * a[3+0*4] ) );

  return det;
}
//****************************************************************************80

double r8mat_det_5d ( double a[5*5] )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_DET_5D computes the determinant of a 5 by 5 R8MAT.
//
//  Discussion:
//
//    The two dimensional array is stored as a one dimensional vector,
//    by COLUMNS.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A[5*5], the matrix whose determinant is desired.
//
//    Output, double R8MAT_DET_5D, the determinant of the matrix.
//
{
  double b[4*4];
  double det;
  int i;
  int inc;
  int j;
  int k;
  double sign;
//
//  Expand the determinant into the sum of the determinants of the
//  five 4 by 4 matrices created by dropping row 1, and column k.
//
  det = 0.0;
  sign = 1.0;

  for ( k = 0; k < 5; k++ )
  {
    for ( i = 0; i < 4; i++ )
    {
      for ( j = 0; j < 4; j++ )
      {
        if ( j < k )
        {
          inc = 0;
        }
        else
        {
          inc = 1;
        }
        b[i+j*4] = a[i+1+(j+inc)*5];
      }
    }

    det = det + sign * a[0+k*5] * r8mat_det_4d ( b );

    sign = -sign;

  }

  return det;
}
//****************************************************************************80

double *r8mat_inverse_2d ( double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_INVERSE_2D inverts a 2 by 2 R8MAT using Cramer's rule.
//
//  Discussion:
//
//    The two dimensional array is stored as a one dimensional vector,
//    by COLUMNS.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A[2*2], the matrix to be inverted.
//
//    Output, double R8MAT_INVERSE_2D[2*2], the inverse of the matrix A.
//
{
  double *b;
  double det;
//
//  Compute the determinant of A.
//
  det = a[0+0*2] * a[1+1*2] - a[0+1*2] * a[1+0*2];
//
//  If the determinant is zero, bail out.
//
  if ( det == 0.0 )
  {
    return NULL;
  }
//
//  Compute the entries of the inverse matrix using an explicit formula.
//
  b = new double[2*2];

  b[0+0*2] = + a[1+1*2] / det;
  b[0+1*2] = - a[0+1*2] / det;
  b[1+0*2] = - a[1+0*2] / det;
  b[1+1*2] = + a[0+0*2] / det;

  return b;
}
//****************************************************************************80

double *r8mat_inverse_3d ( double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_INVERSE_3D inverts a 3 by 3 R8MAT using Cramer's rule.
//
//  Discussion:
//
//    The two dimensional array is stored as a one dimensional vector,
//    by COLUMNS.
//
//    If the determinant is zero, A is singular, and does not have an
//    inverse.  In that case, the output is set to NULL.
//
//    If the determinant is nonzero, its value is an estimate
//    of how nonsingular the matrix A is.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A[3*3], the matrix to be inverted.
//
//    Output, double R8MAT3_INVERSE[3*3], the inverse of the matrix A.
//
{
  double *b;
  double det;
//
//  Compute the determinant of A.
//
  det =
     a[0+0*3] * ( a[1+1*3] * a[2+2*3] - a[1+2*3] * a[2+1*3] )
   + a[0+1*3] * ( a[1+2*3] * a[2+0*3] - a[1+0*3] * a[2+2*3] )
   + a[0+2*3] * ( a[1+0*3] * a[2+1*3] - a[1+1*3] * a[2+0*3] );

  if ( det == 0.0 )
  {
    return NULL;
  }

  b = new double[3*3];

  b[0+0*3] =   ( a[1+1*3] * a[2+2*3] - a[1+2*3] * a[2+1*3] ) / det;
  b[0+1*3] = - ( a[0+1*3] * a[2+2*3] - a[0+2*3] * a[2+1*3] ) / det;
  b[0+2*3] =   ( a[0+1*3] * a[1+2*3] - a[0+2*3] * a[1+1*3] ) / det;

  b[1+0*3] = - ( a[1+0*3] * a[2+2*3] - a[1+2*3] * a[2+0*3] ) / det;
  b[1+1*3] =   ( a[0+0*3] * a[2+2*3] - a[0+2*3] * a[2+0*3] ) / det;
  b[1+2*3] = - ( a[0+0*3] * a[1+2*3] - a[0+2*3] * a[1+0*3] ) / det;

  b[2+0*3] =   ( a[1+0*3] * a[2+1*3] - a[1+1*3] * a[2+0*3] ) / det;
  b[2+1*3] = - ( a[0+0*3] * a[2+1*3] - a[0+1*3] * a[2+0*3] ) / det;
  b[2+2*3] =   ( a[0+0*3] * a[1+1*3] - a[0+1*3] * a[1+0*3] ) / det;

  return b;
}
//****************************************************************************80

double *r8mat_mv ( int m, int n, double a[], double x[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_MV multiplies a matrix times a vector.
//
//  Discussion:
//
//    A R8MAT is a doubly dimensioned array of double precision values, which
//    may be stored as a vector in column-major order.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    18 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns of the matrix.
//
//    Input, double A[M,N], the M by N matrix.
//
//    Input, double X[N], the vector to be multiplied by A.
//
//    Output, double R8MAT_MV[M], the product A*X.
//
{
  int i;
  int j;
  double *y;

  y = new double[m];

  for ( i = 0; i < m; i++ )
  {
    y[i] = 0.0;
    for ( j = 0; j < n; j++ )
    {
      y[i] = y[i] + a[i+j*n] * x[j];
    }
  }

  return y;
}
//****************************************************************************80

void r8mat_print ( int m, int n, double a[], string title )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_PRINT prints an R8MAT, with an optional title.
//
//  Discussion:
//
//    The doubly dimensioned array A is treated as a one dimensional vector,
//    stored by COLUMNS.  Entry A(I,J) is stored as A[I+J*M]
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the number of rows in A.
//
//    Input, int N, the number of columns in A.
//
//    Input, double A[M*N], the M by N matrix.
//
//    Input, string TITLE, a title.
//
{
  r8mat_print_some ( m, n, a, 1, 1, m, n, title );

  return;
}
//****************************************************************************80

void r8mat_print_some ( int m, int n, double a[], int ilo, int jlo, int ihi,
  int jhi, string title )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_PRINT_SOME prints some of an R8MAT.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 April 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the number of rows of the matrix.
//    M must be positive.
//
//    Input, int N, the number of columns of the matrix.
//    N must be positive.
//
//    Input, double A[M*N], the matrix.
//
//    Input, int ILO, JLO, IHI, JHI, designate the first row and
//    column, and the last row and column to be printed.
//
//    Input, string TITLE, a title.
{
# define INCX 5

  int i;
  int i2hi;
  int i2lo;
  int j;
  int j2hi;
  int j2lo;

  cout << "\n";
  cout << title << "\n";
//
//  Print the columns of the matrix, in strips of 5.
//
  for ( j2lo = jlo; j2lo <= jhi; j2lo = j2lo + INCX )
  {
    j2hi = j2lo + INCX - 1;
    j2hi = i4_min ( j2hi, n );
    j2hi = i4_min ( j2hi, jhi );

    cout << "\n";
//
//  For each column J in the current range...
//
//  Write the header.
//
    cout << "  Col:    ";
    for ( j = j2lo; j <= j2hi; j++ )
    {
      cout << setw(7) << j << "       ";
    }
    cout << "\n";
    cout << "  Row\n";
    cout << "\n";
//
//  Determine the range of the rows in this strip.
//
    i2lo = i4_max ( ilo, 1 );
    i2hi = i4_min ( ihi, m );

    for ( i = i2lo; i <= i2hi; i++ )
    {
//
//  Print out (up to) 5 entries in row I, that lie in the current strip.
//
      cout << setw(5) << i << ": ";
      for ( j = j2lo; j <= j2hi; j++ )
      {
        cout << setw(12) << a[i-1+(j-1)*m] << "  ";
      }
      cout << "\n";
    }
  }

  return;
# undef INCX
}
//****************************************************************************80

int r8mat_solve ( int n, int rhs_num, double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_SOLVE uses Gauss-Jordan elimination to solve an N by N linear system.
//
//  Discussion:
//
//    The doubly dimensioned array A is treated as a one dimensional vector,
//    stored by COLUMNS.  Entry A(I,J) is stored as A[I+J*N]
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the order of the matrix.
//
//    Input, int RHS_NUM, the number of right hand sides.  RHS_NUM
//    must be at least 0.
//
//    Input/output, double A[N*(N+RHS_NUM)], contains in rows and columns 1
//    to N the coefficient matrix, and in columns N+1 through
//    N+RHS_NUM, the right hand sides.  On output, the coefficient matrix
//    area has been destroyed, while the right hand sides have
//    been overwritten with the corresponding solutions.
//
//    Output, int R8MAT_SOLVE, singularity flag.
//    0, the matrix was not singular, the solutions were computed;
//    J, factorization failed on step J, and the solutions could not
//    be computed.
//
{
  double apivot;
  double factor;
  int i;
  int ipivot;
  int j;
  int k;
  double temp;

  for ( j = 0; j < n; j++ )
  {
//
//  Choose a pivot row.
//
    ipivot = j;
    apivot = a[j+j*n];

    for ( i = j; i < n; i++ )
    {
      if ( fabs ( apivot ) < fabs ( a[i+j*n] ) )
      {
        apivot = a[i+j*n];
        ipivot = i;
      }
    }

    if ( apivot == 0.0 )
    {
      return j;
    }
//
//  Interchange.
//
    for ( i = 0; i < n + rhs_num; i++ )
    {
      temp          = a[ipivot+i*n];
      a[ipivot+i*n] = a[j+i*n];
      a[j+i*n]      = temp;
    }
//
//  A(J,J) becomes 1.
//
    a[j+j*n] = 1.0;
    for ( k = j; k < n + rhs_num; k++ )
    {
      a[j+k*n] = a[j+k*n] / apivot;
    }
//
//  A(I,J) becomes 0.
//
    for ( i = 0; i < n; i++ )
    {
      if ( i != j )
      {
        factor = a[i+j*n];
        a[i+j*n] = 0.0;
        for ( k = j; k < n + rhs_num; k++ )
        {
          a[i+k*n] = a[i+k*n] - factor * a[j+k*n];
        }
      }

    }

  }

  return 0;
}
//****************************************************************************80

double *r8mat_solve_2d ( double a[], double b[], double *det )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_SOLVE_2D solves a 2 by 2 linear system using Cramer's rule.
//
//  Discussion:
//
//    If DET is zero, then A is singular, and does not have an
//    inverse.  In that case, X is simply set to zero.
//
//    If DET is nonzero, then its value is roughly an estimate
//    of how nonsingular the matrix A is.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 November 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A[2*2], the matrix.
//
//    Input, double B[2], the right hand side.
//
//    Output, double *DET, the determinant of the system.
//
//    Output, double R8MAT_SOLVE_2D[2], the solution of the system, if DET is nonzero.
//    Otherwise, the NULL vector.
//
{
  double *x;
//
//  Compute the determinant.
//
  *det = a[0+0*2] * a[1+1*2] - a[0+1*2] * a[1+0*2];
//
//  If the determinant is zero, bail out.
//
  if ( *det == 0.0 )
  {
    return NULL;
  }
//
//  Compute the solution.
//
  x = new double[2];

  x[0] = (  a[1+1*2] * b[0] - a[0+1*2] * b[1] ) / ( *det );
  x[1] = ( -a[1+0*2] * b[0] + a[0+0*2] * b[1] ) / ( *det );

  return x;
}
//****************************************************************************80

void r8mat_transpose_print ( int m, int n, double a[], string title )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 August 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns.
//
//    Input, double A[M*N], an M by N matrix to be printed.
//
//    Input, string TITLE, a title.
//
{
  r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title );

  return;
}
//****************************************************************************80

void r8mat_transpose_print_some ( int m, int n, double a[], int ilo, int jlo,
  int ihi, int jhi, string title )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 August 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns.
//
//    Input, double A[M*N], an M by N matrix to be printed.
//
//    Input, int ILO, JLO, the first row and column to print.
//
//    Input, int IHI, JHI, the last row and column to print.
//
//    Input, string TITLE, a title.
//
{
# define INCX 5

  int i;
  int i2;
  int i2hi;
  int i2lo;
  int inc;
  int j;
  int j2hi;
  int j2lo;

  cout << "\n";
  cout << title << "\n";

  for ( i2lo = i4_max ( ilo, 1 ); i2lo <= i4_min ( ihi, m ); i2lo = i2lo + INCX )
  {
    i2hi = i2lo + INCX - 1;
    i2hi = i4_min ( i2hi, m );
    i2hi = i4_min ( i2hi, ihi );

    inc = i2hi + 1 - i2lo;

    cout << "\n";
    cout << "  Row: ";
    for ( i = i2lo; i <= i2hi; i++ )
    {
      cout << setw(7) << i << "       ";
    }
    cout << "\n";
    cout << "  Col\n";
    cout << "\n";

    j2lo = i4_max ( jlo, 1 );
    j2hi = i4_min ( jhi, n );

    for ( j = j2lo; j <= j2hi; j++ )
    {
      cout << setw(5) << j << ":";
      for ( i2 = 1; i2 <= inc; i2++ )
      {
        i = i2lo - 1 + i2;
        cout << setw(14) << a[(i-1)+(j-1)*m];
      }
      cout << "\n";
    }
  }

  return;
# undef INCX
}
//****************************************************************************80

double *r8mat_uniform_ab_new ( int m, int n, double a, double b, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_UNIFORM_AB_NEW returns a scaled pseudorandom R8MAT.
//
//  Discussion:
//
//    A R8MAT is a doubly dimensioned array of double precision values, which
//    may be stored as a vector in column-major order.
//
//    This routine implements the recursion
//
//      seed = 16807 * seed mod ( 2**31 - 1 )
//      unif = seed / ( 2**31 - 1 )
//
//    The integer arithmetic never requires more than 32 bits,
//    including a sign bit.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paul Bratley, Bennett Fox, Linus Schrage,
//    A Guide to Simulation,
//    Springer Verlag, pages 201-202, 1983.
//
//    Bennett Fox,
//    Algorithm 647:
//    Implementation and Relative Efficiency of Quasirandom
//    Sequence Generators,
//    ACM Transactions on Mathematical Software,
//    Volume 12, Number 4, pages 362-376, 1986.
//
//    Peter Lewis, Allen Goodman, James Miller,
//    A Pseudo-Random Number Generator for the System/360,
//    IBM Systems Journal,
//    Volume 8, pages 136-143, 1969.
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns.
//
//    Input, double A, B, the limits of the pseudorandom values.
//
//    Input/output, int &SEED, the "seed" value.  Normally, this
//    value should not be 0, otherwise the output value of SEED
//    will still be 0, and r8_uniFORM will be 0.  On output, SEED has
//    been updated.
//
//    Output, double R8MAT_UNIFORM_NEW[M*N], a matrix of pseudorandom values.
//
{
  int i;
  int j;
  int k;
  double *r;

  r = new double[m*n];

  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < m; i++ )
    {
      k = seed / 127773;

      seed = 16807 * ( seed - k * 127773 ) - k * 2836;

      if ( seed < 0 )
      {
        seed = seed + 2147483647;
      }
//
//  Although SEED can be represented exactly as a 32 bit integer,
//  it generally cannot be represented exactly as a 32 bit real number!
//
      r[i+j*m] = a + ( b - a ) * ( double ) ( seed ) * 4.656612875E-10;
    }
  }

  return r;
}
//****************************************************************************80

void r8mat_uniform_01 ( int m, int n, int &seed, double r[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_UNIFORM_01 returns a unit pseudorandom R8MAT.
//
//  Discussion:
//
//    This routine implements the recursion
//
//      seed = ( 16807 * seed ) mod ( 2^31 - 1 )
//      u = seed / ( 2^31 - 1 )
//
//    The integer arithmetic never requires more than 32 bits,
//    including a sign bit.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paul Bratley, Bennett Fox, Linus Schrage,
//    A Guide to Simulation,
//    Second Edition,
//    Springer, 1987,
//    ISBN: 0387964673,
//    LC: QA76.9.C65.B73.
//
//    Bennett Fox,
//    Algorithm 647:
//    Implementation and Relative Efficiency of Quasirandom
//    Sequence Generators,
//    ACM Transactions on Mathematical Software,
//    Volume 12, Number 4, December 1986, pages 362-376.
//
//    Pierre L'Ecuyer,
//    Random Number Generation,
//    in Handbook of Simulation,
//    edited by Jerry Banks,
//    Wiley, 1998,
//    ISBN: 0471134031,
//    LC: T57.62.H37.
//
//    Peter Lewis, Allen Goodman, James Miller,
//    A Pseudo-Random Number Generator for the System/360,
//    IBM Systems Journal,
//    Volume 8, Number 2, 1969, pages 136-143.
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns.
//
//    Input/output, int &SEED, the "seed" value.  Normally, this
//    value should not be 0.  On output, SEED has
//    been updated.
//
//    Output, double R[M*N], a matrix of pseudorandom values.
//
{
  int i;
  int i4_huge = 2147483647;
  int j;
  int k;

  if ( seed == 0 )
  {
    cerr << "\n";
    cerr << "R8MAT_UNIFORM_01 - Fatal error!\n";
    cerr << "  Input value of SEED = 0.\n";
    exit ( 1 );
  }

  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < m; i++ )
    {
      k = seed / 127773;

      seed = 16807 * ( seed - k * 127773 ) - k * 2836;

      if ( seed < 0 )
      {
        seed = seed + i4_huge;
      }

      r[i+j*m] = ( double ) ( seed ) * 4.656612875E-10;
    }
  }

  return;
}
//****************************************************************************80

double *r8mat_uniform_01_new ( int m, int n, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_UNIFORM_01_NEW returns a unit pseudorandom R8MAT.
//
//  Discussion:
//
//    This routine implements the recursion
//
//      seed = ( 16807 * seed ) mod ( 2^31 - 1 )
//      u = seed / ( 2^31 - 1 )
//
//    The integer arithmetic never requires more than 32 bits,
//    including a sign bit.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paul Bratley, Bennett Fox, Linus Schrage,
//    A Guide to Simulation,
//    Second Edition,
//    Springer, 1987,
//    ISBN: 0387964673,
//    LC: QA76.9.C65.B73.
//
//    Bennett Fox,
//    Algorithm 647:
//    Implementation and Relative Efficiency of Quasirandom
//    Sequence Generators,
//    ACM Transactions on Mathematical Software,
//    Volume 12, Number 4, December 1986, pages 362-376.
//
//    Pierre L'Ecuyer,
//    Random Number Generation,
//    in Handbook of Simulation,
//    edited by Jerry Banks,
//    Wiley, 1998,
//    ISBN: 0471134031,
//    LC: T57.62.H37.
//
//    Peter Lewis, Allen Goodman, James Miller,
//    A Pseudo-Random Number Generator for the System/360,
//    IBM Systems Journal,
//    Volume 8, Number 2, 1969, pages 136-143.
//
//  Parameters:
//
//    Input, int M, N, the number of rows and columns.
//
//    Input/output, int &SEED, the "seed" value.  Normally, this
//    value should not be 0.  On output, SEED has
//    been updated.
//
//    Output, double R8MAT_UNIFORM_01[M*N], a matrix of pseudorandom values.
//
{
  int i;
  int i4_huge = 2147483647;
  int j;
  int k;
  double *r;

  if ( seed == 0 )
  {
    cerr << "\n";
    cerr << "R8MAT_UNIFORM_01 - Fatal error!\n";
    cerr << "  Input value of SEED = 0.\n";
    exit ( 1 );
  }

  r = new double[m*n];

  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < m; i++ )
    {
      k = seed / 127773;

      seed = 16807 * ( seed - k * 127773 ) - k * 2836;

      if ( seed < 0 )
      {
        seed = seed + i4_huge;
      }

      r[i+j*m] = ( double ) ( seed ) * 4.656612875E-10;
    }
  }

  return r;
}
//****************************************************************************80

double r8vec_angle_3d ( double u[], double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_ANGLE_3D computes the angle between two vectors in 3D.
//
//  Modified:
//
//    07 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double U[3], V[3], the vectors.
//
//    Output, double ANGLE, the angle between the two vectors.
//
{
  double angle;
  double angle_cos;
  double u_norm;
  double uv_dot;
  double v_norm;

  uv_dot = r8vec_dot_product ( 3, u, v );

  u_norm = sqrt ( r8vec_dot_product ( 3, u, u ) );

  v_norm = sqrt ( r8vec_dot_product ( 3, v, v ) );

  angle_cos = uv_dot / u_norm / v_norm;

  angle = r8_acos ( angle_cos );

  return angle;
}
//****************************************************************************80

double *r8vec_any_normal ( int dim_num, double v1[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_ANY_NORMAL returns some normal vector to V1.
//
//  Discussion:
//
//    If DIM_NUM < 2, then no normal vector can be returned.
//
//    If V1 is the zero vector, then any unit vector will do.
//
//    No doubt, there are better, more robust algorithms.  But I will take
//    just about ANY reasonable unit vector that is normal to V1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, double V1[DIM_NUM], the vector.
//
//    Output, double R8VEC_ANY_NORMAL[DIM_NUM], a vector that is
//    normal to V2, and has unit Euclidean length.
//
{
  int i;
  int j;
  int k;
  double *v2;
  double vj;
  double vk;

  if ( dim_num < 2 )
  {
    cerr << "\n";
    cerr << "R8VEC_ANY_NORMAL - Fatal error!\n";
    cerr << "  Called with DIM_NUM < 2.\n";
    exit ( 1 );
  }

  v2 = new double[dim_num];

  if ( r8vec_norm ( dim_num, v1 ) == 0.0 )
  {
    r8vec_zero ( dim_num, v2 );
    v2[0] = 1.0;
    return v2;
  }
//
//  Seek the largest entry in V1, VJ = V1(J), and the
//  second largest, VK = V1(K).
//
//  Since V1 does not have zero norm, we are guaranteed that
//  VJ, at least, is not zero.
//
  j = -1;
  vj = 0.0;

  k = -1;
  vk = 0.0;

  for ( i = 0; i < dim_num; i++ )
  {
    if ( fabs ( vk ) < fabs ( v1[i] ) || k == -1 )
    {
      if ( fabs ( vj ) < fabs ( v1[i] ) || j == -1 )
      {
        k = j;
        vk = vj;
        j = i;
        vj = v1[i];
      }
      else
      {
        k = i;
        vk = v1[i];
      }
    }
  }
//
//  Setting V2 to zero, except that V2(J) = -VK, and V2(K) = VJ,
//  will just about do the trick.
//
  r8vec_zero ( dim_num, v2 );

  v2[j] = -vk / sqrt ( vk * vk + vj * vj );
  v2[k] =  vj / sqrt ( vk * vk + vj * vj );

  return v2;
}
//****************************************************************************80

void r8vec_bracket ( int n, double x[], double xval, int *left,
  int *right )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_BRACKET searches a sorted R8VEC for successive brackets of a value.
//
//  Discussion:
//
//    If the values in the vector are thought of as defining intervals
//    on the real line, then this routine searches for the interval
//    nearest to or containing the given value.
//
//    It is always true that RIGHT = LEFT+1.
//
//    If XVAL < X[0], then LEFT = 1, RIGHT = 2, and
//      XVAL   < X[0] < X[1];
//    If X(1) <= XVAL < X[N-1], then
//      X[LEFT-1] <= XVAL < X[RIGHT-1];
//    If X[N-1] <= XVAL, then LEFT = N-1, RIGHT = N, and
//      X[LEFT-1] <= X[RIGHT-1] <= XVAL.
//
//    For consistency, this routine computes indices RIGHT and LEFT
//    that are 1-based, although it would be more natural in C and
//    C++ to use 0-based values.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    24 February 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, length of input array.
//
//    Input, double X[N], an array that has been sorted into ascending order.
//
//    Input, double XVAL, a value to be bracketed.
//
//    Output, int *LEFT, *RIGHT, the results of the search.
//
{
  int i;

  for ( i = 2; i <= n - 1; i++ )
  {
    if ( xval < x[i-1] )
    {
      *left = i - 1;
      *right = i;
      return;
    }

   }

  *left = n - 1;
  *right = n;

  return;
}
//****************************************************************************80

void r8vec_copy ( int n, double a1[], double a2[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_COPY copies an R8VEC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the vectors.
//
//    Input, double A1[N], the vector to be copied.
//
//    Input, double A2[N], the copy of A1.
//
{
  int i;

  for ( i = 0; i < n; i++ )
  {
    a2[i] = a1[i];
  }
  return;
}
//****************************************************************************80

double r8vec_cross_product_2d ( double v1[2], double v2[2] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_CROSS_PRODUCT_2D finds the cross product of a pair of R8VEC's in 2D.
//
//  Discussion:
//
//    Strictly speaking, the vectors lie in the (X,Y) plane, and
//    the cross product here is a vector in the Z direction.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V1[2], V2[2], the vectors.
//
//    Output, double R8VEC_CROSS_PRODUCT_2D, the Z component of the cross product
//    of V1 and V2.
//
{
  double value;

  value = v1[0] * v2[1] - v1[1] * v2[0];

  return value;
}
//****************************************************************************80

double r8vec_cross_product_affine_2d ( double v0[2], double v1[2],
  double v2[2] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_CROSS_PRODUCT_AFFINE_2D finds the affine cross product in 2D.
//
//  Discussion:
//
//    Strictly speaking, the vectors lie in the (X,Y) plane, and
//    the cross product here is a vector in the Z direction.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 October 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V0[2], the base vector.
//
//    Input, double V1[2], V2[2], the vectors.
//
//    Output, double R8VEC_CROSS_PRODUCT_AFFINE_2D, the Z component of the
//    cross product of V1 and V2.
//
{
  double value;

  value =
      ( v1[0] - v0[0] ) * ( v2[1] - v0[1] )
    - ( v2[0] - v0[0] ) * ( v1[1] - v0[1] );

  return value;
}
//****************************************************************************80

double *r8vec_cross_product_3d ( double v1[3], double v2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_CROSS_PRODUCT_3D computes the cross product of two R8VEC's in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V1[3], V2[3], the coordinates of the vectors.
//
//    Output, double R8VEC_CROSS_PRODUCT_3D[3], the cross product vector.
//
{
  double *v3;

  v3 = new double[3];

  v3[0] = v1[1] * v2[2] - v1[2] * v2[1];
  v3[1] = v1[2] * v2[0] - v1[0] * v2[2];
  v3[2] = v1[0] * v2[1] - v1[1] * v2[0];

  return v3;
}
//****************************************************************************80

double *r8vec_cross_product_affine_3d ( double v0[3], double v1[3],
  double v2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_CROSS_PRODUCT_AFFINE_3D computes the affine cross product in 3D.
//
//  Discussion:
//
//    An R8VEC is a vector of R8's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 October 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V0[3], the base vector.
//
//    Input, double V1[3], V2[3], the coordinates of the vectors.
//
//    Output, double R8VEC_CROSS_PRODUCT_AFFINE_3D[3], the cross product vector.
//
{
  double *v3;

  v3 = ( double * ) malloc ( 3 * sizeof ( double ) );

  v3[0] =
      ( v1[1] - v0[1] ) * ( v2[2] - v0[2] )
    - ( v2[1] - v0[1] ) * ( v1[2] - v0[2] );

  v3[1] =
      ( v1[2] - v0[2] ) * ( v2[0] - v0[0] )
    - ( v2[2] - v0[2] ) * ( v1[0] - v0[0] );

  v3[2] =
      ( v1[0] - v0[0] ) * ( v2[1] - v0[1] )
    - ( v2[0] - v0[0] ) * ( v1[1] - v0[1] );

  return v3;
}
//****************************************************************************80

double r8vec_distance ( int dim_num, double v1[], double v2[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_DISTANCE returns the Euclidean distance between two R8VEC's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, double V1[DIM_NUM], V2[DIM_NUM], the vectors.
//
//    Output, double R8VEC_DISTANCE, the Euclidean distance
//    between the vectors.
//
{
  int i;
  double value;

  value = 0.0;
  for ( i = 0; i < dim_num; i++ )
  {
    value = value + pow ( v1[i] - v2[i], 2 );
  }
  value = sqrt ( value );

  return value;
}
//****************************************************************************80

double r8vec_dot_product ( int n, double a1[], double a2[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_DOT_PRODUCT computes the dot product of a pair of R8VEC's.
//
//  Discussion:
//
//    An R8VEC is a vector of R8's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the vectors.
//
//    Input, double A1[N], A2[N], the two vectors to be considered.
//
//    Output, double R8VEC_DOT_PRODUCT, the dot product of the vectors.
//
{
  int i;
  double value;

  value = 0.0;
  for ( i = 0; i < n; i++ )
  {
    value = value + a1[i] * a2[i];
  }
  return value;
}
//****************************************************************************80

double r8vec_dot_product_affine ( int n, double v0[], double v1[], double v2[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_DOT_PRODUCT_AFFINE computes the affine dot product.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 October 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the vectors.
//
//    Input, double V0[N], the base vector.
//
//    Input, double V1[N], V2[N], the two vectors to be considered.
//
//    Output, double R8VEC_DOT_PRODUCT_AFFINE, the dot product of the vectors.
//
{
  int i;
  double value;

  value = 0.0;
  for ( i = 0; i < n; i++ )
  {
    value = value + ( v1[i] - v0[i] ) * ( v2[i] - v0[i] );
  }
  return value;
}
//****************************************************************************80

bool r8vec_eq ( int n, double a1[], double a2[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_EQ is true two R8VEC's are equal.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the vectors.
//
//    Input, double A1[N], A2[N], two vectors to compare.
//
//    Output, bool R8VEC_EQ.
//    R8VEC_EQ is TRUE if every pair of elements A1(I) and A2(I) are equal,
//    and FALSE otherwise.
//
{
  int i;

  for ( i = 0; i < n; i++ )
  {
    if ( a1[i] != a2[i] )
    {
      return false;
    }
  }
  return true;
}
//****************************************************************************80

bool r8vec_gt ( int n, double a1[], double a2[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_GT == ( A1 > A2 ) for R8VEC's.
//
//  Discussion:
//
//    The comparison is lexicographic.
//
//    A1 > A2  <=>                              A1(1) > A2(1) or
//                 ( A1(1)     == A2(1)     and A1(2) > A2(2) ) or
//                 ...
//                 ( A1(1:N-1) == A2(1:N-1) and A1(N) > A2(N)
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the dimension of the vectors.
//
//    Input, double A1[N], A2[N], the vectors to be compared.
//
//    Output, bool R8VEC_GT, is TRUE if and only if A1 > A2.
//
{
  int i;

  for ( i = 0; i < n; i++ )
  {
    if ( a2[i] < a1[i] )
    {
       return true;
    }
    else if ( a1[i] < a2[i] )
    {
      return false;
    }
  }

  return false;
}
//****************************************************************************80

bool r8vec_lt ( int n, double a1[], double a2[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_LT == ( A1 < A2 ) for R8VEC's.
//
//  Discussion:
//
//    The comparison is lexicographic.
//
//    A1 < A2  <=>                              A1(1) < A2(1) or
//                 ( A1(1)     == A2(1)     and A1(2) < A2(2) ) or
//                 ...
//                 ( A1(1:N-1) == A2(1:N-1) and A1(N) < A2(N)
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the dimension of the vectors.
//
//    Input, double A1[N], A2[N], the vectors to be compared.
//
//    Output, bool R8VEC_LT, is TRUE if and only if A1 < A2.
//
{
  int i;

  for ( i = 0; i < n; i++ )
  {
    if ( a1[i] < a2[i] )
    {
      return true;
    }
    else if ( a2[i] < a1[i] )
    {
      return false;
    }
  }
  return false;
}
//****************************************************************************80

double r8vec_max ( int n, double *r8vec )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_MAX returns the value of the maximum element in an R8VEC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    15 October 1998
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the array.
//
//    Input, double *R8VEC, a pointer to the first entry of the array.
//
//    Output, double R8VEC_MAX, the value of the maximum element.  This
//    is set to 0.0 if N <= 0.
//
{
  int i;
  double rmax = 0.0;
  double *r8vec_pointer = NULL;

  if ( n <= 0 )
  {
    rmax = 0.0;
    return rmax;
  }

  for ( i = 0; i < n; i++ )
  {
    if ( i == 0 )
    {
      rmax = *r8vec;
      r8vec_pointer = r8vec;
    }
    else
    {
      r8vec_pointer++;
      if ( rmax < *r8vec_pointer )
      {
        rmax = *r8vec_pointer;
      }
    }
  }

  return rmax;

}
//****************************************************************************80

double r8vec_mean ( int n, double x[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_MEAN returns the mean of an R8VEC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 May 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the vector.
//
//    Input, double X[N], the vector whose mean is desired.
//
//    Output, double R8VEC_MEAN, the mean, or average, of the vector entries.
//
{
  int i;
  double mean;

  mean = 0.0;
  for ( i = 0; i < n; i++ )
  {
    mean = mean + x[i];
  }

  mean = mean / ( double ) n;

  return mean;
}
//****************************************************************************80

double r8vec_min ( int n, double *r8vec )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_MIN returns the value of the minimum element in an R8VEC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    15 October 1998
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the array.
//
//    Input, double *R8VEC, a pointer to the first entry of the array.
//
//    Output, double R8VEC_MIN, the value of the minimum element.  This
//    is set to 0.0 if N <= 0.
//
{
  int i;
  double rmin = 0.0;
  double *r8vec_pointer = NULL;

  if ( n <= 0 )
  {
    rmin = 0.0;
    return rmin;
  }

  for ( i = 0; i < n; i++ )
  {
    if ( i == 0 )
    {
      rmin = *r8vec;
      r8vec_pointer = r8vec;
    }
    else
    {
      r8vec_pointer++;
      if ( *r8vec_pointer < rmin )
      {
        rmin = *r8vec_pointer;
      }
    }
  }

  return rmin;
}
//****************************************************************************80

bool r8vec_negative_strict ( int n, double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_NEGATIVE_STRICT: all entries of R8VEC are strictly negative.
//
//  Discussion:
//
//    An R8VEC is a vector of R8's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    24 June 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the dimension of the vector.
//
//    Input, double A[N], the vector.
//
//    Output, bool R8VEC_NEGATIVE_STRICT, is TRUE if every entry of
//    A is strictly negative.
//
{
  int i;
  bool value;

  for ( i = 0; i < n; i++ )
  {
    if ( 0 <= a[i] )
    {
      value = false;
      return value;
    }
  }
  value = true;
  return value;
}
//****************************************************************************80

double r8vec_norm ( int n, double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    r8vec_norm() returns the L2 norm of an R8VEC.
//
//  Discussion:
//
//    An R8VEC is a vector of R8's.
//
//    The vector L2 norm is defined as:
//
//      R8VEC_NORM = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 March 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in A.
//
//    Input, double A[N], the vector whose L2 norm is desired.
//
//    Output, double R8VEC_NORM, the L2 norm of A.
//
{
  int i;
  double v;

  v = 0.0;

  for ( i = 0; i < n; i++ )
  {
    v = v + a[i] * a[i];
  }
  v = sqrt ( v );

  return v;
}
//****************************************************************************80

double r8vec_norm_affine ( int n, double v0[], double v1[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_NORM_AFFINE returns the affine L2 norm of an R8VEC.
//
//  Discussion:
//
//    The affine vector L2 norm is defined as:
//
//      R8VEC_NORM_AFFINE(V0,V1)
//        = sqrt ( sum ( 1 <= I <= N ) ( V1(I) - V0(I) )^2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 October 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the dimension of the vectors.
//
//    Input, double V0[N], the base vector.
//
//    Input, double V1[N], the vector whose affine L2 norm is desired.
//
//    Output, double R8VEC_NORM_AFFINE, the affine L2 norm of V1.
//
{
  int i;
  double value;

  value = 0.0;

  for ( i = 0; i < n; i++ )
  {
    value = value + ( v1[i] - v0[i] ) * ( v1[i] - v0[i] );
  }
  value = sqrt ( value );

  return value;
}
//****************************************************************************80

double *r8vec_normal_01_new ( int n, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_NORMAL_01_NEW returns a unit pseudonormal R8VEC.
//
//  Discussion:
//
//    The standard normal probability distribution function (PDF) has
//    mean 0 and standard deviation 1.
//
//    This routine can generate a vector of values on one call.  It
//    has the feature that it should provide the same results
//    in the same order no matter how we break up the task.
//
//    Before calling this routine, the user may call RANDOM_SEED
//    in order to set the seed of the random number generator.
//
//    The Box-Muller method is used, which is efficient, but
//    generates an even number of values each time.  On any call
//    to this routine, an even number of new values are generated.
//    Depending on the situation, one value may be left over.
//    In that case, it is saved for the next call.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 February 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of values desired.  If N is negative,
//    then the code will flush its internal memory; in particular,
//    if there is a saved value to be used on the next call, it is
//    instead discarded.  This is useful if the user has reset the
//    random number seed, for instance.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double X(N), a sample of the standard normal PDF.
//
//  Local parameters:
//
//    Local, int MADE, records the number of values that have
//    been computed.  On input with negative N, this value overwrites
//    the return value of N, so the user can get an accounting of
//    how much work has been done.
//
//    Local, double R(N+1), is used to store some uniform random values.
//    Its dimension is N+1, but really it is only needed to be the
//    smallest even number greater than or equal to N.
//
//    Local, int SAVED, is 0 or 1 depending on whether there is a
//    single saved value left over from the previous call.
//
//    Local, int X_LO, X_HI, records the range of entries of
//    X that we need to compute.  This starts off as 1:N, but is adjusted
//    if we have a saved value that can be immediately stored in X(1),
//    and so on.
//
//    Local, double Y, the value saved from the previous call, if
//    SAVED is 1.
//
{
  int i;
  int m;
  static int made = 0;
  double r8_pi = 3.141592653589793;
  double *r;
  static int saved = 0;
  double *x;
  int x_hi;
  int x_lo;
  static double y = 0.0;
//
//  I'd like to allow the user to reset the internal data.
//  But this won't work properly if we have a saved value Y.
//  I'm making a crock option that allows the user to signal
//  explicitly that any internal memory should be flushed,
//  by passing in a negative value for N.
//
  if ( n < 0 )
  {
    made = 0;
    saved = 0;
    y = 0.0;
    return NULL;
  }
  else if ( n == 0 )
  {
    return NULL;
  }

  x = new double[n];
//
//  Record the range of X we need to fill in.
//
  x_lo = 1;
  x_hi = n;
//
//  Use up the old value, if we have it.
//
  if ( saved == 1 )
  {
    x[0] = y;
    saved = 0;
    x_lo = 2;
  }
//
//  Maybe we don't need any more values.
//
  if ( x_hi - x_lo + 1 == 0 )
  {
  }
//
//  If we need just one new value, do that here to avoid null arrays.
//
  else if ( x_hi - x_lo + 1 == 1 )
  {
    r = r8vec_uniform_01_new ( 2, seed );

    x[x_hi-1] = sqrt ( -2.0 * log ( r[0] ) ) * cos ( 2.0 * r8_pi * r[1] );
    y =         sqrt ( -2.0 * log ( r[0] ) ) * sin ( 2.0 * r8_pi * r[1] );

    saved = 1;

    made = made + 2;

    delete [] r;
  }
//
//  If we require an even number of values, that's easy.
//
  else if ( ( x_hi - x_lo + 1 ) % 2 == 0 )
  {
    m = ( x_hi - x_lo + 1 ) / 2;

    r = r8vec_uniform_01_new ( 2*m, seed );

    for ( i = 0; i <= 2*m-2; i = i + 2 )
    {
      x[x_lo+i-1] = sqrt ( -2.0 * log ( r[i] ) ) * cos ( 2.0 * r8_pi * r[i+1] );
      x[x_lo+i  ] = sqrt ( -2.0 * log ( r[i] ) ) * sin ( 2.0 * r8_pi * r[i+1] );
    }
    made = made + x_hi - x_lo + 1;

    delete [] r;
  }
//
//  If we require an odd number of values, we generate an even number,
//  and handle the last pair specially, storing one in X(N), and
//  saving the other for later.
//
  else
  {
    x_hi = x_hi - 1;

    m = ( x_hi - x_lo + 1 ) / 2 + 1;

    r = r8vec_uniform_01_new ( 2*m, seed );

    for ( i = 0; i <= 2*m-4; i = i + 2 )
    {
      x[x_lo+i-1] = sqrt ( -2.0 * log ( r[i] ) ) * cos ( 2.0 * r8_pi * r[i+1] );
      x[x_lo+i  ] = sqrt ( -2.0 * log ( r[i] ) ) * sin ( 2.0 * r8_pi * r[i+1] );
    }

    i = 2*m - 2;

    x[x_lo+i-1] = sqrt ( -2.0 * log ( r[i] ) ) * cos ( 2.0 * r8_pi * r[i+1] );
    y           = sqrt ( -2.0 * log ( r[i] ) ) * sin ( 2.0 * r8_pi * r[i+1] );

    saved = 1;

    made = made + x_hi - x_lo + 2;

    delete [] r;
  }

  return x;
}
//****************************************************************************80

double r8vec_normsq ( int n, double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_NORMSQ returns the squared L2 norm of an R8VEC.
//
//  Discussion:
//
//    An R8VEC is a vector of R8's.
//
//    The squared vector L2 norm is defined as:
//
//      R8VEC_NORMSQ = sum ( 1 <= I <= N ) A(I)^2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 October 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the vector dimension.
//
//    Input, double A[N], the vector.
//
//    Output, double R8VEC_NORMSQ, the squared L2 norm.
//
{
  int i;
  double v;

  v = 0.0;

  for ( i = 0; i < n; i++ )
  {
    v = v + a[i] * a[i];
  }
  return v;
}
//****************************************************************************80

double r8vec_normsq_affine ( int n, double v0[], double v1[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_NORMSQ_AFFINE returns the squared affine L2 norm of an R8VEC.
//
//  Discussion:
//
//    The squared affine vector L2 norm is defined as:
//
//      R8VEC_NORMSQ_AFFINE(V0,V1)
//        = sum ( 1 <= I <= N ) ( V1(I) - V0(I) )^2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 October 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the dimension of the vectors.
//
//    Input, double V0[N], the base vector.
//
//    Input, double V1[N], the vector whose squared affine L2 norm is desired.
//
//    Output, double R8VEC_NORMSQ_AFFINE, the squared affine L2 norm.
//
{
  int i;
  double value;

  value = 0.0;

  for ( i = 0; i < n; i++ )
  {
    value = value + ( v1[i] - v0[i] ) * ( v1[i] - v0[i] );
  }
  return value;
}
//****************************************************************************80

bool r8vec_positive_strict ( int n, double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_POSITIVE_STRICT: all entries of R8VEC are strictly positive.
//
//  Discussion:
//
//    An R8VEC is a vector of R8's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    24 June 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the dimension of the vector.
//
//    Input, double A[N], the vector.
//
//    Output, bool R8VEC_POSITIVE_STRICT, is TRUE if every entry of
//    A is strictly positive.
//
{
  int i;
  bool value;

  for ( i = 0; i < n; i++ )
  {
    if ( a[i] <= 0.0 )
    {
      value = false;
      return value;
    }
  }
  value = true;
  return value;
}
//****************************************************************************80

void r8vec_print ( int n, double a[], string title )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_PRINT prints an R8VEC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of components of the vector.
//
//    Input, int A[N], the vector to be printed.
//
//    Input, string TITLE, a title.
//
{
  int i;

  cout << "\n";
  cout << title << "\n";
  cout << "\n";
  for ( i = 0; i <= n-1; i++ )
  {
    cout << setw(6) << i << ": " << setw(10) << a[i] << "\n";
  }

  return;
}
//****************************************************************************80

void r8vec_print_2d ( double x, double y, string title )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_PRINT_2D prints a 2D vector.
//
//  Discussion:
//
//    A format is used which suggests a coordinate pair:
//
//  Example:
//
//    Center : ( 1.23, 7.45 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X, Y, the coordinates of the vector.
//
//    Input, string TITLE, a title to be printed, or ' '.
//
{
  if ( s_len_trim ( title ) == 0 )
  {
    cout << "(" << setw(10) << x << ", " << setw(10) << y << ")\n";
  }
  else
  {
    cout << title << " : (" << setw(10) << x << ", " << setw(10) << y << ")\n";
  }

  return;
}
//****************************************************************************80

void r8vec_print_3d ( double x, double y, double z, string title )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_PRINT_3D prints a 3D vector.
//
//  Discussion:
//
//    A format is used which suggests a coordinate triple:
//
//  Example:
//
//    Center : ( 1.23, 7.45, -3.12 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X, Y, Z, the coordinates of the vector.
//
//    Input, string TITLE, a title to be printed, or ' '.
//
{
  if ( s_len_trim ( title ) == 0 )
  {
    cout << "( " << setw(10) << x
         << ", " << setw(10) << y
         << ", " << setw(10) << z << ")\n";
  }
  else
  {
    cout << title
         << " : ( " << setw(10) << x
         << ", "    << setw(10) << y
         << ", "    << setw(10) << z << ")\n";
  }

  return;
}
//****************************************************************************80

double r8vec_scalar_triple_product ( double v1[3], double v2[3], double v3[3] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_SCALAR_TRIPLE_PRODUCT finds the scalar triple product in 3D.
//
//  Discussion:
//
//    [A,B,C] = A dot ( B cross C )
//            = B dot ( C cross A )
//            = C dot ( A cross B )
//
//    The volume of a parallelepiped, whose sides are given by
//    vectors A, B, and C, is abs ( A dot ( B cross C ) ).
//
//    Three vectors are coplanar if and only if their scalar triple
//    product vanishes.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Eric Weisstein,
//    "Scalar Triple Product",
//    CRC Concise Encyclopedia of Mathematics, 1999
//
//  Parameters:
//
//    Input, double V1[3], V2[3], V3[3], the vectors.
//
//    Output, double R8VEC_SCALAR_TRIPLE_PRODUCT, the scalar triple product.
//
{
# define DIM_NUM 3

  double *v4;
  double value;

  v4 = r8vec_cross_product_3d ( v2, v3 );

  value = r8vec_dot_product ( DIM_NUM, v1, v4 );

  delete [] v4;

  return value;
# undef DIM_NUM
}
//****************************************************************************80

void r8vec_swap ( int n, double a1[], double a2[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_SWAP swaps the entries of two R8VEC's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the arrays.
//
//    Input/output, double A1[N], A2[N], the vectors to swap.
//
{
  int i;
  double temp;

  for ( i = 0; i < n; i++ )
  {
    temp  = a1[i];
    a1[i] = a2[i];
    a2[i] = temp;
  }

  return;
}
//****************************************************************************80

double *r8vec_uniform_01_new ( int n, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_UNIFORM_01_NEW returns a unit pseudorandom R8VEC.
//
//  Discussion:
//
//    This routine implements the recursion
//
//      seed = 16807 * seed mod ( 2^31 - 1 )
//      unif = seed / ( 2^31 - 1 )
//
//    The integer arithmetic never requires more than 32 bits,
//    including a sign bit.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 August 2004
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paul Bratley, Bennett Fox, Linus Schrage,
//    A Guide to Simulation,
//    Springer Verlag, pages 201-202, 1983.
//
//    Bennett Fox,
//    Algorithm 647:
//    Implementation and Relative Efficiency of Quasirandom
//    Sequence Generators,
//    ACM Transactions on Mathematical Software,
//    Volume 12, Number 4, pages 362-376, 1986.
//
//    Peter Lewis, Allen Goodman, James Miller,
//    A Pseudo-Random Number Generator for the System/360,
//    IBM Systems Journal,
//    Volume 8, pages 136-143, 1969.
//
//  Parameters:
//
//    Input, int N, the number of entries in the vector.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double R8VEC_UNIFORM_01_NEW[N], the vector of pseudorandom values.
//
{
  int i;
  int k;
  double *r;

  r = new double[n];

  for ( i = 0; i < n; i++ )
  {
    k = seed / 127773;

    seed = 16807 * ( seed - k * 127773 ) - k * 2836;

    if ( seed < 0 )
    {
      seed = seed + 2147483647;
    }

    r[i] = ( double ) ( seed ) * 4.656612875E-10;
  }

  return r;
}
//****************************************************************************80

double *r8vec_uniform_ab_new ( int n, double a, double b, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_UNIFORM_AB_NEW returns a scaled pseudorandom R8VEC.
//
//  Discussion:
//
//    Each dimension ranges from A to B.
//
//    This routine implements the recursion
//
//      seed = ( 16807 * seed ) mod ( 2^31 - 1 )
//      u = seed / ( 2^31 - 1 )
//
//    The integer arithmetic never requires more than 32 bits,
//    including a sign bit.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    09 April 2012
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paul Bratley, Bennett Fox, Linus Schrage,
//    A Guide to Simulation,
//    Second Edition,
//    Springer, 1987,
//    ISBN: 0387964673,
//    LC: QA76.9.C65.B73.
//
//    Bennett Fox,
//    Algorithm 647:
//    Implementation and Relative Efficiency of Quasirandom
//    Sequence Generators,
//    ACM Transactions on Mathematical Software,
//    Volume 12, Number 4, December 1986, pages 362-376.
//
//    Pierre L'Ecuyer,
//    Random Number Generation,
//    in Handbook of Simulation,
//    edited by Jerry Banks,
//    Wiley, 1998,
//    ISBN: 0471134031,
//    LC: T57.62.H37.
//
//    Peter Lewis, Allen Goodman, James Miller,
//    A Pseudo-Random Number Generator for the System/360,
//    IBM Systems Journal,
//    Volume 8, Number 2, 1969, pages 136-143.
//
//  Parameters:
//
//    Input, int N, the number of entries in the vector.
//
//    Input, double A, B, the lower and upper limits of the pseudorandom values.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double R8VEC_UNIFORM_AB_NEW[N], the vector of pseudorandom values.
//
{
  int i;
  const int i4_huge = 2147483647;
  int k;
  double *r;

  if ( seed == 0 )
  {
    cerr << "\n";
    cerr << "R8VEC_UNIFORM_AB_NEW - Fatal error!\n";
    cerr << "  Input value of SEED = 0.\n";
    exit ( 1 );
  }

  r = new double[n];

  for ( i = 0; i < n; i++ )
  {
    k = seed / 127773;

    seed = 16807 * ( seed - k * 127773 ) - k * 2836;

    if ( seed < 0 )
    {
      seed = seed + i4_huge;
    }

    r[i] = a + ( b - a ) * ( double ) ( seed ) * 4.656612875E-10;
  }

  return r;
}
//****************************************************************************80

double *r8vec_uniform_unit_new ( int m, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_UNIFORM_UNIT_NEW generates a random direction vector in ND.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 October 2012
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the dimension of the space.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double R8VEC_UNIFORM_UNIT_NEW[M], a random direction vector, with unit norm.
//
{
  double *a;
  int i;
  double norm;
//
//  Take M random samples from the normal distribution.
//
  a = r8vec_normal_01_new ( m, seed );
//
//  Compute the norm.
//
  norm = 0.0;
  for ( i = 0; i < m; i++ )
  {
    norm = norm + a[i] * a[i];
  }
  norm = sqrt ( norm );
//
//  Normalize.
//
  for ( i = 0; i < m; i++ )
  {
    a[i] = a[i] / norm;
  }

  return a;
}
//****************************************************************************80

double r8vec_variance ( int n, double x[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_VARIANCE returns the variance of an R8VEC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 May 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the vector.
//
//    Input, double X[N], the vector whose variance is desired.
//
//    Output, double R8VEC_VARIANCE, the variance of the vector entries.
//
{
  int i;
  double mean;
  double variance;

  mean = r8vec_mean ( n, x );

  variance = 0.0;
  for ( i = 0; i < n; i++ )
  {
    variance = variance + ( x[i] - mean ) * ( x[i] - mean );
  }

  if ( 1 < n )
  {
    variance = variance / ( double ) ( n - 1 );
  }
  else
  {
    variance = 0.0;
  }

  return variance;
}
//****************************************************************************80

void r8vec_zero ( int n, double a1[] )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_ZERO zeroes an R8VEC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the vectors.
//
//    Output, double A1[N], a vector of zeroes.
//
{
  int i;

  for ( i = 0; i < n; i++ )
  {
    a1[i] = 0.0;
  }
  return;
}
//****************************************************************************80

double radec_distance_3d ( double ra1, double dec1, double ra2, double dec2 )

//****************************************************************************80
//
//  Purpose:
//
//    RADEC_DISTANCE_3D - angular distance, astronomical units, sphere in 3D.
//
//  Discussion:
//
//    Right ascension is measured in hours, between 0 and 24, and
//    essentially measures longitude.
//
//    Declination measures the angle from the equator towards the north pole,
//    and ranges from -90 (South Pole) to 90 (North Pole).
//
//    On the unit sphere, the angular separation between two points is
//    equal to their geodesic or great circle distance.  On any other
//    sphere, multiply the angular separation by the radius of the
//    sphere to get the geodesic or great circle distance.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double RA1, DEC1, RA2, DEC2, the right ascension and declination
//    of the two points.
//
//    Output, double RADEC_DISTANCE_3D, the angular separation between the points,
//    in radians.
//
{
# define DIM_NUM 3

  double cos_theta;
  int i;
  double norm_v1;
  double norm_v2;
  double phi1;
  double phi2;
  double theta1;
  double theta2;
  double v1[DIM_NUM];
  double v2[DIM_NUM];
  double value;

  theta1 = degrees_to_radians ( 15.0 * ra1 );
  phi1 = degrees_to_radians ( dec1 );

  v1[0] = cos ( theta1 ) * cos ( phi1 );
  v1[1] = sin ( theta1 ) * cos ( phi1 );
  v1[2] =                  sin ( phi1 );

  norm_v1 = r8vec_norm ( DIM_NUM, v1 );

  theta2 = degrees_to_radians ( 15.0 * ra2 );
  phi2 = degrees_to_radians ( dec2 );

  v2[0] = cos ( theta2 ) * cos ( phi2 );
  v2[1] = sin ( theta2 ) * cos ( phi2 );
  v2[2] =                  sin ( phi2 );

  norm_v2 = r8vec_norm ( DIM_NUM, v2 );

  cos_theta = 0.0;
  for ( i = 0; i < 3; i++ )
  {
    cos_theta = cos_theta + v2[i] * v2[i];
  }
  cos_theta = sqrt ( cos_theta );

  cos_theta = cos_theta / ( norm_v1 * norm_v2 );

  value = r8_acos ( cos_theta );

  return value;
# undef DIM_NUM
}
//****************************************************************************80

double *radec_to_xyz ( double ra, double dec )

//****************************************************************************80
//
//  Purpose:
//
//    RADEC_TO_XYZ converts right ascension/declination to (X,Y,Z) coordinates.
//
//  Discussion:
//
//    Right ascension is measured in hours, between 0 and 24, and
//    essentially measures longitude.
//
//    Declination measures the angle from the equator towards the north pole,
//    and ranges from -90 (South Pole) to 90 (North Pole).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double RA, DEC, the right ascension and declination of a point.
//
//    Output, double RADEC_TO_XYZ[3], the corresponding coordinates of a
//    point with radius 1.
//
{
# define DIM_NUM 3

  double *p;
  double phi;
  double theta;

  theta = degrees_to_radians ( 15.0 * ra );
  phi = degrees_to_radians ( dec );

  p = new double[DIM_NUM];

  p[0] = cos ( theta ) * cos ( phi );
  p[1] = sin ( theta ) * cos ( phi );
  p[2] = sin ( phi );

  return p;
# undef DIM_NUM
}
//****************************************************************************80

double radians_to_degrees ( double angle )

//****************************************************************************80
//
//  Purpose:
//
//    RADIANS_TO_DEGREES converts an angle from radians to degrees.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double ANGLE, an angle in radians.
//
//    Output, double RADIANS_TO_DEGREES, the equivalent angle in degrees.
//
{
  double r8_pi = 3.141592653589793;
  double value;

  value = ( angle / r8_pi ) * 180.0;

  return value;
}
//****************************************************************************80

void radians_to_dms ( double radians, int *degrees, int *minutes, int *seconds )

//****************************************************************************80
//
//  Purpose:
//
//    RADIANS_TO_DMS converts an angle from radians to degrees/minutes/seconds.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double RADIANS, the angle in radians.
//
//    Output, int *DEGREES, *MINUTES, *SECONDS, the equivalent angle in
//    degrees, minutes, and seconds.
//
{
  double angle;
  double r8_pi = 3.141592653589793;

  angle = 180.0 * fabs ( radians ) / r8_pi;

  *degrees = ( int ) angle;
  angle = ( angle - ( ( double ) *degrees ) ) * 60.0;
  *minutes = ( int ) angle;
  angle = ( angle - ( ( double ) *minutes ) ) * 60.0;
  *seconds = ( int ) angle;

  if ( radians < 0.0 )
  {
    *degrees = - *degrees;
    *minutes = - *minutes;
    *seconds = - *seconds;
  }

  return;
}
//****************************************************************************80

unsigned long random_initialize ( unsigned long seed )

//****************************************************************************80
//
//  Purpose:
//
//    RANDOM_INITIALIZE initializes the RANDOM random number generator.
//
//  Discussion:
//
//    If you don't initialize RANDOM, the random number generator,
//    it will behave as though it were seeded with value 1.
//    This routine will either take a user-specified seed, or
//    (if the user passes a 0) make up a "random" one.  In either
//    case, the seed is passed to the appropriate routine
//    to call when setting the seed for RANDOM.  The seed is also
//    returned to the user as the value of the function.
//
//    Depending on your system, the appropriate seed setting routine
//    might be SRAND or SRANDOM.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 May 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, unsigned long SEED, is either 0, which means that the user
//    wants this routine to come up with a seed, or nonzero, in which
//    case the user has supplied the seed.
//
//    Output, unsigned long RANDOM_INITIALIZE, is the value of the seed
//    passed to the seed setting routine, which is either the user's input
//    value, or if that was zero, the value selected by this routine.
//
{
  if ( seed != 0 )
  {
    cout << "\n";
    cout << "RANDOM_INITIALIZE\n";
    cout << "  Initialize RANDOM with user SEED = " << seed << "\n";
  }
  else
  {
    seed = get_seed ( );

    cout << "\n";
    cout << "RANDOM_INITIALIZE\n";
    cout << "  Initialize RANDOM with arbitrary SEED = " << seed << "\n";
  }
//
//  Now set the seed.
//
//  srand ( seed );
//
  srandom ( seed );

  return seed;
}
//****************************************************************************80

void rtp_to_xyz ( double r, double theta, double phi, double xyz[3] )

//****************************************************************************80
//
//  Purpose:
//
//    RTP_TO_XYZ converts (R,Theta,Phi) to (X,Y,Z) coordinates.
//
//  Discussion:
//
//    R measures the distance of the point to the origin.
//
//    Theta measures the "longitude" of the point, between 0 and 2 PI.
//
//    PHI measures the angle from the "north pole", between 0 and PI.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    21 July 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, THETA, PHI, the radius, longitude, and
//    declination of a point.
//
//    Output, double XYZ[3], the corresponding Cartesian coordinates.
//
{
  xyz[0] = r * cos ( theta ) * sin ( phi );
  xyz[1] = r * sin ( theta ) * sin ( phi );
  xyz[2] = r *                 cos ( phi );

  return;
}
//****************************************************************************80

int s_len_trim ( string s )

//****************************************************************************80
//
//  Purpose:
//
//    S_LEN_TRIM returns the length of a string to the last nonblank.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, string S, a string.
//
//    Output, int S_LEN_TRIM, the length of the string to the last nonblank.
//    If S_LEN_TRIM is 0, then the string is entirely blank.
//
{
  int n;

  n = s.length ( );

  while ( 0 < n )
  {
    if ( s[n-1] != ' ' )
    {
      return n;
    }
    n = n - 1;
  }

  return n;
}
//****************************************************************************80

void segment_contains_point_1d ( double p1, double p2, double p3, double *u )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENT_CONTAINS_POINT_1D reports if a line segment contains a point in 1D.
//
//  Discussion:
//
//    A line segment is the finite portion of a line that lies between
//    two points.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1, P2, two points defining a line segment.
//    The line segment has origin at P1, and unit at P2.
//
//    Input, double P3, a point to be tested.
//
//    Output, double *U, the coordinate of P3 in units of (P2-P1).
//    The point P3 is contained in the line segment if 0 <= U <= 1.
//
{
  double unit;

  unit = p2 - p1;

  if ( unit == 0.0 )
  {
    if ( p3 == p1 )
    {
      *u = 0.5;
    }
    else if ( p3 < p1 )
    {
      *u = -HUGE_VAL;
    }
    else if ( p1 < p3 )
    {
      *u = HUGE_VAL;
    }
  }
  else
  {
    *u = ( p3 - p1 ) / unit;
  }

  return;
}
//****************************************************************************80

void segment_contains_point_2d ( double p1[2], double p2[2], double p3[2],
  double u[2] )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENT_CONTAINS_POINT_2D reports if a line segment contains a point in 2D.
//
//  Discussion:
//
//    A line segment is the finite portion of a line that lies between
//    two points.
//
//    In exact arithmetic, point P3 is on the line segment between
//    P1 and P2 if and only if 0 <= U(1) <= 1 and U(2) = 0.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], the endpoints of a line segment.
//
//    Input, double P3[2], a point to be tested.
//
//    Output, double U[2], U[0] is the coordinate of P3 along the axis from
//    with origin at P1 and unit at P2, and U[1] is the magnitude of the off-axis
//    portion of the  vector P3-P1, measured in units of (P2-P1).
//
{
# define DIM_NUM 2

  double t1;
  double t2;
  double unit;

  unit = sqrt ( ( p2[0] - p1[0] ) * ( p2[0] - p1[0] )
              + ( p2[1] - p1[1] ) * ( p2[1] - p1[1] ) );

  if ( unit == 0.0 )
  {
    if ( r8vec_eq ( DIM_NUM, p1, p3 ) )
    {
      u[0] = 0.5;
      u[1] = 0.0;
    }
    else
    {
      u[0] = 0.5;
      u[1] = HUGE_VAL;
    }
  }
  else
  {
    u[0] = ( ( p3[0] - p1[0] ) * ( p2[0] - p1[0] )
           + ( p3[1] - p1[1] ) * ( p2[1] - p1[1] ) )
           / ( unit * unit );

    t1 = ( ( u[0] - 1.0 ) * p1[0] - u[0] * p2[0] + p3[0] );
    t2 = ( ( u[0] - 1.0 ) * p1[1] - u[0] * p2[1] + p3[1] );

    u[1] = sqrt ( t1 * t1 + t2 * t2 ) / unit;
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

void segment_point_coords_2d ( double p1[2], double p2[2], double p[2],
  double *s, double *t )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENT_POINT_COORDS_2D: coordinates of a point on a line segment in 2D.
//
//  Discussion:
//
//    A line segment is the finite portion of a line that lies between
//    two points P1 and P2.
//
//    By the coordinates of a point P with respect to a line segment [P1,P2]
//    we mean numbers S and T such that S gives us the distance from the
//    point P to the nearest point PN on the line (not the line segment!),
//    and T gives us the position of PN relative to P1 and P2.
//
//    If S is zero, then P lies on the line.
//
//    If 0 <= T <= 1, then PN lies on the line segment.
//
//    If both conditions hold, then P lies on the line segment.
//
//    If E is the length of the line segment, then the distance of the
//    point to the line segment is:
//
//      sqrt ( S^2 +  T^2    * E^2 )     if      T <= 0;
//             S                         if 0 <= T <= 1
//      sqrt ( S^2 + (T-1)^2 * E62 )     if 1 <= T
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 August 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], the endpoints of the line segment.
//
//    Input, double P[2], the point to be considered.
//
//    Output, double *S, the distance of P to the nearest point PN
//    on the line through P1 and P2.  (S will always be nonnegative.)
//
//    Output, double *T, the relative position of the point PN
//    to the points P1 and P2.
//
{
# define DIM_NUM 2

  double bot;
  int i;
  double pn[DIM_NUM];
//
//  If the line segment is actually a point, then the answer is easy.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) )
  {
    *t = 0.0;
  }
  else
  {
    bot = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      bot = bot + pow ( p2[i] - p1[i], 2 );
    }

    *t = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      *t = *t + ( p[i] - p1[i] ) * ( p2[i] - p1[i] );
    }
    *t = *t / bot;
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    pn[i] = p1[i] + ( *t ) * ( p2[i] - p1[i] );
  }

  *s = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    *s = *s + pow ( p[i] - pn[i], 2 );
  }
  *s = sqrt ( *s );

  return;
# undef DIM_NUM
}
//****************************************************************************80

void segment_point_coords_3d ( double p1[3], double p2[3], double p[3],
  double *s, double *t )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENT_POINT_COORDS_3D: coordinates of a point on a line segment in 3D.
//
//  Discussion:
//
//    A line segment is the finite portion of a line that lies between
//    two points P1 and P2.
//
//    By the coordinates of a point P with respect to a line segment [P1,P2]
//    we mean numbers S and T such that S gives us the distance from the
//    point P to the nearest point PN on the line (not the line segment!),
//    and T gives us the position of PN relative to P1 and P2.
//
//    If S is zero, then P lies on the line.
//
//    If 0 <= T <= 1, then PN lies on the line segment.
//
//    If both conditions hold, then P lies on the line segment.
//
//    If E is the length of the line segment, then the distance of the
//    point to the line segment is:
//
//      sqrt ( S^2 +  T^2    * E^2 )     if      T <= 0;
//             S                         if 0 <= T <= 1
//      sqrt ( S^2 + (T-1)^2 * E^2 )     if 1 <= T
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 August 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the endpoints of the line segment.
//
//    Input, double P[3], the point to be considered.
//
//    Output, double *S, the distance of P to the nearest point PN
//    on the line through P1 and P2.  (S will always be nonnegative.)
//
//    Output, double *T, the relative position of the point PN
//    to the points P1 and P2.
//
{
# define DIM_NUM 3

  double bot;
  int i;
  double pn[DIM_NUM];
//
//  If the line segment is actually a point, then the answer is easy.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) )
  {
    *t = 0.0;
  }
  else
  {
    bot = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      bot = bot + pow ( p2[i] - p1[i], 2 );
    }

    *t = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      *t = *t + ( p[i] - p1[i] ) * ( p2[i] - p1[i] );
    }
    *t = *t / bot;
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    pn[i] = p1[i] + ( *t ) * ( p2[i] - p1[i] );
  }

  *s = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    *s = *s + pow ( p[i] - pn[i], 2 );
  }
  *s = sqrt ( *s );

  return;
# undef DIM_NUM
}
//****************************************************************************80

double segment_point_dist_2d ( double p1[2], double p2[2], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENT_POINT_DIST_2D: distance ( line segment, point ) in 2D.
//
//  Discussion:
//
//    A line segment is the finite portion of a line that lies between
//    two points.
//
//    The nearest point will satisfy the condition
//
//      PN = (1-T) * P1 + T * P2.
//
//    T will always be between 0 and 1.
//
//    Thanks to Kirill Speransky for pointing out that a previous version
//    of this routine was incorrect, 02 May 2006.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 May 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], the endpoints of the line segment.
//
//    Input, double P[2], the point whose nearest neighbor on the line
//    segment is to be determined.
//
//    Output, double SEGMENT_POINT_DIST_2D, the distance from the point
//    to the line segment.
//
{
# define DIM_NUM 2

  double bot;
  double dist;
  int i;
  double t;
  double pn[DIM_NUM];
//
//  If the line segment is actually a point, then the answer is easy.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) )
  {
    t = 0.0;
  }
  else
  {
    bot = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      bot = bot + pow ( p2[i] - p1[i], 2 );
    }

    t = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      t = t + ( p[i] - p1[i] ) * ( p2[i] - p1[i] );
    }

    t = t / bot;
    t = fmax ( t, 0.0 );
    t = fmin ( t, 1.0 );
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    pn[i] = p1[i] + t * ( p2[i] - p1[i] );
  }

  dist = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    dist = dist + pow ( p[i] - pn[i], 2 );
  }
  dist = sqrt ( dist );

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

double segment_point_dist_3d ( double p1[3], double p2[3], double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENT_POINT_DIST_3D: distance ( line segment, point ) in 3D.
//
//  Discussion:
//
//    A line segment is the finite portion of a line that lies between
//    two points.
//
//    Thanks to Kirill Speransky for pointing out that a previous version
//    of this routine was incorrect, 02 May 2006.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 May 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the endpoints of the line segment.
//
//    Input, double P[3], the point whose nearest neighbor on the line
//    segment is to be determined.
//
//    Output, double SEGMENT_POINT_DIST_3D, the distance from the point
//    to the line segment.
//
{
# define DIM_NUM 3

  double bot;
  double dist;
  int i;
  double t;
  double pn[DIM_NUM];
//
//  If the line segment is actually a point, then the answer is easy.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) )
  {
    t = 0.0;
  }
  else
  {
    bot = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      bot = bot + pow ( p2[i] - p1[i], 2 );
    }

    t = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      t = t + ( p[i] - p1[i] ) * ( p2[i] - p1[i] );
    }

    t = t / bot;
    t = fmax ( t, 0.0 );
    t = fmin ( t, 1.0 );
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    pn[i] = p1[i] + t * ( p2[i] - p1[i] );
  }

  dist = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    dist = dist + pow ( p[i] - pn[i], 2 );
  }
  dist = sqrt ( dist );

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

void segment_point_near_2d ( double p1[2], double p2[2], double p[2],
  double pn[2], double *dist, double *t )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENT_POINT_NEAR_2D finds the point on a line segment nearest a point in 2D.
//
//  Discussion:
//
//    A line segment is the finite portion of a line that lies between
//    two points.
//
//    The nearest point will satisfy the condition:
//
//      PN = (1-T) * P1 + T * P2.
//
//    and T will always be between 0 and 1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], the two endpoints of the line segment.
//
//    Input, double P[2], the point whose nearest neighbor
//    on the line segment is to be determined.
//
//    Output, double PN[2], the point on the line segment which is nearest P.
//
//    Output, double *DIST, the distance from the point to the nearest point
//    on the line segment.
//
//    Output, double *T, the relative position of the point Pn to the
//    points P1 and P2.
//
{
# define DIM_NUM 2

  double bot;
  int i;
//
//  If the line segment is actually a point, then the answer is easy.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) )
  {
    *t = 0.0;
  }
  else
  {
    bot = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      bot = bot + pow ( p2[i] - p1[i], 2 );
    }

    *t = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      *t = *t + ( p[i] - p1[i] ) * ( p2[i] - p1[i] );
    }

    *t = *t / bot;
    *t = fmax ( *t, 0.0 );
    *t = fmin ( *t, 1.0 );
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    pn[i] = p1[i] + *t * ( p2[i] - p1[i] );
  }

  *dist = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    *dist = *dist + pow ( p[i] - pn[i], 2 );
  }
  *dist = sqrt ( *dist );

  return;
# undef DIM_NUM
}
//****************************************************************************80

void segment_point_near_3d ( double p1[3], double p2[3], double p[3],
  double pn[3], double *dist, double *t )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENT_POINT_NEAR_3D finds the point on a line segment nearest a point in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the two endpoints of the line segment.
//
//    Input, double P[3], the point whose nearest neighbor
//    on the line segment is to be determined.
//
//    Output, double PN[3], the point on the line segment which is nearest to P.
//
//    Output, double *DIST, the distance from the point to the nearest point
//    on the line segment.
//
//    Output, double *T, the relative position of the nearest point
//    PN to the defining points P1 and P2.
//
//      PN = (1-T)*P1 + T*P2.
//
//    T will always be between 0 and 1.
//
//
{
# define DIM_NUM 3

  double bot;
  int i;
//
//  If the line segment is actually a point, then the answer is easy.
//
  if ( r8vec_eq ( DIM_NUM, p1, p2 ) )
  {
    *t = 0.0;
  }
  else
  {
    bot = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      bot = bot + pow ( p2[i] - p1[i], 2 );
    }

    *t = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      *t = *t + ( p[i] - p1[i] ) * ( p2[i] - p1[i] );
    }

    *t = *t / bot;
    *t = fmax ( *t, 0.0 );
    *t = fmin ( *t, 1.0 );
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    pn[i] = p1[i] + *t * ( p2[i] - p1[i] );
  }

  *dist = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    *dist = *dist + pow ( p[i] - pn[i], 2 );
  }
  *dist = sqrt ( *dist );

  return;
# undef DIM_NUM
}
//****************************************************************************80

double segments_curvature_2d ( double p1[2], double p2[2], double p3[2] )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENTS_CURVATURE_2D computes the curvature of two line segments in 2D.
//
//  Discussion:
//
//    We assume that the segments [P1,P2] and [P2,P3] are given.
//
//    We compute the circle that passes through P1, P2 and P3.
//
//    The inverse of the radius of this circle is the local "curvature".
//
//    If curvature is 0, the two line segments have the same slope,
//    and the three points are collinear.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 March 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], P3[2], the points.
//
//    Output, double SEGMENTS_CURVATURE_2D, the local curvature.
//
{
# define DIM_NUM 2

  double curvature;
  double pc[DIM_NUM];
  double r;

  circle_exp2imp_2d ( p1, p2, p3, &r, pc );

  if ( 0.0 < r )
  {
    curvature = 1.0 / r;
  }
  else
  {
    curvature = 0.0;
  }

  return curvature;
# undef DIM_NUM
}
//****************************************************************************80

double segments_dist_2d ( double p1[2], double p2[2], double q1[2],
  double q2[2] )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENTS_DIST_2D computes the distance between two line segments in 2D.
//
//  Discussion:
//
//    A line segment is the finite portion of a line that lies between
//    two points.
//
//    If the lines through [P1,P2] and [Q1,Q2] intersect, and both
//    line segments include the point of intersection, then the distance
//    is zero and we are done.
//
//    Therefore, we compute the intersection of the two lines, and
//    find the coordinates of that intersection point on each line.
//    This will tell us if the zero distance case has occurred.
//
//    Otherwise, let PN and QN be points in [P1,P2] and [Q1,Q2] for which
//    the distance is minimal.  If the lines do not intersect, then it
//    cannot be the case that both PN and QN are strictly interior to their
//    line segments, aside from the exceptional singular case when
//    the line segments overlap or are parallel.  Even then, one of PN
//    and QN may be taken to be a segment endpoint.
//
//    Therefore, our second computation finds the minimum of:
//
//      Distance ( P1, [Q1,Q2] );
//      Distance ( P2, [Q1,Q2] );
//      Distance ( Q1, [P1,P2] );
//      Distance ( Q2, [P1,P2] );
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 August 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], the endpoints of the first segment.
//
//    Input, double Q1[2], Q2[2], the endpoints of the second segment.
//
//    Output, double SEGMENTS_DIST_2D, the distance between the line segments.
//
{
# define DIM_NUM 2

  double dist;
  double dist2;
  int ival;
  double r[DIM_NUM];
  double rps;
  double rpt;
  double rqs;
  double rqt;
//
//  Determine whether and where the underlying lines intersect.
//
  lines_exp_int_2d ( p1, p2, q1, q2, &ival, r );
//
//  If there is exactly one intersection point part of both lines,
//  check that it is part of both line segments.
//
  if ( ival == 1 )
  {
    segment_point_coords_2d ( p1, p2, r, &rps, &rpt );
    segment_point_coords_2d ( q1, q2, r, &rqs, &rqt );

    if ( 0.0 <= rpt && rpt <= 1.0 && 0.0 <= rqt && rqt <= 1.0 )
    {
      dist = 0.0;
      return dist;
    }
  }
//
//  If there is no intersection, or the intersection point is
//  not part of both line segments, then an endpoint of one
//  line segment achieves the minimum distance.
//
  dist2 = segment_point_dist_2d ( q1, q2, p1 );
  dist = dist2;
  dist2 = segment_point_dist_2d ( q1, q2, p2 );
  dist = fmin ( dist, dist2 );
  dist2 = segment_point_dist_2d ( p1, p2, q1 );
  dist = fmin ( dist, dist2 );
  dist2 = segment_point_dist_2d ( p1, p2, q2 );
  dist = fmin ( dist, dist2 );

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

double segments_dist_3d ( double p1[3], double p2[3], double q1[3],
  double q2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENTS_DIST_3D computes the distance between two line segments in 3D.
//
//  Discussion:
//
//
//    NOTE: The special cases for identical and parallel lines have not been
//    worked out yet; those cases are exceptional, and so this code
//    is made available in a slightly unfinished form!
//
//
//    A line segment is the finite portion of a line that lies between
//    two points P1 and P2.
//
//    Given two line segments, consider the underlying lines on which
//    they lie.
//
//    A) If the lines are identical, then the distance between the line segments
//    is 0, if the segments overlap, or otherwise is attained by the
//    minimum of the distances between each endpoint and the opposing
//    line segment.
//
//    B) If the lines are parallel, then the distance is either the distance
//    between the lines, if the projection of one line segment onto
//    the other overlaps, or otherwise is attained by the
//    minimum of the distances between each endpoint and the opposing
//    line segment.
//
//    C) If the lines are not identical, and not parallel, then there are
//    unique points PN and QN which are the closest pair of points on the lines.
//    If PN is interior to [P1,P2] and QN is interior to [Q1,Q2],
//    then the distance between the two line segments is the distance
//    between PN and QN.  Otherwise, the nearest distance can be computed
//    by taking the minimum of the distance from each endpoing to the
//    opposing line segment.
//
//    Therefore, our computation first checks whether the lines are
//    identical, parallel, or other, and checks for the special case
//    where the minimum occurs in the interior.
//
//    If that case is ruled out, it computes and returns the minimum of:
//
//      Distance ( P1, [Q1,Q2] );
//      Distance ( P2, [Q1,Q2] );
//      Distance ( Q1, [P1,P2] );
//      Distance ( Q2, [P1,P2] );
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 August 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the endpoints of the first
//    segment.
//
//    Input, double Q1[3], Q2[3], the endpoints of the second
//    segment.
//
//    Output, double SEGMENTS_DIST_3D, the distance between the line segments.
//
{
# define DIM_NUM 3

  double a;
  double b;
  double c;
  double d;
  double det;
  double dist;
  double dist2;
  double e;
  int i;
  double pn[DIM_NUM];
  double qn[DIM_NUM];
  double sn;
  double tn;
  double u[DIM_NUM];
  double v[DIM_NUM];
  double w0[DIM_NUM];
//
//  The lines are identical.
//  THIS CASE NOT SET UP YET
//
// if ( lines_exp_equal_3d ( p1, p2, q1, q2 ) ) then
// end if
//
//  The lines are not identical, but parallel
//  THIS CASE NOT SET UP YET.
//
// if ( lines_exp_parallel_3d ( p1, p2, q1, q2 ) ) then
// end if
//
//  C: The lines are not identical, not parallel.
//

//
//  Let U = (P2-P1) and V = (Q2-Q1) be the direction vectors on
//  the two lines.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    u[i] = p2[i] - p1[i];
  }
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v[i] = q2[i] - q1[i];
  }
//
//  Let SN be the unknown coordinate of the nearest point PN on line 1,
//  so that PN = P(SN) = P1 + SN * (P2-P1).
//
//  Let TN be the unknown coordinate of the nearest point QN on line 2,
//  so that QN = Q(TN) = Q1 + TN * (Q2-Q1).
//
//  Let W0 = (P1-Q1).
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    w0[i] = p1[i] - q1[i];
  }
//
//  The vector direction WC = P(SN) - Q(TC) is unique (among directions)
//  perpendicular to both U and V, so
//
//    U dot WC = 0
//    V dot WC = 0
//
//  or, equivalently:
//
//    U dot ( P1 + SN * (P2 - P1) - Q1 - TN * (Q2 - Q1) ) = 0
//    V dot ( P1 + SN * (P2 - P1) - Q1 - TN * (Q2 - Q1) ) = 0
//
//  or, equivalently:
//
//    (u dot u ) * sn - (u dot v ) tc = -u * w0
//    (v dot u ) * sn - (v dot v ) tc = -v * w0
//
//  or, equivalently:
//
//   ( a  -b ) * ( sn ) = ( -d )
//   ( b  -c )   ( tc )   ( -e )
//
  a = r8vec_dot_product ( DIM_NUM, u, u );
  b = r8vec_dot_product ( DIM_NUM, u, v );
  c = r8vec_dot_product ( DIM_NUM, v, v );
  d = r8vec_dot_product ( DIM_NUM, u, w0 );
  e = r8vec_dot_product ( DIM_NUM, v, w0 );
//
//  Check the determinant.
//
  det = - a * c + b * b;

  if ( det == 0.0 )
  {
    sn = 0.0;
    if ( fabs ( b ) < fabs ( c ) )
    {
      tn = e / c;
    }
    else
    {
      tn = d / b;
    }
  }
  else
  {
    sn = ( c * d - b * e ) / det;
    tn = ( b * d - a * e ) / det;
  }
//
//  Now if both nearest points on the lines
//  also happen to lie inside their line segments,
//  then we have found the nearest points on the line segments.
//
  if ( 0.0 <= sn && sn <= 1.0 && 0.0 <= tn && tn <= 1.0 )
  {
    for ( i = 0; i < DIM_NUM; i++ )
    {
      pn[i] = p1[i] + sn * ( p2[i] - p1[i] );
    }
    for ( i = 0; i < DIM_NUM; i++ )
    {
      qn[i] = q1[i] + tn * ( q2[i] - q1[i] );
    }

    dist = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      dist = dist + pow ( pn[i] - qn[i], 2 );
    }
    dist = sqrt ( dist );

    return dist;
  }
//
//  The nearest point did not occur in the interior.
//  Therefore it must be achieved at an endpoint.
//
  dist2 = segment_point_dist_3d ( q1, q2, p1 );
  dist = dist2;
  dist2 = segment_point_dist_3d ( q1, q2, p2 );
  dist = fmin ( dist, dist2 );
  dist2 = segment_point_dist_3d ( p1, p2, q1 );
  dist = fmin ( dist, dist2 );
  dist2 = segment_point_dist_3d ( p1, p2, q2 );
  dist = fmin ( dist, dist2 );

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

double segments_dist_3d_old ( double p1[3], double p2[3], double p3[3],
  double p4[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENTS_DIST_3D_OLD computes the distance between two line segments in 3D.
//
//  Discussion:
//
//    A line segment is the portion of an infinite line that lies between
//    two given points.  The behavior of the distance function is a bit
//    complicated.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 November 1998
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], the endpoints of the first segment.
//
//    Input, double P3[3], P4[3], the endpoints of the second segment.
//
//    Output, double SEGMENTS_DIST_3D, the distance between the line segments.
//
{
# define DIM_NUM 3

  double d1;
  double d2;
  double dist;
  double dl;
  double dm;
  double dr;
  double pn1[DIM_NUM];
  double pn2[DIM_NUM];
  double pt[DIM_NUM];
  bool result;
  double t1;
  double t2;
  double tl;
  double tm;
  double tmin;
  double tr;
//
//  Find the nearest points on line 2 to the endpoints of line 1.
//
  segment_point_near_3d ( p3, p4, p1, pn1, &d1, &t1 );

  segment_point_near_3d ( p3, p4, p2, pn2, &d2, &t2 );

  if ( t1 == t2 )
  {
    dist = segment_point_dist_3d ( p1, p2, pn1 );
    return dist;
  }
//
//  On line 2, over the interval between the points nearest to line 1,
//  the square of the distance of any point to line 1 is a quadratic function.
//  Evaluate it at three points, and seek its local minimum.
//
  dl = segment_point_dist_3d ( p1, p2, pn1 );

  pt[0] = 0.5 * ( pn1[0] + pn2[0] );
  pt[1] = 0.5 * ( pn1[1] + pn2[1] );
  pt[2] = 0.5 * ( pn1[2] + pn2[2] );

  dm = segment_point_dist_3d ( p1, p2, pt );

  dr = segment_point_dist_3d ( p1, p2, pn2 );

  tl = 0.0;
  tm = 0.5;
  tr = 1.0;

  dl = dl * dl;
  dm = dm * dm;
  dr = dr * dr;

  result = minquad ( tl, dl, tm, dm, tr, dr, &tmin, &dist );

  if ( !result )
  {
    cerr << "\n";
    cerr << "SEGMENTS_DIST_3D - Fatal error!\n";
    cerr << "  MINQUAD returned error condition.\n";
    exit ( 1 );
  }

  dist = sqrt ( dist );

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

double segments_int_1d ( double p1, double p2, double q1, double q2,
  double *r1, double *r2 )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENTS_INT_1D computes the intersection of two line segments in 1D.
//
//  Discussion:
//
//    A line segment is the finite portion of a line that lies between
//    two points.
//
//    In 1D, two line segments "intersect" if they overlap.
//
//    Using a real number DIST to report overlap is preferable to
//    returning a TRUE/FALSE flag, since DIST is better able to
//    handle cases where the segments "almost" interlap.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 July 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1, P2, the endpoints of the first segment.
//
//    Input, double Q1, Q2, the endpoints of the second segment.
//
//    Output, double *R1, *R2, the endpoints of the intersection
//    segment.
//    If DIST < 0, then the interval [R1,R2] is the common intersection
//    of the two segments.
//    If DIST = 0, then R1 = R2 is the single common point of the two segments.
//    If DIST > 0, then (R1,R2) is an open interval separating the two
//    segments, which do not overlap at all.
//
//    Output, double SEGMENTS_INT_1D, the "distance" DIST between the segments.
//    < 0, the segments overlap, and the overlap is DIST units long;
//    = 0, the segments overlap at a single point;
//    > 0, the segments do not overlap.  The distance between the nearest
//    points is DIST units.
//
{
  double dist;

  *r1 = fmax ( fmin ( p1, p2 ),
                 fmin ( q1, q2 ) );

  *r2 = fmin ( fmax ( p1, p2 ),
                 fmax ( q1, q2 ) );

  dist = ( *r1 ) - ( *r2 );

  return dist;
}
//****************************************************************************80

void segments_int_2d ( double p1[2], double p2[2], double p3[2],
  double p4[2], int *flag, double p5[2] )

//****************************************************************************80
//
//  Purpose:
//
//    SEGMENTS_INT_2D computes the intersection of two line segments in 2D.
//
//  Discussion:
//
//    A line segment is the finite portion of a line that lies between
//    two points.
//
//    In 2D, two line segments might not intersect, even though the
//    lines, of which they are portions, intersect.
//
//    Thanks to Siavosh Bahrami for pointing out an error involving incorrect
//    indexing of the U array, 17 August 2005.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], P2[2], the endpoints of the first segment.
//
//    Input, double P3[2], P4[2], the endpoints of the second segment.
//
//    Output, int *FLAG, records the results.
//    0, the line segments do not intersect.
//    1, the line segments intersect.
//
//    Output, double *P5[2].
//    If FLAG = 0, P5 = 0.
//    If FLAG = 1, then P5 is a point of intersection.
//
{
# define DIM_NUM 2

  int ival;
  double tol = 0.001;
  double u[DIM_NUM];
//
//  Find the intersection of the two lines.
//
  lines_exp_int_2d ( p1, p2, p3, p4, &ival, p5 );

  if ( ival == 0 )
  {
    *flag = 0;
    p5[0] = 0.0;
    p5[1] = 0.0;
    return;
  }
//
//  Is the intersection point on the first line segment?
//
  segment_contains_point_2d ( p1, p2, p5, u );

  if ( u[0] < 0.0 || 1.0 < u[0] || tol < u[1] )
  {
    *flag = 0;
    p5[0] = 0.0;
    p5[1] = 0.0;
    return;
  }
//
//  Is the intersection point on the second line segment?
//
  segment_contains_point_2d ( p3, p4, p5, u );

  if ( u[0] < 0.0 || 1.0 < u[0] || tol < u[1] )
  {
    *flag = 0;
    p5[0] = 0.0;
    p5[1] = 0.0;
    return;
  }

  *flag = 1;

  return;
# undef DIM_NUM
}
//****************************************************************************80

double shape_point_dist_2d ( double pc[2], double p1[2], int side_num,
  double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    SHAPE_POINT_DIST_2D: distance ( regular shape, point ) in 2D.
//
//  Discussion:
//
//    The "regular shape" is assumed to be an equilateral and equiangular
//    polygon, such as the standard square, pentagon, hexagon, and so on.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PC[2], the center of the shape.
//
//    Input, double P1[2], the first vertex of the shape.
//
//    Input, int SIDE_NUM, the number of sides.
//
//    Input, double P[2], the point to be checked.
//
//    Output, double SHAPE_POINT_DIST_2D, the distance from the point
//    to the shape.
//
{
# define DIM_NUM 2

  double angle;
  double angle2;
  double dist;
  double pa[DIM_NUM];
  double pb[DIM_NUM];
  double r8_pi = 3.141592653589793;
  double radius;
  double sector_angle;
  int sector_index;
//
//  Determine the angle subtended by a single side.
//
  sector_angle = 360.0 / ( ( double ) side_num );
//
//  How long is the half-diagonal?
//
  radius = sqrt ( pow ( p1[0] - pc[0], 2 ) + pow ( p1[1] - pc[1], 2 ) );
//
//  If the radius is zero, then the shape is a point and the computation is easy.
//
  if ( radius == 0.0 )
  {
    dist = sqrt ( pow ( p[0] - pc[0], 2 ) + pow ( p[1] - pc[1], 2 ) );
    return dist;
  }
//
//  If the test point is at the center, then the computation is easy.
//  The angle subtended by any side is ( 2 * PI / SIDE_NUM ) and the
//  nearest distance is the midpoint of any such side.
//
  if ( sqrt ( pow ( p[0] - pc[0], 2 ) + pow ( p[1] - pc[1], 2 ) ) == 0.0 )
  {
    dist = radius * cos ( r8_pi / ( double ) side_num );
    return dist;
  }
//
//  Determine the angle between the ray to the first corner,
//  and the ray to the test point.
//
  angle = angle_deg_2d ( p1, pc, p );
//
//  Determine the sector of the point.
//
  sector_index = ( int ) ( angle / sector_angle ) + 1;
//
//  Generate the two corner points that terminate the SECTOR-th side.
//
  angle2 = ( ( double ) ( sector_index - 1 ) ) * sector_angle;
  angle2 = degrees_to_radians ( angle2 );

  vector_rotate_base_2d ( p1, pc, angle2, pa );

  angle2 = ( ( double ) sector_index ) * sector_angle;
  angle2 = degrees_to_radians ( angle2 );

  vector_rotate_base_2d ( p1, pc, angle2, pb );
//
//  Determine the distance from the test point to the line segment that
//  is the SECTOR-th side.
//
  dist = segment_point_dist_2d ( pa, pb, p );

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

void shape_point_near_2d ( double pc[2], double p1[2], int side_num,
  double p[2], double pn[2], double *dist )

//****************************************************************************80
//
//  Purpose:
//
//    SHAPE_POINT_NEAR_2D: nearest point ( regular shape, point ) in 2D.
//
//  Discussion:
//
//    The "regular shape" is assumed to be an equilateral and equiangular
//    polygon, such as the standard square, pentagon, hexagon, and so on.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 October 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PC[2], the center of the shape.
//
//    Input, double P1[2], the first vertex of the shape.
//
//    Input, int SIDE_NUM, the number of sides.
//
//    Input, double P[2], the point to be checked.
//
//    Output, double PN[2], the point on the shape that is nearest
//    to the given point.
//
//    Output, double *DIST, the distance between the points.
//
{
# define DIM_NUM 2

  double angle;
  double angle2;
  double r8_pi = 3.141592653589793;
  double radius;
  double sector_angle;
  int sector_index;
  double t;
  double pa[DIM_NUM];
  double pb[DIM_NUM];
  double pd[DIM_NUM];
//
//  Determine the angle subtended by a single side.
//
  sector_angle = 360.0 / ( ( double ) side_num );
//
//  How long is the half-diagonal?
//
  radius = sqrt ( pow ( p1[0] - pc[0], 2 ) + pow ( p1[1] - pc[1], 2 ) );
//
//  If the radius is zero, then the shape is a point and the computation is easy.
//
  if ( radius == 0.0 )
  {
    r8vec_copy ( DIM_NUM, pc, pn );
    *dist = sqrt ( pow ( p[0] - pc[0], 2 ) + pow ( p[1] - pc[1], 2 ) );
    return;
  }
//
//  If the test point is at the center, then the computation is easy.
//  The angle subtended by any side is ( 2 * PI / SIDE_NUM ) and the
//  nearest distance is the midpoint of any such side.
//
  if ( sqrt ( pow ( p[0] - pc[0], 2 ) + pow ( p[1] - pc[1], 2 ) ) == 0.0 )
  {
    angle = r8_pi / ( ( double ) side_num );
    pd[0] =   ( p1[0] - pc[0] ) * cos ( angle )
            + ( p1[1] - pc[1] ) * sin ( angle );
    pd[1] = - ( p1[0] - pc[0] ) * sin ( angle )
            + ( p1[1] - pc[1] ) * cos ( angle );
    pn[0] = pc[0] + pd[0] * cos ( angle );
    pn[1] = pc[1] + pd[1] * cos ( angle );
    *dist = radius * cos ( angle );
    return;
  }
//
//  Determine the angle between the ray to the first corner,
//  and the ray to the test point.
//
  angle = angle_deg_2d ( p1, pc, p );
//
//  Determine the sector of the point.
//
  sector_index = ( ( int ) ( angle / sector_angle ) ) + 1;
//
//  Generate the two corner points that terminate the SECTOR-th side.
//
  angle2 = ( ( double ) ( sector_index - 1 ) ) * sector_angle;
  angle2 = degrees_to_radians ( angle2 );

  vector_rotate_base_2d ( p1, pc, angle2, pa );

  angle2 = ( ( double ) sector_index ) * sector_angle;
  angle2 = degrees_to_radians ( angle2 );

  vector_rotate_base_2d ( p1, pc, angle2, pb );
//
//  Determine the point on the SECTOR-th side of the shape which is
//  nearest.
//
  segment_point_near_2d ( pa, pb, p, pn, dist, &t );

  return;
# undef DIM_NUM
}
//****************************************************************************80

void shape_print_3d ( int point_num, int face_num, int face_order_max,
  double point_coord[], int face_order[], int face_point[] )

//****************************************************************************80
//
//  Purpose:
//
//    shape_print_3d() prints information about a polyhedron in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 January 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int POINT_NUM, the number of points.
//
//    Input, int FACE_NUM, the number of faces.
//
//    Input, int FACE_ORDER_MAX, the number of vertices per face.
//
//    Input, double POINT_COORD[DIM_NUM*POINT_NUM], the point coordinates.
//
//    Input, int FACE_ORDER[FACE_NUM], the number of vertices per face.
//
//    Input, int FACE_POINT[FACE_ORDER_MAX*FACE_NUM]; FACE_POINT(I,J)
//    contains the index of the I-th point in the J-th face.  The
//    points are listed in the counter clockwise direction defined
//    by the outward normal at the face.
//
{
# define DIM_NUM 3

  int i;
  int j;

  cout << "\n";
  cout << "SHAPE_PRINT_3D\n";
  cout << "  Information about a polytope.\n";
  cout << "\n";
  cout << "  The number of vertices is " << point_num << "\n";
  cout << "\n";
  cout << "  Vertices:\n";
  cout << "\n";
  cout << "     Index          X               Y               Z\n";
  cout << "\n";
  for ( j = 0; j < point_num; j++ )
  {
    cout << "  " << setw(8) << j + 1 << "  ";
    for ( i = 0; i < DIM_NUM; i++ )
    {
      cout << setprecision(8) << setw(16) << point_coord[i+j*DIM_NUM];
    }
    cout << "\n";
  }

  cout << "\n";
  cout << "  The number of faces is " << face_num << "\n";
  cout << "  The maximum order of any face is " << face_order_max << "\n";
  cout << "\n";
  cout << "     Index     Order         Indices of Nodes in Face\n";
  for ( j = 1; j <= face_order_max; j++ )
  {
    cout << setw(8) << j;
  }
  cout << "\n";
  cout << "                      ";
  cout << "\n";

  for ( j = 0; j < face_num; j++ )
  {
    cout << "  " << setw(8) << j + 1
         << "  " << setw(8) << face_order[j]
         << "  ";
    for ( i = 0; i < face_order[j]; i++ )
    {
      cout << setw(8) << face_point[i+j*face_order_max];
    }
    cout << "\n";
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

void shape_ray_int_2d ( double pc[2], double p1[2], int side_num, double pa[2],
  double pb[2], double pint[2] )

//****************************************************************************80
//
//  Purpose:
//
//    SHAPE_RAY_INT_2D: intersection ( regular shape, ray ) in 2D.
//
//  Discussion:
//
//    The "regular shape" is assumed to be an equilateral and equiangular
//    polygon, such as the standard square, pentagon, hexagon, and so on.
//
//    The origin of the ray is assumed to be inside the shape.  This
//    guarantees that the ray will intersect the shape in exactly one point.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double PC[2], the center of the shape.
//
//    Input, double P1[2], the first vertex of the shape.
//
//    Input, int SIDE_NUM, the number of sides.
//
//    Input, double PA[2], the origin of the ray.
//
//    Input, double PB[2], a second point on the ray.
//
//    Output, double PI[2], the point on the shape intersected by the ray.
//
{
# define DIM_NUM 2

  double angle2;
  bool inside;
  int ival;
  double pv1[DIM_NUM];
  double pv2[DIM_NUM];
  double radius;
  double sector_angle;
  int sector_index;
//
//  Warning!
//  No check is made to ensure that the ray origin is inside the shape.
//  These calculations are not valid if that is not true!
//
//  Determine the angle subtended by a single side.
//
  sector_angle = 360.0 / ( ( double ) side_num );
//
//  How long is the half-diagonal?
//
  radius = sqrt ( pow ( p1[0] - pc[0], 2 ) + pow ( p1[1] - pc[1], 2 ) );
//
//  If the radius is zero, refuse to continue.
//
  if ( radius == 0.0 )
  {
    cerr << "\n";
    cerr << "SHAPE_RAY_INT_2D - Fatal error!\n";
    cerr << "  The shape has radius zero.\n";
    exit ( 1 );
  }
//
//  Determine which sector side intersects the ray.
//
  pv2[0] = 0.0;
  pv2[1] = 0.0;

  for ( sector_index = 1; sector_index <= side_num; sector_index++ )
  {
//
//  Determine the two vertices that define this sector.
//
    if ( sector_index == 1 )
    {
      angle2 = ( ( double ) sector_index - 1 ) * sector_angle;
      angle2 = degrees_to_radians ( angle2 );

      vector_rotate_base_2d ( p1, pc, angle2, pv1 );
    }
    else
    {
      r8vec_copy ( DIM_NUM, pv2, pv1 );
    }

    angle2 = ( ( double ) sector_index ) * sector_angle;
    angle2 = degrees_to_radians ( angle2 );

    vector_rotate_base_2d ( p1, pc, angle2, pv2 );
//
//  Draw the angle from one vertex to the ray origin to the next vertex,
//  and see if that angle contains the ray.  If so, then the ray
//  must intersect the shape side of that sector.
//
    inside = angle_contains_ray_2d ( pv1, pa, pv2, pb );

    if ( inside )
    {
//
//  Determine the intersection of the lines defined by the ray and the
//  sector side.  (We're already convinced that the ray and sector line
//  segment intersect, so we can use the simpler code that treats them
//  as full lines).
//
      lines_exp_int_2d ( pa, pb, pv1, pv2, &ival, pint );

      return;
    }
  }
//
//  If the calculation fell through the loop, then something's wrong.
//
  cerr << "\n";
  cerr << "SHAPE_RAY_INT_2D - Fatal error!\n";
  cerr <<  "  Cannot find intersection of ray and shape.\n";
  exit ( 1 );
# undef DIM_NUM
}
//****************************************************************************80

void simplex_lattice_layer_point_next ( int n, int c[], int v[], bool *more )

//****************************************************************************80
//
//  Purpose:
//
//    SIMPLEX_LATTICE_LAYER_POINT_NEXT: next simplex lattice layer point.
//
//  Discussion:
//
//    The simplex lattice layer L is bounded by the lines
//
//      0 <= X(1:N),
//      L - 1 < sum X(1:N) / C(1:N)  <= L.
//
//    In particular, layer L = 0 always contains just the origin.
//
//    This function returns, one at a time, the points that lie within
//    a given simplex lattice layer.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    31 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the spatial dimension.
//
//    Input, int C[N+1], coefficients defining the
//    lattice layer in entries 1 to N, and the laver index in C[N].
//    The coefficients should be positive, and C[N] must be nonnegative.
//
//    Input/output, int V[N].  On first call for a given layer,
//    the input value of V is not important.  On a repeated call for the same
//    layer, the input value of V should be the output value from the previous
//    call.  On output, V contains the next lattice layer point.
//
//    Input/output, bool *MORE.  On input, set MORE to FALSE to indicate
//    that this is the first call for a given layer.  Thereafter, the input
//    value should be the output value from the previous call.  On output,
//    MORE is TRUE if the returned value V is a new point.
//    If the output value is FALSE, then no more points were found,
//    and V was reset to 0, and the lattice layer has been exhausted.
//
{
  int c1n;
  int i;
  int j;
  int lhs;
  int rhs1;
  int rhs2;
//
//  Treat layer C[N] = 0 specially.
//
  if ( c[n] == 0 )
  {
    if ( !(*more) )
    {
      for ( j = 0; j < n; j++ )
      {
        v[j] = 0;
      }
      *more = true;
    }
    else
    {
      *more = false;
    }
    return;
  }
//
//  Compute the first point.
//
  if ( !(*more) )
  {
    v[0] = ( c[n] - 1 ) * c[0] + 1;
    for ( j = 1; j < n; j++ )
    {
      v[j] = 0;
    }
    *more = true;
  }
  else
  {
    c1n = i4vec_lcm ( n, c );

    rhs1 = c1n * ( c[n] - 1 );
    rhs2 = c1n *   c[n];
//
//  Try to increment component I.
//
    for ( i = 0; i < n; i++ )
    {
      v[i] = v[i] + 1;

      for ( j = 0; j < i; j++ )
      {
        v[j] = 0;
      }
      if ( 0 < i )
      {
        v[0] = rhs1;
        for ( j = 1; j < n; j++ )
        {
          v[0] = v[0] - ( c1n / c[j] ) * v[j];
        }
        v[0] = ( c[0] * v[0] ) / c1n;
        v[0] = i4_max ( v[0], 0 );
      }
      lhs = 0;
      for ( j = 0; j < n; j++ )
      {
        lhs = lhs + ( c1n / c[j] ) * v[j];
      }
      if ( lhs <= rhs1 )
      {
        v[0] = v[0] + 1;
        lhs = lhs + c1n / c[0];
      }
      if ( lhs <= rhs2 )
      {
        return;
      }
    }
    for ( j = 0; j < n; j++ )
    {
      v[j] = 0;
    }
    *more = false;
  }
  return;
}
//****************************************************************************80

void simplex_lattice_point_next ( int n, int c[], int v[], bool *more )

//****************************************************************************80
//
//  Purpose:
//
//    SIMPLEX_LATTICE_POINT_NEXT returns the next simplex lattice point.
//
//  Discussion:
//
//    The lattice simplex is defined by the vertices:
//
//      (0,0,...,0), (C[N]/C[0],0,...,0), (0,C[N]/C[1],...,0) ...
//      (0,0,...C(N]/C[N-1])
//
//    The lattice simplex is bounded by the lines
//
//      0 <= V[0:N-1],
//      V[0] / C[0] + V[1] / C[1] + ... + V[N-1] / C[N-1] <= C[N]
//
//    Lattice points are listed one at a time, starting at the origin,
//    with V[0] increasing first.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the spatial dimension.
//
//    Input, int C[N+1], coefficients defining the
//    lattice simplex.  These should be positive.
//
//    Input/output, int V[N].  On first call, the input
//    value is not important.  On a repeated call, the input value should
//    be the output value from the previous call.  On output, V contains
//    the next lattice point.
//
//    Input/output, bool *MORE.  On input, set MORE to FALSE to indicate
//    that this is the first call for a given simplex.  Thereafter, the input
//    value should be the output value from the previous call.  On output,
//    MORE is TRUE if not only is the returned value V a lattice point,
//    but the routine can be called again for another lattice point.
//    If the output value is FALSE, then no more lattice points were found,
//    and V was reset to 0, and the routine should not be called further
//    for this simplex.
//
{
  int c1n;
  int i;
  int j;
  int lhs;
  int rhs;
  int term;

  if ( !(*more) )
  {
    i4vec_zero ( n, v );
    *more = true;
  }
  else
  {
    c1n = i4vec_lcm ( n, c );
    rhs = c1n * c[n];

    lhs = 0;
    for ( i = 0; i < n; i++ )
    {
      term = 1;
      for ( j = 0; j < n; j++ )
      {
        if ( i == j )
        {
          term = term * v[j];
        }
        else
        {
          term = term * c[j];
        }
      }
      lhs = lhs + term;
    }

    for ( i = 0; i < n; i++ )
    {
      if ( lhs + c1n / c[i] <= rhs )
      {
        v[i] = v[i] + 1;
        *more = true;
        return;
      }
      lhs = lhs - c1n * v[i] / c[i];
      v[i] = 0;
    }
    *more = false;
  }
  return;
}
//****************************************************************************80

int simplex_unit_lattice_point_nd ( int d, int s )

//****************************************************************************80
//
//  Purpose:
//
//    SIMPLEX_UNIT_LATTICE_POINT_ND: count lattice points.
//
//  Discussion:
//
//    The simplex is assumed to be the unit D-dimensional simplex:
//
//    ( (0,0,...,0), (1,0,...,0), (0,1,...,0), ... (0,,0,...,1) )
//
//    or a copy of this simplex scaled by an integer S:
//
//    ( (0,0,...,0), (S,0,...,0), (0,S,...,0), ... (0,,0,...,S) )
//
//    The routine returns the number of integer lattice points that appear
//    inside the simplex or on its boundary.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Matthias Beck, Sinai Robins,
//    Computing the Continuous Discretely,
//    Springer, 2006,
//    ISBN13: 978-0387291390,
//    LC: QA640.7.B43.
//
//  Parameters:
//
//    Input, int D, the spatial dimension.
//
//    Input, int S, the scale factor.
//
//    Output, int SIMPLEX_UNIT_LATTICE_POINT_ND, the number of lattice points.
//
{
  int i;
  int n;

  n = 1;
  for ( i = 1; i <= d; i++ )
  {
    n = ( n * ( s + i ) ) / i;
  }

  return n;
}
//****************************************************************************80

double simplex_unit_volume_nd ( int dim_num )

//****************************************************************************80
//
//  Purpose:
//
//    SIMPLEX_UNIT_VOLUME_ND computes the volume of the unit simplex in ND.
//
//  Discussion:
//
//    The formula is simple: volume = 1/DIM_NUM!.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Output, double SIMPLEX_UNIT_VOLUME_ND, the volume of the cone.
//
{
  int i;
  double volume;

  volume = 1.0;
  for ( i = 1; i <= dim_num; i++ )
  {
    volume = volume / ( ( double ) i );
  }

  return volume;
}
//****************************************************************************80

double simplex_volume_nd ( int dim_num, double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    SIMPLEX_VOLUME_ND computes the volume of a simplex in ND.
//
//  Discussion:
//
//    The formula is:
//
//      volume = 1/DIM_NUM! * det ( A )
//
//    where A is the DIM_NUM by DIM_NUM matrix obtained by subtracting one
//    vector from all the others.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input, double A[DIM_NUM*(DIM_NUM+1)], the points that define the simplex.
//
//    Output, double SIMPLEX_VOLUME_ND, the volume of the simplex.
//
{
  double *b;
  double det;
  int i;
  int info;
  int j;
  int *pivot;
  double volume;

  b = new double [ dim_num * dim_num ];
  pivot = new int [dim_num];

  for ( j = 0; j < dim_num; j++ )
  {
    for ( i = 0; i < dim_num; i++ )
    {
      b[i+j*dim_num] = a[i+j*dim_num] - a[i+dim_num*dim_num];
    }
  }

  info = dge_fa ( dim_num, b, pivot );

  if ( info != 0 )
  {
    volume = -1.0;
  }
  else
  {
    det = dge_det ( dim_num, b, pivot );

    volume = fabs ( det );
    for ( i = 1; i <= dim_num; i++ )
    {
      volume = volume / ( ( double ) i );
    }
  }

  delete [] b;
  delete [] pivot;

  return volume;
}
//****************************************************************************80

double sin_power_int ( double a, double b, int n )

//****************************************************************************80
//
//  Purpose:
//
//    SIN_POWER_INT evaluates the sine power integral.
//
//  Discussion:
//
//    The function is defined by
//
//      SIN_POWER_INT(A,B,N) = Integral ( A <= T <= B ) ( sin ( t ))^n dt
//
//    The algorithm uses the following fact:
//
//      Integral sin^n ( t ) = (1/n) * (
//        sin^(n-1)(t) * cos(t) + ( n-1 ) * Integral sin^(n-2) ( t ) dt )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters
//
//    Input, double A, B, the limits of integration.
//
//    Input, int N, the power of the sine function.
//
//    Output, double SIN_POWER_INT, the value of the integral.
//
{
  double ca;
  double cb;
  int m;
  int mlo;
  double sa;
  double sb;
  double value;

  if ( n < 0 )
  {
    cerr << "\n";
    cerr << "SIN_POWER_INT - Fatal error!\n";
    cerr << "  Power N < 0.\n";
    value = 0.0;
    exit ( 1 );
  }

  sa = sin ( a );
  sb = sin ( b );
  ca = cos ( a );
  cb = cos ( b );

  if ( ( n % 2 ) == 0 )
  {
    value = b - a;
    mlo = 2;
  }
  else
  {
    value = ca - cb;
    mlo = 3;
  }

  for ( m = mlo; m <= n; m = m + 2 )
  {
    value = ( ( double ) ( m - 1 ) * value
              + pow ( sa, m - 1 ) * ca - pow ( sb, m - 1 ) * cb )
      / ( double ) ( m );
  }

  return value;
}
//****************************************************************************80

void soccer_shape_3d ( int point_num, int face_num, int face_order_max,
  double point_coord[], int face_order[], int face_point[] )

//****************************************************************************80
//
//  Purpose:
//
//    SOCCER_SHAPE_3D describes a truncated icosahedron in 3D.
//
//  Discussion:
//
//    The shape is a truncated icosahedron, which is the design used
//    on a soccer ball.  There are 12 pentagons and 20 hexagons.
//
//    Call SOCCER_SIZE_3D to get the values of POINT_NUM, FACE_NUM, and
//    FACE_ORDER_MAX, so you can allocate space for the arrays.
//
//    For each face, the face list must be of length FACE_ORDER_MAX.
//    In cases where a face is of lower than maximum order (the
//    12 pentagons, in this case), the extra entries are listed as
//    "-1".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    http://mathworld.wolfram.com/TruncatedIcosahedron.html
//
//  Parameters:
//
//    Input, int POINT_NUM, the number of points (60).
//
//    Input, int FACE_NUM, the number of faces (32).
//
//    Input, int FACE_ORDER_MAX, the maximum order of any face (6).
//
//    Output, double POINT_COORD[3*POINT_NUM], the point coordinates.
//
//    Output, int FACE_ORDER[FACE_NUM], the number of vertices per face.
//
//    Output, int FACE_POINT[FACE_ORDER_MAX*FACE_NUM]; FACE_POINT(I,J)
//    contains the index of the I-th point in the J-th face.  The
//    points are listed in the counter clockwise direction defined
//    by the outward normal at the face.
//
{
# define DIM_NUM 3

  static int face_order_save[32] = {
    6, 6, 5, 6, 5, 6, 5, 6, 6, 6,
    5, 6, 5, 6, 5, 6, 6, 6, 5, 6,
    5, 5, 6, 6, 6, 5, 6, 5, 6, 6,
    5, 6 };
  static int face_point_save[6*32] = {
       30, 43, 47, 41, 29, 23,
       30, 23, 12,  9, 15, 27,
       30, 27, 35, 45, 43, -1,
       43, 45, 53, 59, 56, 47,
       23, 29, 21, 11, 12, -1,
       27, 15, 13, 22, 33, 35,
       47, 56, 54, 44, 41, -1,
       45, 35, 33, 42, 51, 53,
       12, 11,  4,  1,  3,  9,
       29, 41, 44, 37, 25, 21,
       15,  9,  3,  6, 13, -1,
       56, 59, 60, 58, 55, 54,
       53, 51, 57, 60, 59, -1,
       11, 21, 25, 19, 10,  4,
       33, 22, 24, 36, 42, -1,
       13,  6,  7, 17, 24, 22,
       54, 55, 48, 39, 37, 44,
       51, 42, 36, 40, 50, 57,
        4, 10,  8,  2,  1, -1,
        3,  1,  2,  5,  7,  6,
       25, 37, 39, 28, 19, -1,
       55, 58, 52, 46, 48, -1,
       60, 57, 50, 49, 52, 58,
       10, 19, 28, 26, 16,  8,
       36, 24, 17, 20, 32, 40,
        7,  5, 14, 20, 17, -1,
       48, 46, 34, 26, 28, 39,
       50, 40, 32, 38, 49, -1,
        8, 16, 18, 14,  5,  2,
       46, 52, 49, 38, 31, 34,
       16, 26, 34, 31, 18, -1,
       32, 20, 14, 18, 31, 38 };
  static double point_coord_save[DIM_NUM*60] = {
       -1.00714,    0.153552,   0.067258,
       -0.960284,   0.0848813, -0.33629,
       -0.95172,   -0.153552,   0.33629,
       -0.860021,   0.529326,   0.150394,
       -0.858,     -0.290893,  -0.470806,
       -0.849436,  -0.529326,   0.201774,
       -0.802576,  -0.597996,  -0.201774,
       -0.7842,     0.418215,  -0.502561,
       -0.749174,  -0.0848813,  0.688458,
       -0.722234,   0.692896,  -0.201774,
       -0.657475,   0.597996,   0.502561,
       -0.602051,   0.290893,   0.771593,
       -0.583675,  -0.692896,   0.470806,
       -0.579632,  -0.333333,  -0.771593,
       -0.52171,   -0.418215,   0.771593,
       -0.505832,   0.375774,  -0.803348,
       -0.489955,  -0.830237,  -0.33629,
       -0.403548,   0.,        -0.937864,
       -0.381901,   0.925138,  -0.201774,
       -0.352168,  -0.666667,  -0.688458,
       -0.317142,   0.830237,   0.502561,
       -0.271054,  -0.925138,   0.33629,
       -0.227464,   0.333333,   0.937864,
       -0.224193,  -0.993808,  -0.067258,
       -0.179355,   0.993808,   0.150394,
       -0.165499,   0.608015,  -0.803348,
       -0.147123,  -0.375774,   0.937864,
       -0.103533,   0.882697,  -0.502561,
       -0.0513806,  0.666667,   0.771593,
        0.0000000,  0.,         1.021,
        0.0000000,  0.,        -1.021,
        0.0513806, -0.666667,  -0.771593,
        0.103533,  -0.882697,   0.502561,
        0.147123,   0.375774,  -0.937864,
        0.165499,  -0.608015,   0.803348,
        0.179355,  -0.993808,  -0.150394,
        0.224193,   0.993808,   0.067258,
        0.227464,  -0.333333,  -0.937864,
        0.271054,   0.925138,  -0.33629,
        0.317142,  -0.830237,  -0.502561,
        0.352168,   0.666667,   0.688458,
        0.381901,  -0.925138,   0.201774,
        0.403548,   0.,         0.937864,
        0.489955,   0.830237,   0.33629,
        0.505832,  -0.375774,   0.803348,
        0.521710,   0.418215,  -0.771593,
        0.579632,   0.333333,   0.771593,
        0.583675,   0.692896,  -0.470806,
        0.602051,  -0.290893,  -0.771593,
        0.657475,  -0.597996,  -0.502561,
        0.722234,  -0.692896,   0.201774,
        0.749174,   0.0848813, -0.688458,
        0.784200,  -0.418215,   0.502561,
        0.802576,   0.597996,   0.201774,
        0.849436,   0.529326,  -0.201774,
        0.858000,   0.290893,   0.470806,
        0.860021,  -0.529326,  -0.150394,
        0.951720,   0.153552,  -0.33629,
        0.960284,  -0.0848813,  0.33629,
        1.007140,  -0.153552,  -0.067258 };

  i4vec_copy ( face_num, face_order_save, face_order );
  i4vec_copy ( face_order_max*face_num, face_point_save, face_point );
  r8vec_copy ( DIM_NUM*point_num, point_coord_save, point_coord );

  return;
# undef DIM_NUM
}
//****************************************************************************80

void soccer_size_3d ( int *point_num, int *edge_num, int *face_num,
  int *face_order_max )

//****************************************************************************80
//
//  Purpose:
//
//    SOCCER_SIZE_3D gives "sizes" for a truncated icosahedron in 3D.
//
//  Discussion:
//
//    The shape is a truncated icosahedron, which is the design used
//    on a soccer ball.  There are 12 pentagons and 20 hexagons.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 July 2007
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    http://polyhedra.wolfram.com/uniform/u25.html
//
//  Parameters:
//
//    Output, int *POINT_NUM, the number of points.
//
//    Output, int *EDGE_NUM, the number of edges.
//
//    Output, int *FACE_NUM, the number of faces.
//
//    Output, int *FACE_ORDER_MAX, the maximum order of any face.
//
{
  *point_num = 60;
  *edge_num = 90;
  *face_num = 32;
  *face_order_max = 6;

  return;
}
//****************************************************************************80

void sort_heap_external ( int n, int *indx, int *i, int *j, int isgn )

//****************************************************************************80
//
//  Purpose:
//
//    SORT_HEAP_EXTERNAL externally sorts a list of items into ascending order.
//
//  Discussion:
//
//    The actual list is not passed to the routine.  Hence it may
//    consist of integers, reals, numbers, names, etc.  The user,
//    after each return from the routine, will be asked to compare or
//    interchange two items.
//
//    The current version of this code mimics the FORTRAN version,
//    so the values of I and J, in particular, are FORTRAN indices.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 February 2004
//
//  Author:
//
//    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
//    C++ version by John Burkardt.
//
//  Parameters:
//
//    Input, int N, the length of the input list.
//
//    Input/output, int *INDX.
//    The user must set INDX to 0 before the first call.
//    On return,
//      if INDX is greater than 0, the user must interchange
//      items I and J and recall the routine.
//      If INDX is less than 0, the user is to compare items I
//      and J and return in ISGN a negative value if I is to
//      precede J, and a positive value otherwise.
//      If INDX is 0, the sorting is done.
//
//    Output, int *I, *J.  On return with INDX positive,
//    elements I and J of the user's list should be
//    interchanged.  On return with INDX negative, elements I
//    and J are to be compared by the user.
//
//    Input, int ISGN. On return with INDX negative, the
//    user should compare elements I and J of the list.  If
//    item I is to precede item J, set ISGN negative,
//    otherwise set ISGN positive.
//
{
  static int i_save = 0;
  static int j_save = 0;
  static int k = 0;
  static int k1 = 0;
  static int n1 = 0;
//
//  INDX = 0: This is the first call.
//
  if ( *indx == 0 )
  {

    i_save = 0;
    j_save = 0;
    k = n / 2;
    k1 = k;
    n1 = n;
  }
//
//  INDX < 0: The user is returning the results of a comparison.
//
  else if ( *indx < 0 )
  {
    if ( *indx == -2 )
    {
      if ( isgn < 0 )
      {
        i_save = i_save + 1;
      }
      j_save = k1;
      k1 = i_save;
      *indx = -1;
      *i = i_save;
      *j = j_save;
      return;
    }

    if ( 0 < isgn )
    {
      *indx = 2;
      *i = i_save;
      *j = j_save;
      return;
    }

    if ( k <= 1 )
    {
      if ( n1 == 1 )
      {
        i_save = 0;
        j_save = 0;
        *indx = 0;
      }
      else
      {
        i_save = n1;
        j_save = 1;
        n1 = n1 - 1;
        *indx = 1;
      }
      *i = i_save;
      *j = j_save;
      return;
    }

    k = k - 1;
    k1 = k;
  }
//
//  0 < INDX: the user was asked to make an interchange.
//
  else if ( *indx == 1 )
  {
    k1 = k;
  }

  for ( ;; )
  {

    i_save = 2 * k1;

    if ( i_save == n1 )
    {
      j_save = k1;
      k1 = i_save;
      *indx = -1;
      *i = i_save;
      *j = j_save;
      return;
    }
    else if ( i_save <= n1 )
    {
      j_save = i_save + 1;
      *indx = -2;
      *i = i_save;
      *j = j_save;
      return;
    }

    if ( k <= 1 )
    {
      break;
    }

    k = k - 1;
    k1 = k;
  }

  if ( n1 == 1 )
  {
    i_save = 0;
    j_save = 0;
    *indx = 0;
    *i = i_save;
    *j = j_save;
  }
  else
  {
    i_save = n1;
    j_save = 1;
    n1 = n1 - 1;
    *indx = 1;
    *i = i_save;
    *j = j_save;
  }

  return;
}
//****************************************************************************80

double sphere_cap_area_2d ( double r, double h )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_CAP_AREA_2D computes the surface area of a spherical cap in 2D.
//
//  Discussion:
//
//    Draw any radius of the sphere and note the point P where the radius
//    intersects the sphere.  Consider the point on the radius line which is
//    H units from P.  Draw the circle that lies in the plane perpendicular to
//    the radius, and which intersects the sphere.  The circle divides the sphere
//    into two pieces, and the corresponding disk divides the solid sphere into
//    two pieces.  The spherical cap is the part of the solid sphere that
//    includes the point P.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double H, the "height" of the spherical cap.
//    H must be between 0 and 2 * R.
//
//    Output, double SPHERE_CAP_AREA_2D, the area of the spherical cap.
//
{
  double area;
  double r8_pi = 3.141592653589793;
  double theta;

  if ( h <= 0.0 )
  {
    area = 0.0;
  }
  else if ( 2.0 * r <= h )
  {
    area = 2.0 * r8_pi * r;
  }
  else
  {
    theta = 2.0 * r8_asin ( sqrt ( r * r - ( r - h ) * ( r - h ) ) / r );
    area = r * theta;
    if ( r <= h )
    {
      area = 2.0 * r8_pi * r - area;
    }
  }

  return area;
}
//****************************************************************************80

double sphere_cap_area_3d ( double r, double h )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_CAP_AREA_3D computes the surface area of a spherical cap in 3D.
//
//  Discussion:
//
//    Draw any radius of the sphere and note the point P where the radius
//    intersects the sphere.  Consider the point on the radius line which is
//    H units from P.  Draw the circle that lies in the plane perpendicular to
//    the radius, and which intersects the sphere.  The circle divides the sphere
//    into two pieces, and the corresponding disk divides the solid sphere into
//    two pieces.  The spherical cap is the part of the solid sphere that
//    includes the point P.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double H, the "height" of the spherical cap.
//    H must be between 0 and 2 * R.
//
//    Output, double SPHERE_CAP_AREA_3D, the area of the spherical cap.
//
{
  double area;
  double r8_pi = 3.141592653589793;

  if ( h <= 0.0 )
  {
    area = 0.0;
  }
  else if ( 2.0 * r <= h )
  {
    area = 4.0 * r8_pi * r * r;
  }
  else
  {
    area = 2.0 * r8_pi * r * h;
  }

  return area;
}
//****************************************************************************80

double sphere_cap_area_nd ( int dim_num, double r, double h )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_CAP_AREA_ND computes the area of a spherical cap in ND.
//
//  Discussion:
//
//    The spherical cap is a portion of the surface of the sphere:
//
//      sum ( X(1:N)^2 ) = R^2
//
//    which is no more than H units from the uppermost point on the sphere.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Thomas Ericson, Victor Zinoviev,
//    Codes on Euclidean Spheres,
//    Elsevier, 2001, pages 439-441.
//    QA166.7 E75
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input, double R, the radius of the sphere.
//
//    Input, double H, the "thickness" of the spherical cap,
//    which is normally between 0 and 2 * R.
//
//    Output, double SPHERE_CAP_AREA_ND, the area of the spherical cap.
//
{
  double area;
  double area2;
  double haver_sine;
  int i;
  double theta;
  double ti;
  double tj;
  double tk;

  if ( h <= 0.0 )
  {
    area = 0.0;
    return area;
  }

  if ( 2.0 * r <= h )
  {
    area = sphere_imp_area_nd ( dim_num, r );
    return area;
  }
//
//  For cases where R < H < 2 * R, work with the complementary region.
//
  haver_sine = sqrt ( ( 2.0 * r - h ) * h );

  theta = r8_asin ( haver_sine / r );

  if ( dim_num < 1 )
  {
    area = -1.0;
  }
  else if ( dim_num == 1 )
  {
    area = 0.0;
  }
  else if ( dim_num == 2 )
  {
    area = 2.0 * theta * r;
  }
  else
  {
    ti = theta;

    tj = ti;
    ti = 1.0 - cos ( theta );

    for ( i = 2; i <= dim_num-2; i++ )
    {
      tk = tj;
      tj = ti;
      ti = ( ( double ) ( i - 1 ) * tk
        - cos ( theta ) * pow ( sin ( theta ), i - 1 ) )
        / ( double ) ( i );
    }

    area = sphere_k ( dim_num-1 ) * ti * pow ( r, dim_num - 1 );
  }
//
//  Adjust for cases where R < H < 2R.
//
  if ( r < h )
  {
    area2 = sphere_imp_area_nd ( dim_num, r );
    area = area2 - area;
  }

  return area;
}
//****************************************************************************80

double sphere_cap_volume_2d ( double r, double h )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_CAP_VOLUME_2D computes the volume of a spherical cap in 2D.
//
//  Discussion:
//
//    Draw any radius R of the circle and denote as P the point where the
//    radius intersects the circle.  Now consider the point Q which lies
//    on the radius and which is H units from P.  The line which is
//    perpendicular to the radius R and passes through Q divides the
//    circle into two pieces.  The piece including the point P is the
//    spherical (circular) cap of height (or thickness) H.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double H, the "height" of the spherical cap.  H must
//    be between 0 and 2 * R.
//
//    Output, double SPHERE_CAP_VOLUME_2D, the volume (area) of the spherical cap.
//
{
  double r8_pi = 3.141592653589793;
  double theta;
  double volume;

  if ( h <= 0.0 )
  {
    volume = 0.0;
  }
  else if ( 2.0 * r <= h )
  {
    volume = r8_pi * r * r;
  }
  else
  {
    theta = 2.0 * r8_asin ( sqrt ( r * r - ( r - h ) * ( r - h ) ) / r );
    volume = r * r * ( theta - sin ( theta ) ) / 2.0;

    if ( r < h )
    {
      volume = r8_pi * r * r - volume;
    }
  }

  return volume;
}
//****************************************************************************80

double sphere_cap_volume_3d ( double r, double h )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_CAP_VOLUME_3D computes the volume of a spherical cap in 3D.
//
//  Discussion:
//
//    Draw any radius of the sphere and note the point P where the radius
//    intersects the sphere.  Consider the point on the radius line which is
//    H units from P.  Draw the circle that lies in the plane perpendicular to
//    the radius, and which intersects the sphere.  The circle divides the sphere
//    into two pieces, and the corresponding disk divides the solid sphere into
//    two pieces.  The spherical cap is the part of the solid sphere that
//    includes the point P.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double H, the "height" of the spherical cap.  H must be between
//    0 and 2 * R.
//
//    Output, double SPHERE_CAP_VOLUME_3D, the volume of the spherical cap.
//
{
  double r8_pi = 3.141592653589793;

  if ( h <= 0.0 )
  {
    return 0.0;
  }
  else if ( 2.0 * r <= h )
  {
    return ( 4.0 / 3.0 ) * r8_pi * r * r * r;
  }
  else
  {
    return ( 1.0 / 3.0 ) * r8_pi * h * h * ( 3.0 * r - h );
  }
}
//****************************************************************************80

double sphere_cap_volume_nd ( int dim_num, double r, double h )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_CAP_VOLUME_ND computes the volume of a spherical cap in ND.
//
//  Discussion:
//
//    The spherical cap is a portion of the surface and interior of the sphere:
//
//      sum ( X(1:N)^2 ) <= R^2
//
//    which is no more than H units from some point P on the sphere.
//
//
//    The algorithm proceeds from the observation that the N-dimensional
//    sphere can be parameterized by a quantity RC that runs along the
//    radius from the center to the point P.  The value of RC at the
//    base of the spherical cap is (R-H) and at P it is R.  We intend to
//    use RC as our integration parameeter.
//
//    The volume of the spherical cap is then the integral, as RC goes
//    from (R-H) to R, of the N-1 dimensional volume of the sphere
//    of radius RS, where RC^2 + RS^2 = R^2.
//
//    The volume of the N-1 dimensional sphere of radius RS is simply
//    some constants times RS^(N-1).
//
//    After factoring out the constant terms, and writing RC = R * cos ( T ),
//    and RS = R * sin ( T ), and letting
//      T_MAX = r8_asin ( sqrt ( ( 2.0 * r - h ) * h / r ) ),
//    the "interesting part" of our integral becomes
//
//      constants * R^N * Integral ( T = 0 to T_MAX ) sin**N ( T ) dT
//
//    The integral of sin^N ( T ) dT can be handled by recursion.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input, double R, the radius of the sphere.
//
//    Input, double H, the "thickness" of the spherical cap,
//    which is normally between 0 and 2 * R.
//
//    Output, double SPHERE_CAP_VOLUME_ND, the volume of the spherical cap.
//
{
  double angle;
  double arg;
  double factor1;
  double factor2;
  double volume;
  double volume2;

  if ( h <= 0.0 )
  {
    volume = 0.0;
    return volume;
  }

  if ( 2.0 * r <= h )
  {
    volume = sphere_imp_volume_nd ( dim_num, r );
    return volume;
  }

  if ( dim_num < 1 )
  {
    volume = -1.0;
  }
  else if ( dim_num == 1 )
  {
    volume = h;
  }
  else
  {
    factor1 = sphere_unit_volume_nd ( dim_num - 1 );

    angle = r8_asin ( sqrt ( ( 2.0 * r - h ) * h / r ) );

    arg = 0.0;
    factor2 = sin_power_int ( arg, angle, dim_num );

    volume = factor1 * factor2 * pow ( r, dim_num );

    if ( r < h )
    {
      volume2 = sphere_imp_volume_nd ( dim_num, r );
      volume = volume2 - volume;
    }
  }

  return volume;
}
//****************************************************************************80

void sphere_dia2imp_3d ( double p1[3], double p2[3], double *r, double pc[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_DIA2IMP_3D converts a diameter to an implicit sphere in 3D.
//
//  Discussion:
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    26 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], are the coordinates
//    of two points which form a diameter of the sphere.
//
//    Output, double *R, the computed radius of the sphere.
//
//    Output, double PC[3], the computed center of the sphere.
//
{
  *r = 0.5 * sqrt ( pow ( p1[0] - p2[0], 2 )
                  + pow ( p1[1] - p2[1], 2 )
                  + pow ( p1[2] - p2[2], 2 ) );

  pc[0] = 0.5 * ( p1[0] + p2[0] );
  pc[1] = 0.5 * ( p1[1] + p2[1] );
  pc[2] = 0.5 * ( p1[2] + p2[2] );

  return;
}
//****************************************************************************80

double sphere_distance_xyz ( double xyz1[3], double xyz2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_DISTANCE_XYZ computes great circle distances on a sphere.
//
//  Discussion:
//
//    XYZ coordinates are used.
//
//    We assume the points XYZ1 and XYZ2 lie on the same sphere.
//
//    This computation is a special form of the Vincenty formula.
//    It should be less sensitive to errors associated with very small
//    or very large angular separations.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 August 2010
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    "Great-circle distance",
//    Wikipedia.
//
//  Parameters:
//
//    Input, double XYZ1[3], the coordinates of the first point.
//
//    Input, double XYZ2[3], the coordinates of the second point.
//
//    Output, double DIST, the great circle distance between the points.
//
{
  double bot;
  double dist;
  double lat1;
  double lat2;
  double lon1;
  double lon2;
  double r;
  double top;

  r = r8vec_norm ( 3, xyz1 );

  lat1 = r8_asin ( xyz1[2] );
  lon1 = r8_atan ( xyz1[1], xyz1[0] );

  lat2 = r8_asin ( xyz2[2] );
  lon2 = r8_atan ( xyz2[1], xyz2[0] );

  top = pow ( cos ( lat2 ) * sin ( lon1 - lon2 ), 2 )
      + pow ( cos ( lat1 ) * sin ( lat2 )
            - sin ( lat1 ) * cos ( lat2 ) * cos ( lon1 - lon2 ), 2 );

  top = sqrt ( top );

  bot = sin ( lat1 ) * sin ( lat2 )
      + cos ( lat1 ) * cos ( lat2 ) * cos ( lon1 - lon2 );

  dist = r * atan2 ( top, bot );

  return dist;
}
//****************************************************************************80

double sphere_distance1 ( double lat1, double lon1, double lat2, double lon2,
  double r )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_DISTANCE1 computes great circle distances on a sphere.
//
//  Discussion:
//
//    This computation is based on the law of cosines for spheres.
//    This formula can suffer from rounding errors when the angular
//    distances are small.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    18 February 2009
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    "Great-circle distance",
//    Wikipedia.
//
//  Parameters:
//
//    Input, double LAT1, LON1, the latitude and longitude of
//    the first point.
//
//    Input, double LAT2, LON2, the latitude and longitude of
//    the second point.
//
//    Input, double R, the radius of the sphere.
//
//    Output, double DIST, the great circle distance between
//    the points, measured in the same units as R.
//
{
  double c;
  double dist;

  c = cos ( lat1 ) * cos ( lat2 ) * cos ( lon1 - lon2 )
    + sin ( lat1 ) * sin ( lat2 );

  dist = r * acos ( c );

  return dist;
}
//****************************************************************************80

double sphere_distance2 ( double lat1, double lon1, double lat2, double lon2,
  double r )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_DISTANCE2 computes great circle distances on a sphere.
//
//  Discussion:
//
//    This computation is written in terms of haversines, and can be more
//    accurate when measuring small angular distances.  It can be somewhat
//    inaccurate when the two points are antipodal.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    18 February 2009
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    "Great-circle distance",
//    Wikipedia.
//
//  Parameters:
//
//    Input, double LAT1, LON1, the latitude and longitude of
//    the first point.
//
//    Input, double LAT2, LON2, the latitude and longitude of
//    the second point.
//
//    Input, double R, the radius of the sphere.
//
//    Output, double DIST, the great circle distance between
//    the points, measured in the same units as R.
//
{
  double dist;
  double s;

  s = pow ( sin ( ( lat1 - lat2 ) / 2.0 ), 2 )
    + cos ( lat1 ) * cos ( lat2 ) * pow ( sin ( ( lon1 - lon2 ) / 2.0 ), 2 );
  s = sqrt ( s );

  dist = 2.0 * r * asin ( s );

  return dist;
}
//****************************************************************************80

double sphere_distance3 ( double lat1, double lon1, double lat2, double lon2,
  double r )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_DISTANCE3 computes great circle distances on a sphere.
//
//  Discussion:
//
//    This computation is a special form of the Vincenty formula.
//    It should be less sensitive to errors associated with very small
//    or very large angular separations.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    18 February 2009
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    "Great-circle distance",
//    Wikipedia.
//
//  Parameters:
//
//    Input, double LAT1, LON1, the latitude and longitude of
//    the first point.
//
//    Input, double LAT2, LON2, the latitude and longitude of
//    the second point.
//
//    Input, double R, the radius of the sphere.
//
//    Output, double DIST, the great circle distance between
//    the points, measured in the same units as R.
//
{
  double bot;
  double dist;
  double top;

  top = pow ( cos ( lat2 ) * sin ( lon1 - lon2 ), 2 )
      + pow ( cos ( lat1 ) * sin ( lat2 )
            - sin ( lat1 ) * cos ( lat2 ) * cos ( lon1 - lon2 ), 2 );

  top = sqrt ( top );

  bot = sin ( lat1 ) * sin ( lat2 )
      + cos ( lat1 ) * cos ( lat2 ) * cos ( lon1 - lon2 );

  dist = r * atan2 ( top, bot );

  return dist;
}
//****************************************************************************80

bool sphere_exp_contains_point_3d ( double p1[3], double p2[3], double p3[3],
  double p4[3], double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_EXP_CONTAINS_POINT_3D determines if an explicit sphere contains a point in 3D.
//
//  Discussion:
//
//    An explicit sphere in 3D is determined by four points,
//    which should be distinct, and not coplanar.
//
//    The computation checks the determinant of:
//
//      x1  y1  z1  x1^2+y1^2+z1^2  1
//      x2  y2  z2  x2^2+y2^2+z2^2  1
//      x3  y3  z3  x3^2+y3^2+z3^2  1
//      x4  y4  z4  x4^2+y4^2+z4^2  1
//      x   y   z   x^2 +y^2 +z^2   1
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], P4[3], the coordinates of four points
//    that lie on a circle.
//
//    Input, double P[3], the coordinates of a point, whose
//    position relative to the sphere is desired.
//
//    Output, bool SPHERE_EXP_CONTAINS_POINT_3D, is TRUE if the point
//    is in the sphere, FALSE otherwise.
//
{
  double a[5*5];
//
//  Set up the matrix.
//
  a[0+0*5] = p1[0];
  a[1+0*5] = p2[0];
  a[2+0*5] = p3[0];
  a[3+0*5] = p4[0];
  a[4+0*5] = p[0];

  a[0+1*5] = p1[1];
  a[1+1*5] = p2[1];
  a[2+1*5] = p3[1];
  a[3+1*5] = p4[1];
  a[4+1*5] = p[1];

  a[0+2*5] = p1[2];
  a[1+2*5] = p2[2];
  a[2+2*5] = p3[2];
  a[3+2*5] = p4[2];
  a[4+2*5] = p[2];

  a[0+3*5] = p1[0] * p1[0] + p1[1] * p1[1] + p1[2] * p1[2];
  a[1+3*5] = p2[0] * p2[0] + p2[1] * p2[1] + p2[2] * p2[2];
  a[2+3*5] = p3[0] * p3[0] + p3[1] * p3[1] + p3[2] * p3[2];
  a[3+3*5] = p4[0] * p4[0] + p4[1] * p4[1] + p4[2] * p4[2];
  a[4+3*5] = p[0]  * p[0]  + p[1]  * p[1]  + p[2]  * p[2];

  a[0+4*5] = 1.0;
  a[1+4*5] = 1.0;
  a[2+4*5] = 1.0;
  a[3+4*5] = 1.0;
  a[4+4*5] = 1.0;

  if ( r8mat_det_5d ( a ) < 0.0 )
  {
    return false;
  }
  else
  {
    return true;
  }
}
//****************************************************************************80

void sphere_exp_point_near_3d ( double p1[3], double p2[3], double p3[3],
  double p4[3], double p[3], double pn[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_EXP_POINT_NEAR_3D finds the nearest point on an explicit sphere to a point in 3D.
//
//  Discussion:
//
//    An explicit sphere in 3D is determined by four points,
//    which should be distinct, and not coplanar.
//
//    If the center of the sphere is PC, and the point is P, then
//    the desired point lies at a positive distance R along the vector
//    P-PC unless P = PC, in which case any
//    point on the sphere is "nearest".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    27 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], P4[3], the coordinates of four points
//    that lie on a sphere.
//
//    Input, double P[3], the coordinates of a point whose nearest point on the
//    sphere is desired.
//
//    Output, double PN[3], the nearest point on the sphere.
//
{
  double norm;
  double r;
  double pc[3];
//
//  Find the center.
//
  sphere_exp2imp_3d ( p1, p2, p3, p4, &r, pc );
//
//  If P = PC, bail out now.
//
  norm = sqrt ( pow ( p[0] - pc[0], 2 )
              + pow ( p[1] - pc[1], 2 )
              + pow ( p[2] - pc[2], 2 ) );

  if ( norm == 0.0 )
  {
    pn[0] = pc[0] + r;
    pn[1] = pc[1];
    pn[2] = pc[2];
    return;
  }
//
//  Compute the nearest point.
//
  pn[0] = pc[0] + r * ( p[0] - pc[0] ) / norm;
  pn[1] = pc[1] + r * ( p[1] - pc[1] ) / norm;
  pn[2] = pc[2] + r * ( p[2] - pc[2] ) / norm;

  return;
}
//****************************************************************************80

void sphere_exp2imp_3d ( double p1[3], double p2[3], double p3[3], double p4[3],
  double *r, double pc[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_EXP2IMP_3D converts a sphere from explicit to implicit form in 3D.
//
//  Discussion:
//
//    An explicit sphere in 3D is determined by four points,
//    which should be distinct, and not coplanar.
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double P1[3], P2[3], P3[3], P4[3], the coordinates of four
//    distinct noncoplanar points on the sphere.
//
//    Output, double *R, PC[3], the radius and coordinates of the
//    center of the sphere.  If the linear system is
//    singular, then R = -1, PC[] = 0.
//
{
# define DIM_NUM 3

  double tet[DIM_NUM*4];

  r8vec_copy ( DIM_NUM, p1, tet+0*3 );
  r8vec_copy ( DIM_NUM, p2, tet+1*3 );
  r8vec_copy ( DIM_NUM, p3, tet+2*3 );
  r8vec_copy ( DIM_NUM, p4, tet+3*3 );

  tetrahedron_circumsphere_3d ( tet, r, pc );

  return;
# undef DIM_NUM
}
//****************************************************************************80

void sphere_exp2imp_nd ( int n, double p[], double &r, double pc[] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_EXP2IMP_ND finds an N-dimensional sphere through N+1 points.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    26 July 2011
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the spatial dimension.
//
//    Input, double P[N*(N+1)], the points.
//
//    Output, double &R, the radius of the sphere.
//
//    Output, double PC[N], the center of the sphere.
//
{
  double *a;
  int i;
  int info;
  int j;
  double t;
//
//  Set up the linear system.
//
  a = new double[n*(n+1)];

  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < n; i++ )
    {
      a[i+j*n] = p[j+(i+1)*n];
    }
  }

  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < n; i++ )
    {
      a[i+j*n] = a[i+j*n] - p[j+0*n];
    }
  }

  for ( i = 0; i < n; i++ )
  {
    t = 0.0;
    for ( j = 0; j < n; j++ )
    {
      t = t + a[i+j*n] * a[i+j*n];
    }
    a[i+n*n] = t;
  }
//
//  Solve the linear system.
//
  info = r8mat_solve ( n, 1, a );
//
//  If the system was singular, return a consolation prize.
//
  if ( info != 0 )
  {
    r = -1.0;
    for ( i = 0; i < n; i++ )
    {
      pc[i] = 0.0;
    }
    delete [] a;

    return;
  }
//
//  Compute the radius and center.
//
  r = 0.0;
  for ( i = 0; i < n; i++ )
  {
    r = r + a[i+n*n] * a[i+n*n];
  }
  r = 0.5 * sqrt ( r );

  for ( i = 0; i < n; i++ )
  {
    pc[i] = p[i+0*n] + 0.5 * a[i+n*n];
  }

  delete [] a;

  return;
}
//****************************************************************************80

double sphere_imp_area_3d ( double r )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_AREA_3D computes the surface area of an implicit sphere in 3D.
//
//  Discussion:
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Output, double SPHERE_IMP_AREA_3D, the area of the sphere.
//
{
  double area;
  double r8_pi = 3.141592653589793;

  area = 4.0 * r8_pi * r * r;

  return area;
}
//****************************************************************************80

double sphere_imp_area_nd ( int dim_num, double r )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_AREA_ND computes the surface area of an implicit sphere in ND.
//
//  Discussion:
//
//    DIM_NUM   Area
//
//    2      2       * PI   * R
//    3      4       * PI   * R^2
//    4      2       * PI^2 * R^3
//    5      (8/3)   * PI^2 * R^4
//    6                PI^3 * R^5
//    7      (16/15) * PI^3 * R^6
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input, double R, the radius of the sphere.
//
//    Output, double SPHERE_IMP_AREA_ND, the area of the sphere.
//
{
  double area;

  area = pow ( r, dim_num-1 ) * sphere_unit_area_nd ( dim_num );

  return area;
}
//****************************************************************************80

bool sphere_imp_contains_point_3d ( double r, double pc[3], double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_CONTAINS_POINT_3D determines if an implicit sphere contains a point in 3D.
//
//  Discussion:
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double PC[3], the coordinates of the center of the sphere.
//
//    Input, double P[3], the point to be checked.
//
//    Output, bool SPHERE_IMP_CONTAINS_POINT_3D, is TRUE if the point is inside or
//    on the sphere, FALSE otherwise.
//
{
  if ( pow ( p[0] - pc[0], 2 )
     + pow ( p[1] - pc[1], 2 )
     + pow ( p[2] - pc[2], 2 ) <= r * r )
  {
    return true;
  }
  else
  {
    return false;
  }
}
//****************************************************************************80

void sphere_imp_grid_icos_size ( int factor, int *node_num, int *edge_num,
  int *triangle_num )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_GRID_ICOS_SIZE sizes an icosahedral grid on a sphere.
//
//  Discussion:
//
//    With FACTOR = 1, the grid has 20 triangular faces, 30 edges, and 12 nodes.
//
//    With FACTOR = 2, each triangle of the icosahedron is subdivided into
//    2x2 subtriangles, resulting in 80 faces, 120 edges, and
//    42 = 12 + 20 * 3 * (1)/2 + 20 * 0 ) nodes.
//
//    With FACTOR = 3, each triangle of the icosahedron is subdivided into
//    3x3 subtriangles, resulting in 180 faces, 270 edges, and
//    72 ( = 12 + 20 * 3 * (2)/2 + 20 * 1 ) nodes.
//
//    In general, each triangle is subdivided into FACTOR*FACTOR subtriangles,
//    resulting in 20 * FACTOR * FACTOR faces, 30 * FACTOR*FACTOR edges, and
//      12
//    + 20 * 3          * (FACTOR-1) / 2
//    + 20 * (FACTOR-2) * (FACTOR-1) / 2 nodes.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 July 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int FACTOR, the subdivision factor, which must
//    be at least 1.
//
//    Output, int *NODE_NUM, the number of nodes.
//
//    Output, int *EDGE_NUM, the number of edges.
//
//    Output, int *TRIANGLE_NUM, the number of triangles.
//
{
  *node_num = 12
            + 10 * 3              * ( factor - 1 )
            + 10 * ( factor - 2 ) * ( factor - 1 );

  *edge_num = 30 * factor * factor;

  *triangle_num = 20 * factor * factor;

  return;
}
//****************************************************************************80

void sphere_imp_gridfaces_3d ( int maxtri, int nlat, int nlong, int *ntri,
  int tri[] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_GRIDFACES_3D produces a grid of triangles on an implicit sphere in 3D.
//
//  Discussion:
//
//    The point numbering system is the same used in SPHERE_IMP_GRIDPOINTS_3D,
//    and that routine may be used to compute the coordinates of the points.
//
//    The two dimensional array TRI[3,MAXTRI] is stored by columns.
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    14 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int MAXTRI, the maximum number of triangles.
//
//    Input, int NLAT, NLONG, the number of latitude and longitude
//    lines to draw.  The latitudes do not include the North and South
//    poles, which will be included automatically, so NLAT = 5, for instance,
//    will result in points along 7 lines of latitude.
//
//    Output, int *NTRI, the number of triangles.
//
//    Output, int TRI[3*MAXTRI], contains NTRI triples of point indices for
//    the triangles that make up the grid.
//
{
  int i;
  int j;
  int n;
  int n_max;
  int n_min;
  int ne;
  int nw;
  int s;
  int s_max;
  int s_min;
  int se;
  int sw;

  *ntri = 0;
//
//  The first row.
//
  n = 1;

  sw = 2;
  se = sw + 1;

  s_min = 2;
  s_max = nlong + 1;

  for ( j = 0; j <= nlong-1; j++ )
  {
    if ( *ntri < maxtri )
    {
      tri[0+(*ntri)*3] = sw;
      tri[1+(*ntri)*3] = se;
      tri[2+(*ntri)*3] = n;
      *ntri = *ntri + 1;
    }

    sw = se;

    if ( se == s_max )
    {
      se = s_min;
    }
    else
    {
      se = se + 1;
    }

  }
//
//  The intermediate rows.
//
  for ( i = 1; i <= nlat; i++ )
  {
    n_max = s_max;
    n_min = s_min;

    s_max = s_max + nlong;
    s_min = s_min + nlong;

    nw = n_min;
    ne = nw + 1;
    sw = s_min;
    se = sw + 1;

    for ( j = 0; j <= nlong - 1; j++ )
    {
      if ( *ntri < maxtri )
      {
        tri[0+(*ntri)*3] = sw;
        tri[1+(*ntri)*3] = se;
        tri[2+(*ntri)*3] = nw;
        *ntri = *ntri + 1;
      }

      if ( *ntri < maxtri )
      {
        tri[0+(*ntri)*3] = ne;
        tri[1+(*ntri)*3] = nw;
        tri[2+(*ntri)*3] = se;
        *ntri = *ntri + 1;
      }

      sw = se;
      nw = ne;

      if ( se == s_max )
      {
        se = s_min;
      }
      else
      {
        se = se + 1;
      }

      if ( ne == n_max )
      {
        ne = n_min;
      }
      else
      {
        ne = ne + 1;
      }

    }

  }
//
//  The last row.
//
  n_max = s_max;
  n_min = s_min;

  s = n_max + 1;

  nw = n_min;
  ne = nw + 1;

  for ( j = 0; j <= nlong-1; j++ )
  {
    if ( *ntri < maxtri )
    {
      tri[0+(*ntri)*3] = ne;
      tri[1+(*ntri)*3] = nw;
      tri[2+(*ntri)*3] = s;
      *ntri = *ntri + 1;
    }

    nw = ne;

    if ( ne == n_max )
    {
      ne = n_min;
    }
    else
    {
      ne = ne + 1;
    }

  }

  return;
}
//****************************************************************************80

int sphere_imp_line_project_3d ( double r, double pc[3], int n, double p[],
  int maxpnt2, double pp[], double thetamin, double thetamax )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_LINE_PROJECT_3D projects a line onto an implicit sphere in 3D.
//
//  Discussion:
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//    The line to be projected is specified as a sequence of points.
//    If two successive points subtend a small angle, then the second
//    point is essentially dropped.  If two successive points subtend
//    a large angle, then intermediate points are inserted, so that
//    the projected line stays closer to the sphere.
//
//    Note that if any P coincides with the center of the sphere, then
//    its projection is mathematically undefined.  P will
//    be returned as PC.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.  If R is
//    zero, PP will be returned as PC, and if R is
//    negative, points will end up diametrically opposite from where
//    you would expect them for a positive R.
//
//    Input, double PC[3], the coordinates of the center of the sphere.
//
//    Input, int N, the number of points on the line that is
//    to be projected.
//
//    Input, double P[3*N], the coordinates of the points
//    on the line that is to be projected.
//
//    Input, int MAXPNT2, the maximum number of points on the projected
//    line.  Even if the routine thinks that more points are needed,
//    no more than MAXPNT2 will be generated.
//
//    Output, double PP[3*N2], the coordinates of the
//    points representing the projected line.  The value N2 is returned
//    as the function value of this routine.  These points lie on the
//    sphere.  Successive points are separated by at least THETAMIN
//    radians, and by no more than THETAMAX radians.
//
//    Input, double THETAMIN, THETAMAX, the minimum and maximum angular
//    projections allowed between successive projected points.
//    If two successive points on the original line have projections
//    separated by more than THETAMAX radians, then intermediate points
//    will be inserted, in an attempt to keep the line closer to the
//    sphere.  If two successive points are separated by less than
//    THETAMIN radians, then the second point is dropped, and the
//    line from the first point to the next point is considered.
//
//    Output, int SPHERE_IMP_LINE_PROJECT_3D, the number of points on
//    the projected line.  This value can be zero, if the line has an
//    angular projection of less than THETAMIN radians.
//
{
# define DIM_NUM 3

  double alpha;
  double ang3d;
  double dot;
  int i;
  int j;
  int nfill;
  int n2;
  double tnorm;
  double p1[DIM_NUM];
  double p2[DIM_NUM];
  double pi[DIM_NUM];
//
//  Check the input.
//
  if ( r == 0.0 )
  {
    n2 = 0;
    return n2;
  }
  r8vec_copy ( DIM_NUM, pc, p1 );
  r8vec_copy ( DIM_NUM, pc, p2 );

  n2 = 0;

  for ( i = 0; i < n; i++ )
  {
    if ( r8vec_eq ( DIM_NUM, p, pc ) )
    {
    }
    else
    {
      r8vec_copy ( DIM_NUM, p2, p1 );

      alpha = sqrt ( pow ( p[0+i*3] - pc[0], 2 )
                   + pow ( p[1+i*3] - pc[1], 2 )
                   + pow ( p[2+i*3] - pc[2], 2 ) );

      p2[0] = pc[0] + r * ( p[0+i*3] - pc[0] ) / alpha;
      p2[1] = pc[1] + r * ( p[1+i*3] - pc[1] ) / alpha;
      p2[2] = pc[2] + r * ( p[2+i*3] - pc[2] ) / alpha;
//
//  If we haven't gotten any points yet, take this point as our start.
//
      if ( n2 == 0 )
      {
        pp[0+n2*3] = p2[0];
        pp[1+n2*3] = p2[1];
        pp[2+n2*3] = p2[2];
        n2 = n2 + 1;
      }
//
//  Compute the angular projection of P1 to P2.
//
      else if ( 1 <= n2 )
      {
        dot = ( p1[0] - pc[0] ) * ( p2[0] - pc[0] )
            + ( p1[1] - pc[1] ) * ( p2[1] - pc[1] )
            + ( p1[2] - pc[2] ) * ( p2[2] - pc[2] );
        ang3d = r8_acos (  dot / ( r * r ) );
//
//  If the angle is at least THETAMIN, (or it's the last point),
//  then we will draw a line segment.
//
        if ( thetamin < fabs ( ang3d ) || i == n )
        {
//
//  Now we check to see if the line segment is too long.
//
          if ( thetamax < fabs ( ang3d ) )
          {
            nfill = ( int ) ( fabs ( ang3d ) / thetamax );

            for ( j = 1; j < nfill; j++ )
            {
              pi[0] = ( ( double ) ( nfill - j ) * ( p1[0] - pc[0] )
                      + ( double ) (         j ) * ( p2[0] - pc[0] ) );
              pi[1] = ( ( double ) ( nfill - j ) * ( p1[1] - pc[1] )
                      + ( double ) (         j ) * ( p2[1] - pc[1] ) );
              pi[2] = ( ( double ) ( nfill - j ) * ( p1[2] - pc[2] )
                      + ( double ) (         j ) * ( p2[2] - pc[2] ) );

              tnorm = r8vec_norm ( DIM_NUM, pi );

              if ( tnorm != 0.0 )
              {
                pi[0] = pc[0] + r * pi[0] / tnorm;
                pi[1] = pc[1] + r * pi[1] / tnorm;
                pi[2] = pc[2] + r * pi[2] / tnorm;
                pp[0+n2*3] = pi[0];
                pp[1+n2*3] = pi[1];
                pp[2+n2*3] = pi[2];
                n2 = n2 + 1;
              }
            }
          }
//
//  Now tack on the projection of point 2.
//
          pp[0+n2*3] = p2[0];
          pp[1+n2*3] = p2[1];
          pp[2+n2*3] = p2[2];
          n2 = n2 + 1;
        }
      }
    }
  }
  return n2;
# undef DIM_NUM
}
//****************************************************************************80

void sphere_imp_local2xyz_3d ( double r, double pc[3], double theta, double phi,
  double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_LOCAL2XYZ_3D converts local to XYZ coordinates on an implicit sphere in 3D.
//
//  Discussion:
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//    The "local" spherical coordinates of a point are two angles, THETA and PHI.
//    PHI measures the angle that the vector from the origin to the point
//    makes with the positive Z axis.  THETA measures the angle that the
//    projection of the vector onto the XY plane makes with the positive X axis.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double PC[3], the coordinates of the center of the sphere.
//
//    Input, double THETA, PHI, the local (THETA,PHI) spherical coordinates
//    of a point on the sphere.  THETA and PHI are angles, measure in
//    radians.  Usually, 0 <= THETA < 2 * PI, and 0 <= PHI <= PI.
//
//    Output, double P[3], the XYZ coordinates of the point.
//
{
  p[0] = pc[0] + r * sin ( phi ) * cos ( theta );
  p[1] = pc[1] + r * sin ( phi ) * sin ( theta );
  p[2] = pc[2] + r * cos ( phi );

  return;
}
//****************************************************************************80

void sphere_imp_point_near_3d ( double r, double pc[3], double p[3],
  double pn[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_POINT_NEAR_3D finds the nearest point on an implicit sphere to a point in 3D.
//
//  Discussion:
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//    If the center of the sphere is PC, and the point is P, then
//    the desired point lies at a positive distance R along the vector
//    P-PC unless P = PC, in which case any point
//    on the sphere is "nearest".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double PC[3], the coordinates of the center of the sphere.
//
//    Input, double P[3], the coordinates of a point whose
//    nearest point on the sphere is desired.
//
//    Output, double PN[3], the nearest point on the sphere.
//
{
  double norm;
//
//  If P = PC, bail out now.
//
  norm = sqrt ( pow ( p[0] - pc[0], 2 )
              + pow ( p[1] - pc[1], 2 )
              + pow ( p[2] - pc[2], 2 ) );

  if ( norm == 0.0 )
  {
    pn[0] = pc[0] + r;
    pn[1] = pc[1];
    pn[2] = pc[2];
  }
//
//  Compute the nearest point.
//
  else
  {
    pn[0] = pc[0] + r * ( p[0] - pc[0] ) / norm;
    pn[1] = pc[1] + r * ( p[1] - pc[1] ) / norm;
    pn[2] = pc[2] + r * ( p[2] - pc[2] ) / norm;
  }

  return;
}
//****************************************************************************80

void sphere_imp_point_project_3d ( double r, double pc[3], double p[3],
  double pp[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_POINT_PROJECT_3D projects a point onto an implicit sphere, in 3D.
//
//  Discussion:
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double PC[3], the coordinates of the center of the sphere.
//
//    Input, double P[3], the coordinates of a point.
//
//    Output, double PP[3], the coordinates of the point as projected
//    onto the sphere from the center.
//
{
# define DIM_NUM 3

  double norm;

  if ( r == 0.0 )
  {
    r8vec_copy ( DIM_NUM, pc, pp );
  }
  else if ( r8vec_eq ( DIM_NUM, p, pc ) )
  {
    pp[0] = pc[0];
    pp[1] = pc[1];
    pp[2] = pc[2] + r;
  }
  else
  {
    norm = sqrt ( pow ( p[0] - pc[0], 2 )
                + pow ( p[1] - pc[1], 2 )
                + pow ( p[2] - pc[2], 2 ) );

    pp[0] = pc[0] + r * ( p[0] - pc[0] ) / norm;
    pp[1] = pc[1] + r * ( p[1] - pc[1] ) / norm;
    pp[2] = pc[2] + r * ( p[2] - pc[2] ) / norm;
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

double sphere_imp_volume_3d ( double r )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_VOLUME_3D computes the volume of an implicit sphere in 3D.
//
//  Discussion:
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Output, double SPHERE_IMP_VOLUME_3D, the volume of the sphere.
//
{
  double r8_pi = 3.141592653589793;
  double volume;

  volume = 4.0 * r8_pi * r * r * r / 3.0;

  return volume;
}
//****************************************************************************80

double sphere_imp_volume_nd ( int dim_num, double r )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_VOLUME_ND computes the volume of an implicit sphere in ND.
//
//  Discussion:
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//    DIM_NUM  Volume
//
//    2             PI   * R^2
//    3  (4/3)    * PI   * R^3
//    4  (1/2)    * PI^2 * R^4
//    5  (8/15)   * PI^2 * R^5
//    6  (1/6)    * PI^3 * R^6
//    7  (16/105) * PI^3 * R^7
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input, double R, the radius of the sphere.
//
//    Output, double SPHERE_IMP_VOLUME_ND, the volume of the sphere.
//
{
  double value;

  value = pow ( r, dim_num ) * sphere_unit_volume_nd ( dim_num );

  return value;
}
//****************************************************************************80

double sphere_imp_zone_area_3d ( double r, double h1, double h2 )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_ZONE_AREA_3D computes the surface area of a spherical zone in 3D.
//
//  Discussion:
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//    Draw any radius of the sphere and note the point P where the radius
//    intersects the sphere.  Now choose two points on the radius line, a
//    distance H1 and H2 from the point P.  Consider all the points on or within
//    the sphere whose projection onto the radius lies between these two points.
//    These points constitute the spherical zone, which can also be considered
//    the difference of two spherical caps.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double H1, H2, the distances that define the thickness of the zone.
//    H1 and H2 must be between 0 and 2 * R.
//
//    Output, double SPHERE_IMP_ZONE_AREA_3D, the area of the spherical zone.
//
{
  double h;
  double r8_pi = 3.141592653589793;

  h = fabs ( h1 - h2 );

  if ( h <= 0.0 )
  {
    return 0.0;
  }
  else if ( 2.0 * r <= h )
  {
    return 4.0 * r8_pi * r * r;
  }
  else
  {
    return 2.0 * r8_pi * r * h;
  }
}
//****************************************************************************80

double sphere_imp_zone_volume_3d ( double r, double h1, double h2 )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP_ZONE_VOLUME_3D computes the volume of a spherical zone in 3D.
//
//  Discussion:
//
//    The implicit form of a sphere in 3D is:
//
//        pow ( P[0] - PC[0], 2 )
//      + pow ( P[1] - PC[1], 2 )
//      + pow ( P[2] - PC[2], 2 ) = pow ( R, 2 )
//
//    Draw any radius of the sphere and note the point P where the radius
//    intersects the sphere.  Now choose two points on the radius line, a
//    distance H1 and H2 from the point P.  Consider all the points on or within
//    the sphere whose projection onto the radius lies between these two points.
//    These points constitute the spherical zone, which can also be considered
//    the difference of two spherical caps.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double H1, H2, the distances that define the thickness of the zone.
//    H1 and H2 must be between 0 and 2 * R.
//
//    Output, double SPHERE_IMP_ZONE_VOLUME_3D, the volume of the spherical zone
//
{
  double h11;
  double h22;
  double r8_pi = 3.141592653589793;

  h11 = fmin ( h1, h2 );
  h11 = fmax ( h11, 0.0 );

  if ( 2.0 * r <= h11 )
  {
    return 0.0;
  }

  h22 = fmax ( h1, h2 );
  h22 = fmin ( h22, 2.0 * r );

  if ( h22 <= 0.0 )
  {
    return 0.0;
  }

  return ( 1.0 / 3.0 ) * r8_pi * (
      h22 * h22 * ( 3.0 * r - h22 )
    - h11 * h11 * ( 3.0 * r - h11 ) );
}
//****************************************************************************80

void sphere_imp2exp_3d ( double r, double pc[3], double p1[3], double p2[3],
  double p3[3], double p4[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_IMP2EXP_3D converts a sphere from implicit to explicit form in 3D.
//
//  Discussion:
//
//    An implicit sphere in 3D satisfies the equation:
//
//      sum ( ( P(1:DIM_NUM) - PC(1:DIM_NUM) )^2 ) = R^2
//
//    An explicit sphere in 3D is determined by four points,
//    which should be distinct, and not coplanar.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    21 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double R, PC[3], the radius and center of the sphere.
//
//    Output, double P1[3], P2[3], P3[3], P4[3],
//    four distinct noncoplanar points on the sphere.
//
{
# define DIM_NUM 3

  double phi;
  double r8_pi = 3.141592653589793;
  double theta;

  theta = 0.0;
  phi = 0.0;

  p1[0] = pc[0] + r * cos ( theta ) * sin ( phi );
  p1[1] = pc[1] + r * sin ( theta ) * sin ( phi );
  p1[2] = pc[2] + r                 * cos ( phi );

  theta = 0.0;
  phi = 2.0 * r8_pi / 3.0;

  p2[0] = pc[0] + r * cos ( theta ) * sin ( phi );
  p2[1] = pc[1] + r * sin ( theta ) * sin ( phi );
  p2[2] = pc[2] + r                 * cos ( phi );

  theta = 2.0 * r8_pi / 3.0;
  phi = 2.0 * r8_pi / 3.0;

  p3[0] = pc[0] + r * cos ( theta ) * sin ( phi );
  p3[1] = pc[1] + r * sin ( theta ) * sin ( phi );
  p3[2] = pc[2] + r                 * cos ( phi );

  theta = 4.0 * r8_pi / 3.0;
  phi = 2.0 * r8_pi / 3.0;

  p4[0] = pc[0] + r * cos ( theta ) * sin ( phi );
  p4[1] = pc[1] + r * sin ( theta ) * sin ( phi );
  p4[2] = pc[2] + r                 * cos ( phi );

  return;
# undef DIM_NUM
}
//****************************************************************************80

double sphere_k ( int dim_num )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_K computes a factor useful for spherical computations.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Thomas Ericson, Victor Zinoviev,
//    Codes on Euclidean Spheres,
//    Elsevier, 2001, pages 439-441.
//    QA166.7 E75
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Output, double SPHERE_K, the factor.
//
{
  double r8_pi = 3.141592653589793;
  double value;

  if ( ( dim_num % 2 ) == 0 )
  {
    value = pow ( 2.0 * r8_pi, dim_num / 2 );
  }
  else
  {
    value = 2.0 * pow ( 2.0 * r8_pi, ( dim_num - 1 ) / 2 );
  }

  value = value / ( double ) ( i4_factorial2 ( dim_num - 2 ) );

  return value;
}
//****************************************************************************80

double sphere_triangle_angles_to_area ( double r, double a, double b, double c )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_TRIANGLE_ANGLES_TO_AREA computes the area of a spherical triangle.
//
//  Discussion:
//
//    A sphere centered at 0 in 3D satisfies the equation:
//
//      X^2 + Y^2 + Z^2 = R^2
//
//    A spherical triangle is specified by three points on the surface
//    of the sphere.
//
//    The area formula is known as Girard's formula.
//
//    The area of a spherical triangle is:
//
//      AREA = ( A + B + C - PI ) * R^2
//
//    where A, B and C are the (surface) angles of the triangle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double A, B, C, the angles of the triangle.
//
//    Output, double SPHERE_TRIANGLE_ANGLES_TO_AREA, the area of the spherical triangle.
//
{
  double area;
  double r8_pi = 3.141592653589793;

  area = r * r * ( a + b + c - r8_pi );

  return area;
}
//****************************************************************************80

void sphere_triangle_sides_to_angles ( double r, double as, double bs, 
  double cs, double &a, double &b, double &c )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_TRIANGLE_SIDES_TO_ANGLES computes spherical triangle angles.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double AS, BS, CS, the (geodesic) length of the sides of the
//    triangle.
//
//    Output, double &A, &B, &C, the spherical angles of the triangle.
//    Angle A is opposite the side of length AS, and so on.
//
{
  double asu;
  double bsu;
  double csu;
  double ssu;
  double tan_a2;
  double tan_b2;
  double tan_c2;

  asu = as / r;
  bsu = bs / r;
  csu = cs / r;
  ssu = ( asu + bsu + csu ) / 2.0;

  tan_a2 = sqrt ( ( sin ( ssu - bsu ) * sin ( ssu - csu ) ) /
                  ( sin ( ssu ) * sin ( ssu - asu )     ) );

  a = 2.0 * atan ( tan_a2 );

  tan_b2 = sqrt ( ( sin ( ssu - asu ) * sin ( ssu - csu ) ) /
                  ( sin ( ssu ) * sin ( ssu - bsu )     ) );

  b = 2.0 * atan ( tan_b2 );

  tan_c2 = sqrt ( ( sin ( ssu - asu ) * sin ( ssu - bsu ) ) /
                  ( sin ( ssu ) * sin ( ssu - csu )     ) );

  c = 2.0 * atan ( tan_c2 );

  return;
}
//****************************************************************************80

void sphere_triangle_vertices_to_angles ( double r, double v1[3], double v2[3],
  double v3[3], double &a, double &b, double &c )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_TRIANGLE_VERTICES_TO_ANGLES computes the angles of a spherical triangle.
//
//  Discussion:
//
//    A sphere centered at 0 in 3D satisfies the equation:
//
//      X*X + Y*Y + Z*Z = R*R
//
//    A spherical triangle is specified by three points on the surface
//    of the sphere.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double V1[3], V2[3], V3[3], the vertices of the triangle.
//
//    Output, double &A, &B, &C, the angles of the spherical triangle.
{
  double as;
  double bs;
  double cs;
//
//  Compute the lengths of the sides of the spherical triangle.
//
  sphere_triangle_vertices_to_sides ( r, v1, v2, v3, as, bs, cs );
//
//  Get the spherical angles.
//
  sphere_triangle_sides_to_angles ( r, as, bs, cs, a, b, c );

  return;
}
//****************************************************************************80

double sphere_triangle_vertices_to_area ( double r, double v1[3], double v2[3],
  double v3[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_TRIANGLE_VERTICES_TO_AREA computes the area of a spherical triangle.
//
//  Discussion:
//
//    A sphere centered at 0 in 3D satisfies the equation:
//
//      X*X + Y*Y + Z*Z = R*R
//
//    A spherical triangle is specified by three points on the surface
//    of the sphere.
//
//    The area formula is known as Girard's formula.
//
//    The area of a spherical triangle is:
//
//      AREA = ( A + B + C - PI ) * R*R
//
//    where A, B and C are the (surface) angles of the triangle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double V1[3], V2[3], V3[3], the vertices of the triangle.
//
//    Output, double SPHERE_TRIANGLE_VERTICES_TO_AREA, the area of the
//    spherical triangle.
{
  double area;
  double a;
  double as;
  double b;
  double bs;
  double c;
  double cs;
//
//  Compute the lengths of the sides of the spherical triangle.
//
  sphere_triangle_vertices_to_sides ( r, v1, v2, v3, as, bs, cs );
//
//  Get the spherical angles.
//
  sphere_triangle_sides_to_angles ( r, as, bs, cs, a, b, c );
//
//  Get the area
//
  area = sphere_triangle_angles_to_area ( r, a, b, c );

  return area;
}
//****************************************************************************80

void sphere_triangle_vertices_to_centroid ( double r, double v1[3], double v2[3],
  double v3[3], double vs[] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_TRIANGLE_VERTICES_TO_CENTROID gets a spherical triangle centroid.
//
//  Discussion:
//
//    A sphere centered at 0 in 3D satisfies the equation:
//
//      X*X + Y*Y + Z*Z = R*R
//
//    A spherical triangle is specified by three points on the sphere.
//
//    The (true) centroid of a spherical triangle is the point
//
//      VT = (XT,YT,ZT) = Integral ( X, Y, Z ) dArea / Integral 1 dArea
//
//    Note that the true centroid does NOT, in general, lie on the sphere.
//
//    The "flat" centroid VF is the centroid of the planar triangle defined by
//    the vertices of the spherical triangle.
//
//    The "spherical" centroid VS of a spherical triangle is computed by
//    the intersection of the geodesic bisectors of the triangle angles.
//    The spherical centroid lies on the sphere.
//
//    VF, VT and VS lie on a line through the center of the sphere.  We can
//    easily calculate VF by averaging the vertices, and from this determine
//    VS by normalizing.
//
//    (Of course, we still will not have actually computed VT, which lies
//    somewhere between VF and VS!)
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double V1[3], V2[3], V3[3], the vertices of the triangle.
//
//    Output, double VS[3], the coordinates of the "spherical centroid"
//    of the spherical triangle.
//
{
# define DIM_NUM 3

  int i;
  double norm;

  for ( i = 0; i < DIM_NUM; i++ )
  {
    vs[i] = ( v1[i] + v2[i] + v3[i] ) / 3.0;
  }

  norm = r8vec_norm ( DIM_NUM, vs );

  for ( i = 0; i < DIM_NUM; i++ )
  {
    vs[i] = r * vs[i] / norm;
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

int sphere_triangle_vertices_to_orientation ( double a[], double b[], double c[] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_TRIANGLE_VERTICES_TO_ORIENTATION seeks the orientation of a spherical triangle.
//
//  Discussion:
//
//    Three points on a sphere actually compute two triangles; typically
//    we are interested in the smaller of the two.
//
//    As long as our triangle is "small", we can define an orientation
//    by comparing the direction of the centroid against the normal
//    vector (C-B) x (A-B).  If the dot product of these vectors
//    is positive, we say the triangle has positive orientation.
//
//    By using information from the triangle orientation, we can correctly
//    determine the area of a Voronoi polygon by summing up the pieces
//    of Delaunay triangles, even in the case when the Voronoi vertex
//    lies outside the Delaunay triangle.  In that case, the areas of
//    some of the Delaunay triangle pieces must be formally negative.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A[3], B[3], C[3], three points on a sphere.
//
//    Output, int SPHERE_TRIANGLE_VERTICES_TO_ORIENTATION, is +1 if the spherical triangle
//    is judged to have positive orientation, and -1 otherwise.
//
{
  double cd[3];
  double cp[3];
  int i;
  int o;
  double v1[3];
  double v2[3];
//
//  Centroid.
//
  for ( i = 0; i < 3; i++ )
  {
    cd[i] = ( a[i] + b[i] + c[i] ) / 3.0;
  }
//
//  Cross product ( C - B ) x ( A - B );
//
  for ( i = 0; i < 3; i++ )
  {
    v1[i] = c[i] - b[i];
    v2[i] = a[i] - b[i];
  }
  cp[0] = v1[1] * v2[2] - v1[2] * v2[1];
  cp[1] = v1[2] * v2[0] - v1[0] * v2[2];
  cp[2] = v1[0] * v2[1] - v1[1] * v2[0];
//
//  Compare the directions.
//
  if ( r8vec_dot_product ( 3, cp, cd ) < 0.0 )
  {
    o = - 1;
  }
  else
  {
    o = + 1;
  }
  return o;
}
//****************************************************************************80

void sphere_triangle_vertices_to_sides ( double r, double v1[3], double v2[3],
  double v3[3], double &as, double &bs, double &cs )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_TRIANGLE_VERTICES_TO_SIDES_3D computes spherical triangle sides.
//
//  Discussion:
//
//    We can use the ACOS system call here, but the ARC_COSINE routine
//    will automatically take care of cases where the input argument is
//    (usually slightly) out of bounds.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R, the radius of the sphere.
//
//    Input, double V1[3], V2[3], V3[3], the vertices of the spherical
//    triangle.
//
//    Output, double &AS, &BS, &CS, the (geodesic) length of the sides of the
//    triangle.
//
{
  as = r * r8_acos ( r8vec_dot_product ( 3, v2, v3 ) / ( r * r ) );
  bs = r * r8_acos ( r8vec_dot_product ( 3, v3, v1 ) / ( r * r ) );
  cs = r * r8_acos ( r8vec_dot_product ( 3, v1, v2 ) / ( r * r ) );

  return;
}
//****************************************************************************80

double sphere_unit_area_nd ( int dim_num )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_UNIT_AREA_ND computes the surface area of a unit sphere in ND.
//
//  Discussion:
//
//    The unit sphere in ND satisfies the equation:
//
//      Sum ( 1 <= I <= DIM_NUM ) X(I) * X(I) = 1
//
//    DIM_NUM   Area
//
//     2    2        * PI
//     3    4        * PI
//     4  ( 2 /   1) * PI^2
//     5  ( 8 /   3) * PI^2
//     6  ( 1 /   1) * PI^3
//     7  (16 /  15) * PI^3
//     8  ( 1 /   3) * PI^4
//     9  (32 / 105) * PI^4
//    10  ( 1 /  12) * PI^5
//
//    For the unit sphere, Area(DIM_NUM) = DIM_NUM * Volume(DIM_NUM)
//
//    Sphere_Unit_Area ( DIM_NUM ) = 2 * PI^(DIM_NUM/2) / Gamma ( DIM_NUM / 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Output, double SPHERE_UNIT_AREA_ND, the area of the sphere.
//
{
  double area;
  int i;
  int m;
  double r8_pi = 3.141592653589793;

  if ( ( dim_num % 2 ) == 0 )
  {
    m = dim_num / 2;
    area = 2.0 * pow ( r8_pi, m );
    for ( i = 1; i <= m-1; i++ )
    {
      area = area / ( ( double ) i );
    }
  }
  else
  {
    m = ( dim_num - 1 ) / 2;
    area = pow ( 2.0, dim_num ) * pow ( r8_pi, m );
    for ( i = m + 1; i <= 2 * m; i++ )
    {
      area = area / ( ( double ) i );
    }
  }

  return area;
}
//****************************************************************************80

void sphere_unit_area_values ( int &n_data, int &n, double &area )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_UNIT_AREA_VALUES returns some areas of the unit sphere in ND.
//
//  Discussion:
//
//    The formula for the surface area of the unit sphere in N dimensions is:
//
//      Sphere_Unit_Area ( N ) = 2 * PI^(N/2) / Gamma ( N / 2 )
//
//    Some values of the function include:
//
//       N   Area
//
//       2    2        * PI
//       3  ( 4 /    ) * PI
//       4  ( 2 /   1) * PI^2
//       5  ( 8 /   3) * PI^2
//       6  ( 1 /   1) * PI^3
//       7  (16 /  15) * PI^3
//       8  ( 1 /   3) * PI^4
//       9  (32 / 105) * PI^4
//      10  ( 1 /  12) * PI^5
//
//    For the unit sphere, Area(N) = N * Volume(N)
//
//    In Mathematica, the function can be evaluated by:
//
//      2 * Pi^(n/2) / Gamma[n/2]
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    20 August 2004
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Stephen Wolfram,
//    The Mathematica Book,
//    Fourth Edition,
//    Cambridge University Press, 1999,
//    ISBN: 0-521-64314-7,
//    LC: QA76.95.W65.
//
//  Parameters:
//
//    Input/output, int &N_DATA.
//    On input, if N_DATA is 0, the first test data is returned, and
//    N_DATA is set to the index of the test data.  On each subsequent
//    call, N_DATA is incremented and that test data is returned.  When
//    there is no more test data, N_DATA is set to 0.
//
//    Output, int &N, the spatial dimension.
//
//    Output, double &AREA, the area of the unit sphere
//    in that dimension.
//
{
# define N_MAX 20

  static double area_vec[N_MAX] = {
     0.2000000000000000E+01,
     0.6283185307179586E+01,
     0.1256637061435917E+02,
     0.1973920880217872E+02,
     0.2631894506957162E+02,
     0.3100627668029982E+02,
     0.3307336179231981E+02,
     0.3246969701133415E+02,
     0.2968658012464836E+02,
     0.2550164039877345E+02,
     0.2072514267328890E+02,
     0.1602315322625507E+02,
     0.1183817381218268E+02,
     0.8389703410491089E+01,
     0.5721649212349567E+01,
     0.3765290085742291E+01,
     0.2396678817591364E+01,
     0.1478625959000308E+01,
     0.8858104195716824E+00,
     0.5161378278002812E+00 };

  static int n_vec[N_MAX] = {
     1,
     2,
     3,
     4,
     5,
     6,
     7,
     8,
     9,
    10,
    11,
    12,
    13,
    14,
    15,
    16,
    17,
    18,
    19,
    20 };

  if ( n_data < 0 )
  {
    n_data = 0;
  }

  n_data = n_data + 1;

  if ( N_MAX < n_data )
  {
    n_data = 0;
    n = 0;
    area = 0.0;
  }
  else
  {
    n = n_vec[n_data-1];
    area = area_vec[n_data-1];
  }

  return;
# undef N_MAX
}
//****************************************************************************80

double *sphere_unit_sample_2d ( int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_UNIT_SAMPLE_2D picks a random point on the unit sphere (circle) in 2D.
//
//  Discussion:
//
//    The unit sphere in 2D satisfies the equation:
//
//      X * X + Y * Y = 1
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double SPHERE_UNIT_SAMPLE_2D[2], the random point on the unit circle.
//
{
  double r8_pi = 3.141592653589793;
  double u;
  double *x;

  u = r8_uniform_01 ( seed );

  x = new double[2];

  x[0] = cos ( 2.0 * r8_pi * u );
  x[1] = sin ( 2.0 * r8_pi * u );

  return x;
}
//****************************************************************************80

double *sphere_unit_sample_3d ( int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_UNIT_SAMPLE_3D picks a random point on the unit sphere in 3D.
//
//  Discussion:
//
//    The unit sphere in 3D satisfies the equation:
//
//      X * X + Y * Y + Z * Z = 1
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double SPHERE_UNIT_SAMPLE_3D[3], the sample point.
//
{
  double phi;
  double r8_pi = 3.141592653589793;
  double theta;
  double vdot;
  double *x;
//
//  Pick a uniformly random VDOT, which must be between -1 and 1.
//  This represents the dot product of the random vector with the Z unit vector.
//
//   this works because the surface area of the sphere between
//  Z and Z + dZ is independent of Z.  So choosing Z uniformly chooses
//  a patch of area uniformly.
//
  vdot = 2.0 * r8_uniform_01 ( seed ) - 1.0;

  phi = r8_acos ( vdot );
//
//  Pick a uniformly random rotation between 0 and 2 Pi around the
//  axis of the Z vector.
//
  theta = 2.0 * r8_pi * r8_uniform_01 ( seed );

  x = new double[3];

  x[0] = cos ( theta ) * sin ( phi );
  x[1] = sin ( theta ) * sin ( phi );
  x[2] = cos ( phi );

  return x;
}
//****************************************************************************80

double *sphere_unit_sample_3d_2 ( int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_UNIT_SAMPLE_3D_2 is a BAD method for sampling the unit sphere in 3D.
//
//  Discussion:
//
//    The unit sphere in 3D satisfies the equation:
//
//      X * X + Y * Y + Z * Z = 1
//
//    Points on the unit sphere have coordinates ( PHI, THETA ) where
//    PHI varies from 0 to PI, and THETA from 0 to 2 PI, so that:
//
//    x = cos ( theta ) * sin ( phi )
//    y = sin ( theta ) * sin ( phi )
//    z =                 cos ( phi )
//
//    This routine implements a sampling of the sphere that simply
//    picks PHI and THETA uniformly at random from their ranges.
//    This is a uniform sampling on the cylinder, but it is NOT
//    a uniform sampling on the sphere.  I implement it here just
//    so I can run some tests against the code in SPHERE_UNIT_SAMPLE_3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double SPHERE_UNIT_SAMPLE_3D_2[3], the sample point.
//
{
  double phi;
  const double r8_pi = 3.141592653589793;
  double theta;
  double *x;

  phi = r8_pi * r8_uniform_01 ( seed );
  theta = 2.0 * r8_pi * r8_uniform_01 ( seed );

  x = new double[3];

  x[0] = cos ( theta ) * sin ( phi );
  x[1] = sin ( theta ) * sin ( phi );
  x[2] = cos ( phi );

  return x;
}
//****************************************************************************80

double *sphere_unit_sample_nd ( int dim_num, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_UNIT_SAMPLE_ND picks a random point on the unit sphere in ND.
//
//  Discussion:
//
//    The unit sphere in ND satisfies the equation:
//
//      Sum ( 1 <= I <= DIM_NUM ) X(I) * X(I) = 1
//
//    DIM_NUM-1 random Givens rotations are applied to the point ( 1, 0, 0, ..., 0 ).
//
//    The I-th Givens rotation is in the plane of coordinate axes I and I+1,
//    and has the form:
//
//     [ cos ( theta )  - sin ( theta ) ] * x(i)      = x'(i)
//     [ sin ( theta )    cos ( theta ) ]   x(i+1)      x'(i+1)
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double SPHERE_UNIT_SAMPLE_ND[DIM_NUM], the random point.
//
{
  int i;
  double random_cosine;
  double random_sign;
  double random_sine;
  double *p;
  double pi;

  p = new double[dim_num];

  p[0] = 1.0;
  for ( i = 1; i < dim_num; i++ )
  {
    p[i] = 0.0;
  }

  for ( i = 0; i < dim_num-1; i++ )
  {
    random_cosine = 2.0 * r8_uniform_01 ( seed ) - 1.0;

    random_sign = ( double ) ( 2 * ( int ) ( 2.0 *
      r8_uniform_01 ( seed ) ) - 1 );

    random_sine = random_sign
      * sqrt ( 1.0 - random_cosine * random_cosine );

    pi = p[i];
    p[i  ] = random_cosine * pi;
    p[i+1] = random_sine   * pi;
  }

  return p;
}
//****************************************************************************80

double *sphere_unit_sample_nd_2 ( int dim_num, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_UNIT_SAMPLE_ND_2 picks a random point on the unit sphere in ND.
//
//  Discussion:
//
//    The unit sphere in ND satisfies the equation:
//
//      Sum ( 1 <= I <= DIM_NUM ) X(I) * X(I) = 1
//
//    DIM_NUM independent normally distributed random numbers are generated,
//    and then scaled to have unit norm.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double SPHERE_UNIT_SAMPLE_ND_2[DIM_NUM], the random point.
//
{
  int i;
  double norm;
  double *p;

  p = r8vec_normal_01_new ( dim_num, seed );

  norm = r8vec_norm ( dim_num, p );

  for ( i = 0; i < dim_num; i++ )
  {
    p[i] = p[i] / norm;
  }

  return p;
}
//****************************************************************************80

double *sphere_unit_sample_nd_3 ( int dim_num, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_UNIT_SAMPLE_ND_3 picks a random point on the unit sphere in ND.
//
//  Discussion:
//
//    The unit sphere in ND satisfies the equation:
//
//      Sum ( 1 <= I <= DIM_NUM ) X(I) * X(I) = 1
//
//    Points in the [-1,1] cube are generated.  Points lying outside
//    the sphere are rejected.  Points inside the unit sphere are normalized
//    to lie on the sphere.
//
//    Because the volume of the unit sphere
//    relative to the unit cube decreases drastically in higher dimensions,
//    this routine becomes increasingly inefficient at higher DIM_NUM.
//    Above DIM_NUM = 5, this problem will become significant.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double SPHERE_UNIT_SAMPLE_ND_3[DIM_NUM], the random point.
//
{
  int i;
  double norm;
  double *p;

  for ( ; ; )
  {
    p = r8vec_uniform_01_new ( dim_num, seed );

    for ( i = 0; i < dim_num; i++ )
    {
      p[i] = 2.0 * p[i] - 1.0;
    }

    norm = r8vec_norm ( dim_num, p );

    if ( norm <= 1.0 )
    {
      for ( i = 0; i < dim_num; i++ )
      {
        p[i] = p[i] / norm;
      }
      break;
    }
    delete [] p;
  }

  return p;
}
//****************************************************************************80

double sphere_unit_volume_nd ( int dim_num )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_UNIT_VOLUME_ND computes the volume of a unit sphere in ND.
//
//  Discussion:
//
//    The unit sphere in ND satisfies the equation:
//
//      Sum ( 1 <= I <= DIM_NUM ) X(I) * X(I) = 1
//
//     DIM_NUM  Volume
//
//     1    2
//     2    1        * PI
//     3  ( 4 /   3) * PI
//     4  ( 1 /   2) * PI^2
//     5  ( 8 /  15) * PI^2
//     6  ( 1 /   6) * PI^3
//     7  (16 / 105) * PI^3
//     8  ( 1 /  24) * PI^4
//     9  (32 / 945) * PI^4
//    10  ( 1 / 120) * PI^5
//
//    For the unit sphere, Volume(N) = 2 * PI * Volume(N-2)/ N
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the space.
//
//    Output, double SPHERE_UNIT_VOLUME_ND, the volume of the sphere.
//
{
  int i;
  int m;
  double r8_pi = 3.141592653589793;
  double volume;

  if ( dim_num % 2== 0 )
  {
    m = dim_num / 2;
    volume = 1.0;
    for ( i = 1; i <= m; i++ )
    {
      volume = volume * r8_pi / ( ( double ) i );
    }
  }
  else
  {
    m = ( dim_num - 1 ) / 2;
    volume = pow ( r8_pi, m ) * pow ( 2.0, dim_num );
    for ( i = m + 1; i <= 2 * m + 1; i++ )
    {
      volume = volume / ( ( double ) i );
    }
  }

  return volume;
}
//****************************************************************************80

void sphere_unit_volume_values ( int &n_data, int &n, double &volume )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE_UNIT_VOLUME_VALUES returns some volumes of the unit sphere in ND.
//
//  Discussion:
//
//    The formula for the volume of the unit sphere in N dimensions is
//
//      Volume(N) = 2 * PI^(N/2) / ( N * Gamma ( N / 2 ) )
//
//    This function satisfies the relationships:
//
//      Volume(N) = 2 * PI * Volume(N-2) / N
//      Volume(N) = Area(N) / N
//
//    Some values of the function include:
//
//       N  Volume
//
//       1    1
//       2    1        * PI
//       3  ( 4 /   3) * PI
//       4  ( 1 /   2) * PI^2
//       5  ( 8 /  15) * PI^2
//       6  ( 1 /   6) * PI^3
//       7  (16 / 105) * PI^3
//       8  ( 1 /  24) * PI^4
//       9  (32 / 945) * PI^4
//      10  ( 1 / 120) * PI^5
//
//    In Mathematica, the function can be evaluated by:
//
//      2 * Pi^(n/2) / ( n * Gamma[n/2] )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    21 August 2004
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Stephen Wolfram,
//    The Mathematica Book,
//    Fourth Edition,
//    Cambridge University Press, 1999,
//    ISBN: 0-521-64314-7,
//    LC: QA76.95.W65.
//
//  Parameters:
//
//    Input/output, int &N_DATA.
//    On input, if N_DATA is 0, the first test data is returned, and
//    N_DATA is set to the index of the test data.  On each subsequent
//    call, N_DATA is incremented and that test data is returned.  When
//    there is no more test data, N_DATA is set to 0.
//
//    Output, int &N, the spatial dimension.
//
//    Output, double &VOLUME, the volume of the unit
//    sphere in that dimension.
//
{
# define N_MAX 20

  static int n_vec[N_MAX] = {
     1,  2,
     3,  4,
     5,  6,
     7,  8,
     9, 10,
    11, 12,
    13, 14,
    15, 16,
    17, 18,
    19, 20 };

  static double volume_vec[N_MAX] = {
     0.2000000000000000E+01,
     0.3141592653589793E+01,
     0.4188790204786391E+01,
     0.4934802200544679E+01,
     0.5263789013914325E+01,
     0.5167712780049970E+01,
     0.4724765970331401E+01,
     0.4058712126416768E+01,
     0.3298508902738707E+01,
     0.2550164039877345E+01,
     0.1884103879389900E+01,
     0.1335262768854589E+01,
     0.9106287547832831E+00,
     0.5992645293207921E+00,
     0.3814432808233045E+00,
     0.2353306303588932E+00,
     0.1409811069171390E+00,
     0.8214588661112823E-01,
     0.4662160103008855E-01,
     0.2580689139001406E-01  };

  if ( n_data < 0 )
  {
    n_data = 0;
  }

  n_data = n_data + 1;

  if ( N_MAX < n_data )
  {
    n_data = 0;
    n = 0;
    volume = 0.0;
  }
  else
  {
    n = n_vec[n_data-1];
    volume = volume_vec[n_data-1];
  }

  return;
# undef N_MAX
}
//****************************************************************************80

double sphere01_distance_xyz ( double xyz1[3], double xyz2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE01_DISTANCE_XYZ computes great circle distances on a unit sphere.
//
//  Discussion:
//
//    XYZ coordinates are used.
//
//    We assume the points XYZ1 and XYZ2 lie on the unit sphere.
//
//    This computation is a special form of the Vincenty formula.
//    It should be less sensitive to errors associated with very small
//    or very large angular separations.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 September 2010
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    "Great-circle distance",
//    Wikipedia.
//
//  Parameters:
//
//    Input, double XYZ1[3], the coordinates of the first point.
//
//    Input, double XYZ2[3], the coordinates of the second point.
//
//    Output, double DIST, the great circle distance between the points.
//
{
  double bot;
  double dist;
  double lat1;
  double lat2;
  double lon1;
  double lon2;
  double top;

  lat1 = r8_asin ( xyz1[2] );
  lon1 = r8_atan ( xyz1[1], xyz1[0] );

  lat2 = r8_asin ( xyz2[2] );
  lon2 = r8_atan ( xyz2[1], xyz2[0] );

  top = pow ( cos ( lat2 ) * sin ( lon1 - lon2 ), 2 )
      + pow ( cos ( lat1 ) * sin ( lat2 )
            - sin ( lat1 ) * cos ( lat2 ) * cos ( lon1 - lon2 ), 2 );

  top = sqrt ( top );

  bot = sin ( lat1 ) * sin ( lat2 )
      + cos ( lat1 ) * cos ( lat2 ) * cos ( lon1 - lon2 );

  dist = atan2 ( top, bot );

  return dist;
}
//****************************************************************************80

double sphere01_polygon_area ( int n, double lat[], double lon[] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE01_POLYGON_AREA returns the area of a spherical polygon.
//
//  Discussion:
//
//    On a unit sphere, the area of a spherical polygon with N sides
//    is equal to the spherical excess:
//
//      E = sum ( interior angles ) - ( N - 2 ) * pi.
//
//    On a sphere with radius R, the area is the spherical excess multiplied
//    by R * R.
//
//    The code was revised in accordance with suggestions in Carvalho
//    and Cavalcanti.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 August 2005
//
//  Author:
//
//    Robert Miller
//
//  Reference:
//
//    Paulo Cezar Pinto Carvalho, Paulo Roma Cavalcanti,
//    Point in Polyhedron Testing Using Spherical Polygons,
//    Graphics Gems, Volume V,
//    Edited by Alan Paeth,
//    Academic Press, 1995, T385.G6975.
//
//    Robert Miller,
//    Computing the Area of a Spherical Polygon,
//    Graphics Gems, Volume IV, pages 132-138,
//    Edited by Paul Heckbert,
//    Academic Press, 1994, T385.G6974.
//
//    Eric Weisstein,
//    "Spherical Polygon",
//    CRC Concise Encyclopedia of Mathematics,
//    CRC Press, 1999.
//
//  Parameters:
//
//    Input, int N, the number of vertices.
//
//    Input, double LAT[N], LON[N], the latitudes and longitudes of the vertices
//    of the spherical polygon.
//
//    Output, double SPHERE01_POLYGON_AREA, the area of the spherical polygon
//    in spherical radians.
//
{
  double a = 0.0;
  double area = 0.0;
  double b = 0.0;
  double beta1 = 0.0;
  double beta2 = 0.0;
  double c = 0.0;
  double cos_b1 = 0.0;
  double cos_b2 = 0.0;
  double excess = 0.0;
  double hav_a = 0.0;
  int j;
  int k;
  double lam = 0.0;
  double lam1 = 0.0;
  double lam2 = 0.0;
  double r8_pi = 3.141592653589793;
  double s;
  double t;

  area = 0.0;

  for ( j = 0; j <= n; j++ )
  {
    if ( j == 0 )
    {
      lam1 = lon[j];
      beta1 = lat[j];
      lam2 = lon[j+1];
      beta2 = lat[j+1];
      cos_b1 = cos ( beta1 );
      cos_b2 = cos ( beta2 );
    }
    else
    {
      k = ( j + 1 ) % ( n + 1 );
      lam1 = lam2;
      beta1 = beta2;
      lam2 = lon[k];
      beta2 = lat[k];
      cos_b1 = cos_b2;
      cos_b2 = cos ( beta2 );
    }

    if ( lam1 != lam2 )
    {
      hav_a = haversine ( beta2 - beta1 )
        + cos_b1 * cos_b2 * haversine ( lam2 - lam1 );
      a = 2.0 * asin ( sqrt ( hav_a ) );

      b = 0.5 * r8_pi - beta2;
      c = 0.5 * r8_pi - beta1;
      s = 0.5 * ( a + b + c );
//
//  Given the three sides of a spherical triangle, we can use a formula
//  to find the spherical excess.
//
      t = tan ( s / 2.0 ) * tan ( ( s - a ) / 2.0 )
        * tan ( ( s - b ) / 2.0 ) * tan ( ( s - c ) / 2.0 );

      excess = fabs ( 4.0 * atan ( sqrt ( fabs ( t ) ) ) );

      if ( lam1 < lam2 )
      {
        lam = lam2 - lam1;
      }
      else
      {
        lam = lam2 - lam1 + 2.0 * r8_pi;
      }

      if ( r8_pi < lam )
      {
        excess = - excess;
      }

      area = area + excess;
    }
  }
  if ( area < 0.0 )
  {
    area = 4.0 * r8_pi + area;
  }
//area = fabs ( area );

  return area;
}
//****************************************************************************80

double sphere01_polygon_area_karney ( int n, double lat[], double lon[] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE01_POLYGON_AREA_KARNEY returns the area of a spherical polygon.
//
//  Discussion:
//
//    On a unit sphere, the area of a spherical polygon with N sides
//    is equal to the spherical excess:
//
//      E = sum ( interior angles ) - ( N - 2 ) * pi.
//
//    On a sphere with radius R, the area is the spherical excess multiplied
//    by R * R.
//
//    The text of this function was kindly supplied by the author.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 September 2010
//
//  Author:
//
//    Charles Karney
//
//  References:
//
//    Charles Karney,
//    GeographicLib,
//    September 2010
//    http://geographiclib.sf.net/html/geodesic.html#geodarea
//
//    Isaac Todhunter,
//    Section 103, equation 2,
//    Spherical Trigonometry,
//    Macmillan, 1871,
//    http://books.google.com/books?id=3uBHAAAAIAAJ&pg=PA71
//
//    This routine gives the area for spherical polygons which do not include a
//    pole.  A solution of this problem for ellipsoidal polygons which include
//    a pole is provided by the Planimeter utility of GeographicLib; see
//    http://geographiclib.sf.net/html/utilities.html#planimeter
//
//  Parameters:
//
//    Input, int N, the number of vertices.
//
//    Input, double LAT[N], LON[N], the latitudes and longitudes of the
//    vertices of the spherical polygon, measured in radians.
//
//    Output, double SPHERE01_POLYGON_AREA_KARNEY, the signed area of the
//    spherical polygon in spherical radians.  Clockwise traversal yields a
//    positive result.
//
{
  double area;
  int j;
  double lam1;
  double lam2;
  double tbeta1;
  double tbeta2;

  area = 0.0;
  lam2 = lon[n-1];
  tbeta2 = tan ( lat[n-1] / 2.0 );

  for ( j = 0; j < n; j++ )
  {
    lam1 = lam2;
    lam2 = lon[j];
    tbeta1 = tbeta2;
    tbeta2 = tan ( lat[j] / 2.0 );
    area = area + atan2 ( tan ( ( lam1 - lam2 ) / 2.0 ) * ( tbeta1 + tbeta2 ),
                          ( 1.0 + tbeta1 * tbeta2 ) );
  }
  area = 2.0 * area;

  return area;
}
//****************************************************************************80

double sphere01_triangle_angles_to_area ( double a, double b, double c )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE01_TRIANGLE_ANGLES_TO_AREA: area of a spherical triangle on the unit sphere.
//
//  Discussion:
//
//    A unit sphere centered at 0 in 3D satisfies the equation:
//
//      X^2 + Y^2 + Z^2 = 1
//
//    A spherical triangle is specified by three points on the surface
//    of the sphere.
//
//    The area formula is known as Girard's formula.
//
//    The area of a spherical triangle on the unit sphere is:
//
//      AREA = ( A + B + C - PI )
//
//    where A, B and C are the (surface) angles of the triangle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 September 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A, B, C, the angles of the triangle.
//
//    Output, double SPHERE_TRIANGLE_ANGLES_TO_AREA, the area of the
//    spherical triangle.
//
{
  double area;
  double r8_pi = 3.141592653589793;

  area = a + b + c - r8_pi;

  return area;
}
//****************************************************************************80

void sphere01_triangle_sides_to_angles ( double as, double bs, double cs,
  double &a, double &b, double &c )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE01_TRIANGLE_SIDES_TO_ANGLES: angles of spherical triangle on unit sphere.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 September 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double AS, BS, CS, the (geodesic) length of the sides of the
//    triangle.
//
//    Output, double &A, &B, &C, the spherical angles of the triangle.
//    Angle A is opposite the side of length AS, and so on.
//
{
  double asu;
  double bsu;
  double csu;
  double ssu;
  double tan_a2;
  double tan_b2;
  double tan_c2;

  asu = as;
  bsu = bs;
  csu = cs;
  ssu = ( asu + bsu + csu ) / 2.0;

  tan_a2 = sqrt ( ( sin ( ssu - bsu ) * sin ( ssu - csu ) ) /
                  ( sin ( ssu ) * sin ( ssu - asu )     ) );

  a = 2.0 * atan ( tan_a2 );

  tan_b2 = sqrt ( ( sin ( ssu - asu ) * sin ( ssu - csu ) ) /
                  ( sin ( ssu ) * sin ( ssu - bsu )     ) );

  b = 2.0 * atan ( tan_b2 );

  tan_c2 = sqrt ( ( sin ( ssu - asu ) * sin ( ssu - bsu ) ) /
                  ( sin ( ssu ) * sin ( ssu - csu )     ) );

  c = 2.0 * atan ( tan_c2 );

  return;
}
//****************************************************************************80

void sphere01_triangle_vertices_to_angles ( double v1[3], double v2[3],
  double v3[3], double &a, double &b, double &c )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE01_TRIANGLE_VERTICES_TO_ANGLES: angles of spherical triangle on unit sphere.
//
//  Discussion:
//
//    A unit sphere centered at 0 in 3D satisfies the equation:
//
//      X*X + Y*Y + Z*Z = 1
//
//    A spherical triangle is specified by three points on the surface
//    of the sphere.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 September 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V1[3], V2[3], V3[3], the vertices of the triangle.
//
//    Output, double &A, &B, &C, the angles of the spherical triangle.
{
  double as;
  double bs;
  double cs;
//
//  Compute the lengths of the sides of the spherical triangle.
//
  sphere01_triangle_vertices_to_sides ( v1, v2, v3, as, bs, cs );
//
//  Get the spherical angles.
//
  sphere01_triangle_sides_to_angles ( as, bs, cs, a, b, c );

  return;
}
//****************************************************************************80

double sphere01_triangle_vertices_to_area ( double v1[3], double v2[3], 
  double v3[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE01_TRIANGLE_VERTICES_TO_AREA: area of a spherical triangle on unit sphere.
//
//  Discussion:
//
//    A unit sphere centered at 0 in 3D satisfies the equation:
//
//      X*X + Y*Y + Z*Z = 1
//
//    A spherical triangle is specified by three points on the surface
//    of the sphere.
//
//    The area formula is known as Girard's formula.
//
//    The area of a spherical triangle on the unit sphere is:
//
//      AREA = A + B + C - PI
//
//    where A, B and C are the (surface) angles of the triangle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 September 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V1[3], V2[3], V3[3], the vertices of the triangle.
//
//    Output, double SPHERE_TRIANGLE_VERTICES_TO_AREA, the area of the
//    spherical triangle.
{
  double area;
  double a;
  double as;
  double b;
  double bs;
  double c;
  double cs;
//
//  Compute the lengths of the sides of the spherical triangle.
//
  sphere01_triangle_vertices_to_sides ( v1, v2, v3, as, bs, cs );
//
//  Get the spherical angles.
//
  sphere01_triangle_sides_to_angles ( as, bs, cs, a, b, c );
//
//  Get the area
//
  area = sphere01_triangle_angles_to_area ( a, b, c );

  return area;
}
//****************************************************************************80

double *sphere01_triangle_vertices_to_centroid ( double v1[3], double v2[3], 
  double v3[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE01_TRIANGLE_VERTICES_TO_CENTROID: centroid of spherical triangle on unit sphere.
//
//  Discussion:
//
//    A sphere centered at 0 in 3D satisfies the equation:
//
//      X*X + Y*Y + Z*Z = 1
//
//    A spherical triangle is specified by three points on the sphere.
//
//    The (true) centroid of a spherical triangle is the point
//
//      VT = (XT,YT,ZT) = Integral ( X, Y, Z ) dArea / Integral 1 dArea
//
//    Note that the true centroid does NOT, in general, lie on the sphere.
//
//    The "flat" centroid VF is the centroid of the planar triangle defined by
//    the vertices of the spherical triangle.
//
//    The "spherical" centroid VS of a spherical triangle is computed by
//    the intersection of the geodesic bisectors of the triangle angles.
//    The spherical centroid lies on the sphere.
//
//    VF, VT and VS lie on a line through the center of the sphere.  We can
//    easily calculate VF by averaging the vertices, and from this determine
//    VS by normalizing.
//
//    (Of course, we still will not have actually computed VT, which lies
//    somewhere between VF and VS!)
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 September 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V1[3], V2[3], V3[3], the vertices of the triangle.
//
//    Output, double SPHERE01_TRIANGLE_VERTICES_TO_CENTROID[3], the coordinates
//    of the "spherical centroid" of the spherical triangle.
//
{
# define DIM_NUM 3

  int i;
  double norm;
  double *vs;

  vs = new double[3];

  for ( i = 0; i < DIM_NUM; i++ )
  {
    vs[i] = ( v1[i] + v2[i] + v3[i] ) / 3.0;
  }

  norm = r8vec_norm ( DIM_NUM, vs );

  for ( i = 0; i < DIM_NUM; i++ )
  {
    vs[i] = vs[i] / norm;
  }

  return vs;
# undef DIM_NUM
}
//****************************************************************************80

void sphere01_triangle_vertices_to_midpoints ( double v1[3], double v2[3], 
  double v3[3], double m1[3], double m2[3], double m3[3] )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE01_TRIANGLE_VERTICES_TO_MIDPOINTS gets the midsides of a spherical triangle.
//
//  Discussion:
//
//    The points are assumed to lie on the unit sphere.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 September 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V1[3], V2[3], V3[3], the vertices of the triangle.
//
//    Output, double M1[3], M2[3], M3[3], the coordinates of
//    the midpoints of the sides of the spherical triangle.
//
{
  int i;
  double norm;

  for ( i = 0; i < 3; i++ )
  {
    m1[i] = ( v1[i] + v2[i] ) / 2.0;
  }
  norm = r8vec_norm ( 3, m1 );
  for ( i = 0; i < 3; i++ )
  {
    m1[i] = m1[i] / norm;
  }

  for ( i = 0; i < 3; i++ )
  {
    m2[i] = ( v2[i] + v3[i] ) / 2.0;
  }
  norm = r8vec_norm ( 3, m2 );
  for ( i = 0; i < 3; i++ )
  {
    m2[i] = m2[i] / norm;
  }

  for ( i = 0; i < 3; i++ )
  {
    m3[i] = ( v3[i] + v1[i] ) / 2.0;
  }
  norm = r8vec_norm ( 3, m3 );
  for ( i = 0; i < 3; i++ )
  {
    m3[i] = m3[i] / norm;
  }

  return;
}
//****************************************************************************80

void sphere01_triangle_vertices_to_sides ( double v1[3], double v2[3],
  double v3[3], double &as, double &bs, double &cs )

//****************************************************************************80
//
//  Purpose:
//
//    SPHERE01_TRIANGLE_VERTICES_TO_SIDES_3D: sides of spherical triangle on unit sphere.
//
//  Discussion:
//
//    We can use the ACOS system call here, but the ARC_COSINE routine
//    will automatically take care of cases where the input argument is
//    (usually slightly) out of bounds.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    23 September 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V1[3], V2[3], V3[3], the vertices of the spherical
//    triangle.
//
//    Output, double &AS, &BS, &CS, the (geodesic) length of the sides of the
//    triangle.
//
{
  as = r8_acos ( r8vec_dot_product ( 3, v2, v3 ) );
  bs = r8_acos ( r8vec_dot_product ( 3, v3, v1 ) );
  cs = r8_acos ( r8vec_dot_product ( 3, v1, v2 ) );

  return;
}
//****************************************************************************80

void string_2d ( int vec_num, double p1[], double p2[], int *string_num,
  int order[], int string[] )

//****************************************************************************80
//
//  Purpose:
//
//    STRING_2D groups line segments into connected lines in 2D.
//
//  Discussion:
//
//    The routine receives an unordered set of line segments, described by
//    pairs of coordinates P1 and P2, and tries to group them
//    into ordered lists that constitute connected jagged lines.
//
//    This routine will not match two endpoints unless they are exactly equal.
//
//    On input, line segment I has endpoints PI(I) and P2(I).
//
//    On output, the order of the components may have been
//    switched.  That is, for some I, P1(I) and P2(I) may have been swapped.
//
//    More importantly, all the entries P1(I) and P2(I)
//    may have been swapped with another index J.
//
//    The resulting coordinates will have been sorted in order
//    of the string to which they belong, and then by the order
//    of their traversal within that string.
//
//    The array STRING(I) identifies the string to which segment I belongs.
//
//    If two segments I and J have the same value of STRING, then
//    ORDER(I) and ORDER(J) give the relative order of the two segments
//    in the string.  Thus if ORDER(I) = -3 and ORDER(J) = 2, then when
//    the string is traversed, segment I is traversed first, then four other
//    segments are traversed, and then segment J is traversed.
//
//    For each string, the segment with ORDER(I) = 0 is the initial segment
//    from which the entire string was "grown" (with growth possible to both the
//    left and the right).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int VEC_NUM, the number of line segments to be analyzed.
//
//    Input/output, double P1[2*VEC_NUM], P2[2*VEC_NUM], the line segments.
//
//    Output, int *STRING_NUM, the number of strings created.
//
//    Output, int ORDER[VEC_NUM], the order vector.
//
//    Output, int STRING[VEC_NUM], the string to which each segment I belongs.
//
{
  int i;
  int indx;
  int isgn;
  int itemp;
  int j;
  int jval;
  int kval;
  int match;
  int seed;
  double temp;
  double x1val;
  double x2val;
  double y1val;
  double y2val;
//
//  Mark STRING so that each segment is alone.
//
  for ( i = 0; i < vec_num; i++ )
  {
    order[i] = 0;
    string[i] = vec_num + i + 1;
  }
//
//  Starting with the lowest numbered group of line segments,
//  see if any higher numbered groups belong.
//
  seed = 0;
  *string_num = 1;
  string[seed] = *string_num;

  for ( ; ; )
  {
    x1val = p1[0+seed*2];
    x2val = p2[0+seed*2];
    y1val = p1[1+seed*2];
    y2val = p2[1+seed*2];
    jval = order[seed];
    kval = order[seed];

    for ( ; ; )
    {
      match = 0;

      for ( j = 0; j < vec_num; j++ )
      {
        if ( *string_num < string[j] )
        {
          if ( x1val == p1[0+j*2] && y1val == p1[1+j*2] )
          {
            jval = jval - 1;
            order[j] = jval;
            string[j] = *string_num;
            x1val = p2[0+j*2];
            y1val = p2[1+j*2];
            match = match + 1;

            temp = p1[0+j*2];
            p1[0+j*2] = p2[0+j*2];
            p2[0+j*2] = temp;

            temp = p1[1+j*2];
            p1[1+j*2] = p2[1+j*2];
            p2[1+j*2] = temp;
          }
          else if ( x1val == p2[0+j*2] && y1val == p2[1+j*2] )
          {
            jval = jval - 1;
            order[j] = jval;
            string[j] = *string_num;
            x1val = p1[0+j*2];
            y1val = p1[1+j*2];
            match = match + 1;
          }
          else if ( x2val == p1[0+j*2] && y2val == p1[1+j*2] )
          {
            kval = kval + 1;
            order[j] = kval;
            string[j] = *string_num;
            x2val = p2[0+j*2];
            y2val = p2[1+j*2];
            match = match + 1;
          }
          else if ( x2val == p2[0+j*2] && y2val == p2[1+j*2] )
          {
            kval = kval + 1;
            order[j] = kval;
            string[j] = *string_num;
            x2val = p1[0+j*2];
            y2val = p1[1+j*2];
            match = match + 1;

            temp = p1[0+j*2];
            p1[0+j*2] = p2[0+j*2];
            p2[0+j*2] = temp;

            temp = p1[1+j*2];
            p1[1+j*2] = p2[1+j*2];
            p2[1+j*2] = temp;
          }
        }
      }
//
//  If the string has closed on itself, then we don't want to
//  look for any more matches for this string.
//
      if ( x1val == x2val && y1val == y2val )
      {
        break;
      }
//
//  If we made no matches this pass, we're done.
//
      if ( match <= 0 )
      {
        break;
      }
    }
//
//  This string is "exhausted".  Are there any line segments we
//  haven't looked at yet?
//
    seed = 0;

    for ( i = 0; i < vec_num; i++)
    {
      if ( *string_num < string[i] )
      {
        seed = i;
        *string_num = *string_num + 1;
        string[i] = *string_num;
        break;
      }
    }

    if ( seed == 0 )
    {
      break;
    }
  }
//
//  There are no more line segments to look at.  Renumber the
//  isolated segments.
//
//  Question: Can this ever happen?
//
  for ( i = 0; i < vec_num; i++ )
  {
    if ( vec_num < string[i] )
    {
      *string_num = *string_num + 1;
      string[i] = *string_num;
    }
  }
//
//  Now sort the line segments by string and by order of traversal.
//
  i = 0;
  isgn = 0;
  j = 0;

  indx = 0;

  for ( ; ; )
  {
    sort_heap_external ( vec_num, &indx, &i, &j, isgn );

    if ( 0 < indx )
    {
      itemp       = order[i-1];
      order[i-1]  = order[j-1];
      order[j-1]  = itemp;

      itemp       = string[i-1];
      string[i-1] = string[j-1];
      string[j-1] = itemp;

      temp          = p1[0+(i-1)*2];
      p1[0+(i-1)*2] = p1[0+(j-1)*2];
      p1[0+(j-1)*2] = temp;

      temp          = p1[1+(i-1)*2];
      p1[1+(i-1)*2] = p1[1+(j-1)*2];
      p1[1+(j-1)*2] = temp;

      temp          = p2[0+(i-1)*2];
      p2[0+(i-1)*2] = p2[0+(j-1)*2];
      p2[0+(j-1)*2] = temp;

      temp          = p2[1+(i-1)*2];
      p2[1+(i-1)*2] = p2[1+(j-1)*2];
      p2[1+(j-1)*2] = temp;
    }
    else if ( indx < 0 )
    {
      if ( ( string[i-1] < string[j-1] ) ||
           ( string[i-1] == string[j-1] && order[i-1] < order[j-1] ) )
      {
        isgn = -1;
      }
      else
      {
        isgn = +1;
      }
    }
    else if ( indx == 0 )
    {
      break;
    }
  }

  return;
}
//****************************************************************************80

void super_ellipse_points_2d ( double pc[2], double r1, double r2,
  double expo, double psi, int n, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    SUPER_ELLIPSE_POINTS_2D returns N points on a tilted superellipse in 2D.
//
//  Discussion:
//
//    The points are "equally spaced" in the angular sense.  They are
//    not equally spaced along the perimeter.
//
//    The parametric formula of the (untilted) superellipse is:
//
//      X = R1 * cos**EXPO ( THETA )
//      Y = R2 * sin**EXPO ( THETA )
//
//    An implicit form of the (untilted) superellipse is:
//
//      (X/R1)**(2/EXPO) + (Y/R2)**(2/EXPO) = 1
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Martin Gardner,
//    The Mathematical Carnival,
//    Knopf, 1975, pages 240-254.
//
//  Parameters:
//
//    Input, double PC[2], the coordinates of the center of the superellipse.
//
//    Input, double R1, R2, the "radius" of the superellipse in the major
//    and minor axis directions.  A circle has these values equal.
//
//    Input, double EXPO, the exponent of the superellipse.
//    0 = a rectangle;
//    between 0 and 1, a "rounded" rectangle;
//    1.0 = an ellipse;
//    2.0 = a diamond;
//    > 2.0 a pinched shape.
//
//    Input, double PSI, the angle that the major axis of the superellipse
//    makes with the X axis.  A value of 0.0 means that the major and
//    minor axes of the superellipse will be the X and Y coordinate axes.
//
//    Input, int N, the number of points desired.  N must be at least 1.
//
//    Output, double P[2*N], the coordinates of points on the superellipse.
//
{
  double act;
  double ast;
  int i;
  double r8_pi = 3.141592653589793;
  double sct;
  double sst;
  double theta;

  for ( i = 0; i < n; i++ )
  {
    theta = ( 2.0 * r8_pi * ( double ) ( i ) ) / ( double ) ( n );

    act = fabs ( cos ( theta ) );
    sct = r8_sign ( cos ( theta ) );
    ast = fabs ( sin ( theta ) );
    sst = r8_sign ( sin ( theta ) );

    p[0+i*2] = pc[0] + r1 * cos ( psi ) * sct * pow ( act, expo )
                     - r2 * sin ( psi ) * sst * pow ( ast, expo );

    p[1+i*2] = pc[1] + r1 * sin ( psi ) * sct * pow ( act, expo )
                     + r2 * cos ( psi ) * sst * pow ( ast, expo );
  }

  return;
}
//****************************************************************************80

double *tetrahedron_barycentric_3d ( double tetra[3*4], double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    tetrahedron_barycentric_3d() returns the barycentric coordinates of a point in 3D.
//
//  Discussion:
//
//    The barycentric coordinates of a point P with respect to
//    a tetrahedron are a set of four values C(1:4), each associated
//    with a vertex of the tetrahedron.  The values must sum to 1.
//    If all the values are between 0 and 1, the point is contained
//    within the tetrahedron.
//
//    The barycentric coordinate of point X related to vertex A can be
//    interpreted as the ratio of the volume of the tetrahedron with
//    vertex A replaced by vertex X to the volume of the original
//    tetrahedron.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double TETRA[3*4], the vertices of the tetrahedron.
//
//    Input, double P[3], the point to be checked.
//
//    Output, double C[4], the barycentric coordinates of the point with
//    respect to the tetrahedron.
//
{
# define N 3
# define RHS_NUM 1

  double a[N*(N+RHS_NUM)];
  double *c;
  int info;
//
//  Set up the linear system
//
//    ( X2-X1  X3-X1  X4-X1 ) C1    X - X1
//    ( Y2-Y1  Y3-Y1  Y4-Y1 ) C2  = Y - Y1
//    ( Z2-Z1  Z3-Z1  Z4-Z1 ) C3    Z - Z1
//
//  which is satisfied by the barycentric coordinates.
//

  a[0+0*N] = tetra[0+1*3] - tetra[0+0*3];
  a[1+0*N] = tetra[1+1*3] - tetra[1+0*3];
  a[2+0*N] = tetra[2+1*3] - tetra[2+0*3];

  a[0+1*N] = tetra[0+2*3] - tetra[0+0*3];
  a[1+1*N] = tetra[1+2*3] - tetra[1+0*3];
  a[2+1*N] = tetra[2+2*3] - tetra[2+0*3];

  a[0+2*N] = tetra[0+3*3] - tetra[0+0*3];
  a[1+2*N] = tetra[1+3*3] - tetra[1+0*3];
  a[2+2*N] = tetra[2+3*3] - tetra[2+0*3];

  a[0+3*N] = p[0]         - tetra[0+0*3];
  a[1+3*N] = p[1]         - tetra[1+0*3];
  a[2+3*N] = p[2]         - tetra[2+0*3];
//
//  Solve the linear system.
//
  info = r8mat_solve ( N, RHS_NUM, a );

  if ( info != 0 )
  {
    cerr << "\n";
    cerr << "TETRAHEDRON_BARYCENTRIC_3D - Fatal error!\n";
    cerr << "  The linear system is singular.\n";
    cerr << "  The input data does not form a proper tetrahedron.\n";
    exit ( 1 );
  }

  c = new double[4];

  c[1] = a[0+3*N];
  c[2] = a[1+3*N];
  c[3] = a[2+3*N];

  c[0] = 1.0 - c[1] - c[2] - c[3];

  return c;
# undef N
# undef RHS_NUM
}
//****************************************************************************80

double *tetrahedron_centroid_3d ( double tetra[3*4] )

//****************************************************************************80
//
//  Purpose:
//
//    tetrahedron_centroid_3d() computes the centroid of a tetrahedron in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double TETRA[3*4], the vertices of the tetrahedron.
//
//    Output, double TETRAHEDRON_CENTROID_3D[3], the coordinates of the centroid.
//
{
# define DIM_NUM 3

  double *centroid;

  centroid = new double[3];

  centroid[0] = 0.25 * ( tetra[0+0*DIM_NUM] + tetra[0+1*DIM_NUM]
                       + tetra[0+2*DIM_NUM] + tetra[0+3*DIM_NUM] );
  centroid[1] = 0.25 * ( tetra[1+0*DIM_NUM] + tetra[1+1*DIM_NUM]
                       + tetra[1+2*DIM_NUM] + tetra[1+3*DIM_NUM] );
  centroid[2] = 0.25 * ( tetra[2+0*DIM_NUM] + tetra[2+1*DIM_NUM]
                       + tetra[2+2*DIM_NUM] + tetra[2+3*DIM_NUM] );

  return centroid;
# undef DIM_NUM
}
//****************************************************************************80

void tetrahedron_circumsphere_3d ( double tetra[3*4], double *r, double pc[3] )

//****************************************************************************80
//
//  Purpose:
//
//    tetrahedron_circumsphere_3d() computes the circumsphere of a tetrahedron in 3D.
//
//  Discussion:
//
//    The circumsphere, or circumscribed sphere, of a tetrahedron is the sphere that
//    passes through the four vertices.  The circumsphere is not necessarily
//    the smallest sphere that contains the tetrahedron.
//
//    Surprisingly, the diameter of the sphere can be found by solving
//    a 3 by 3 linear system.  This is because the vectors P2 - P1,
//    P3 - P1 and P4 - P1 are secants of the sphere, and each forms a
//    right triangle with the diameter through P1.  Hence, the dot product of
//    P2 - P1 with that diameter is equal to the square of the length
//    of P2 - P1, and similarly for P3 - P1 and P4 - P1.  This determines
//    the diameter vector originating at P1, and hence the radius and
//    center.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double TETRA[3*4], the vertices of the tetrahedron.
//
//    Output, double *R, PC[3], the coordinates of the center of the
//    circumscribed sphere, and its radius.  If the linear system is
//    singular, then R = -1, PC[] = 0.
//
{
# define DIM_NUM 3
# define RHS_NUM 1

  double a[DIM_NUM*(DIM_NUM+RHS_NUM)];
  int info;
//
//  Set up the linear system.
//
  a[0+0*3] = tetra[0+1*3] - tetra[0+0*3];
  a[0+1*3] = tetra[1+1*3] - tetra[1+0*3];
  a[0+2*3] = tetra[2+1*3] - tetra[2+0*3];
  a[0+3*3] = pow ( tetra[0+1*3] - tetra[0+0*3], 2 )
           + pow ( tetra[1+1*3] - tetra[1+0*3], 2 )
           + pow ( tetra[2+1*3] - tetra[2+0*3], 2 );

  a[1+0*3] = tetra[0+2*3] - tetra[0+0*3];
  a[1+1*3] = tetra[1+2*3] - tetra[1+0*3];
  a[1+2*3] = tetra[2+2*3] - tetra[2+0*3];
  a[1+3*3] = pow ( tetra[0+2*3] - tetra[0+0*3], 2 )
           + pow ( tetra[1+2*3] - tetra[1+0*3], 2 )
           + pow ( tetra[2+2*3] - tetra[2+0*3], 2 );

  a[2+0*3] = tetra[0+3*3] - tetra[0+0*3];
  a[2+1*3] = tetra[1+3*3] - tetra[1+0*3];
  a[2+2*3] = tetra[2+3*3] - tetra[2+0*3];
  a[2+3*3] = pow ( tetra[0+3*3] - tetra[0+0*3], 2 )
           + pow ( tetra[1+3*3] - tetra[1+0*3], 2 )
           + pow ( tetra[2+3*3] - tetra[2+0*3], 2 );
//
//  Solve the linear system.
//
  info = r8mat_solve ( DIM_NUM, RHS_NUM, a );
//
//  If the system was singular, return a consolation prize.
//
  if ( info != 0 )
  {
    *r = -1.0;
    r8vec_zero ( DIM_NUM, pc );
    return;
  }
//
//  Compute the radius and center.
//
  *r = 0.5 * sqrt
    ( a[0+3*3] * a[0+3*3]
    + a[1+3*3] * a[1+3*3]
    + a[2+3*3] * a[2+3*3] );

  pc[0] = tetra[0+0*3] + 0.5 * a[0+3*3];
  pc[1] = tetra[1+0*3] + 0.5 * a[1+3*3];
  pc[2] = tetra[2+0*3] + 0.5 * a[2+3*3];

  return;
# undef DIM_NUM
# undef RHS_NUM
}
//****************************************************************************80

double tetrahedron_volume_3d ( double tetra[3*4] )

//****************************************************************************80
//
//  Purpose:
//
//    tetrahedron_volume_3d() computes the volume of a tetrahedron in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double TETRA[3*4], the vertices of the tetrahedron.
//
//    Output, double TETRAHEDRON_VOLUME_3D, the volume of the tetrahedron.
//
{
  double a[4*4];
  int i;
  int j;
  double volume;

  for ( i = 0; i < 3; i++ )
  {
    for ( j = 0; j < 4; j++ )
    {
      a[i+j*4] = tetra[i+j*3];
    }
  }

  i = 3;
  for ( j = 0; j < 4; j++ )
  {
    a[i+j*4] = 1.0;
  }

  volume = fabs ( r8mat_det_4d ( a ) ) / 6.0;

  return volume;
}
//****************************************************************************80

void timestamp ( )

//****************************************************************************80
//
//  Purpose:
//
//    timestamp() prints the current YMDHMS date as a time stamp.
//
//  Example:
//
//    31 May 2001 09:45:54 AM
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 July 2009
//
//  Author:
//
//    John Burkardt
//
{
# define TIME_SIZE 40

  static char time_buffer[TIME_SIZE];
  const struct std::tm *tm_ptr;
  std::time_t now;

  now = std::time ( NULL );
  tm_ptr = std::localtime ( &now );

  std::strftime ( time_buffer, TIME_SIZE, "%d %B %Y %I:%M:%S %p", tm_ptr );

  std::cout << time_buffer << "\n";

  return;
# undef TIME_SIZE
}
//****************************************************************************80

void tmat_init ( double a[4*4] )

//****************************************************************************80
//
//  Purpose:
//
//    TMAT_INIT initializes the geometric transformation matrix.
//
//  Discussion:
//
//    The geometric transformation matrix can be thought of as a 4 by 4
//    matrix "A" having components:
//
//      r11 r12 r13 t1
//      r21 r22 r23 t2
//      r31 r32 r33 t3
//        0   0   0  1
//
//    This matrix encodes the rotations, scalings and translations that
//    are applied to graphical objects.
//
//    A point P = (x,y,z) is rewritten in "homogeneous coordinates" as
//    PH = (x,y,z,1).  Then to apply the transformations encoded in A to
//    the point P, we simply compute A * PH.
//
//    Individual transformations, such as a scaling, can be represented
//    by simple versions of the transformation matrix.  If the matrix
//    A represents the current set of transformations, and we wish to
//    apply a new transformation B, { the original points are
//    transformed twice:  B * ( A * PH ).  The new transformation B can
//    be combined with the original one A, to give a single matrix C that
//    encodes both transformations: C = B * A.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    James Foley, Andries vanDam, Steven Feiner, John Hughes,
//    Computer Graphics, Principles and Practice,
//    Second Edition,
//    Addison Wesley, 1990.
//
//  Parameters:
//
//    Input, double A[4*4], the geometric transformation matrix.
//
{
  int i;
  int j;

  for ( i = 0; i < 4; i++ )
  {
    for ( j = 0; j < 4; j++ )
    {
      if ( i == j )
      {
        a[i+j*4] = 1.0;
      }
      else
      {
        a[i+j*4] = 0.0;
      }
    }
  }
  return;
}
//****************************************************************************80

void tmat_mxm ( double a[4*4], double b[4*4], double c[4*4] )

//****************************************************************************80
//
//  Purpose:
//
//    TMAT_MXM multiplies two geometric transformation matrices.
//
//  Discussion:
//
//    The product is accumulated in a temporary array, and { assigned
//    to the result.  Therefore, it is legal for any two, or all three,
//    of the arguments to share memory.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 October 1998
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    James Foley, Andries vanDam, Steven Feiner, John Hughes,
//    Computer Graphics, Principles and Practice,
//    Second Edition,
//    Addison Wesley, 1990.
//
//  Parameters:
//
//    Input, double A[4*4], the first geometric transformation matrix.
//
//    Input, double B[4*4], the second geometric transformation matrix.
//
//    Output, double C[4*4], the product A * B.
//
{
  double d[4*4];
  int i;
  int j;
  int k;

  for ( i = 0; i < 4; i++ )
  {
    for ( k = 0; k < 4; k++ )
    {
      d[i+k*4] = 0.0;
      for ( j = 0; j < 4; j++ )
      {
        d[i+k*4] = d[i+k*4] + a[i+j*4] * b[j+k*4];
      }
    }
  }

  for ( i = 0; i < 4; i++ )
  {
    for ( j = 0; j < 4; j++ )
    {
      c[i+j*4] = d[i+j*4];
    }
  }
  return;
}
//****************************************************************************80

void tmat_mxp ( double a[4*4], double x[4], double y[4] )

//****************************************************************************80
//
//  Purpose:
//
//    TMAT_MXP multiplies a geometric transformation matrix times a point.
//
//  Discussion:
//
//    The matrix will normally have the form
//
//      xx xy xz tx
//      yx yy yz ty
//      zx zy zz tz
//       0  0  0  1
//
//    where the 3x3 initial block controls rotations and scalings,
//    and the values [ tx, ty, tz ] implement a translation.
//
//    The matrix is stored as a vector, by COLUMNS.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    James Foley, Andries vanDam, Steven Feiner, John Hughes,
//    Computer Graphics, Principles and Practice,
//    Second Edition,
//    Addison Wesley, 1990.
//
//  Parameters:
//
//    Input, double A[4*4], the geometric transformation matrix.
//
//    Input, double X[3], the point to be multiplied.  There is a
//    "theoretical" fourth component of X, which can be assumed to
//    equal 1.
//
//    Output, double Y[3], the result of A*X.  The product is accumulated in
//    a temporary vector, and assigned to the result.  Therefore, it
//    is legal for X and Y to share memory.
//
{
  int i;
  int j;
  double z[3];

  for ( i = 0; i < 3; i++ )
  {
    z[i] = a[i+3*4];
    for ( j = 0; j < 3; j++ )
    {
      z[i] = z[i] + a[i+j*4] * x[j];
    }
  }

  for ( i = 0; i < 3; i++ )
  {
    y[i] = z[i];
  }
  return;
}
//****************************************************************************80

void tmat_mxp2 ( double a[4*4], double p1[], double p2[], int n )

//****************************************************************************80
//
//  Purpose:
//
//    TMAT_MXP2 multiplies a geometric transformation matrix times N points.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    James Foley, Andries vanDam, Steven Feiner, John Hughes,
//    Computer Graphics, Principles and Practice,
//    Second Edition,
//    Addison Wesley, 1990.
//
//  Parameters:
//
//    Input, double A[4*4], the geometric transformation matrix.
//
//    Input, double P1[3*N], the points to be multiplied.
//
//    Output, double P2[3*N], the transformed points.  Each product is
//    accumulated in a temporary vector, and assigned to the
//    result.  Therefore, it is legal for X and Y to share memory.
//
{
  int i;
  int j;
  int k;

  for ( k = 0; k < n; k++ )
  {
    for ( i = 0; i < 3; i++ )
    {
      p2[i+k*3] = a[i+3*4];
      for ( j = 0; j < 3; j++ )
      {
        p2[i+k*3] = p2[i+k*3] + a[i+j*4] * p1[j+k*3];
      }
    }
  }
  return;
}
//****************************************************************************80

void tmat_mxv ( double a[4*4], double x[4], double y[4] )

//****************************************************************************80
//
//  Purpose:
//
//    TMAT_MXV multiplies a geometric transformation matrix times a vector.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    James Foley, Andries vanDam, Steven Feiner, John Hughes,
//    Computer Graphics, Principles and Practice,
//    Second Edition,
//    Addison Wesley, 1990.
//
//  Parameters:
//
//    Input, double A[4*4], the geometric transformation matrix.
//
//    Input, double X[4], the vector to be multiplied.  The fourth component
//    of X is implicitly assigned the value of 1.
//
//    Output, double Y[4], the result of A*X.  The product is accumulated in
//    a temporary vector, and assigned to the result.  Therefore, it
//    is legal for X and Y to share memory.
//
{
  int i;
  int j;
  double z[4];

  for ( i = 0; i < 3; i++ )
  {
    z[i] = 0.0;
    for ( j = 0; j < 3; j++ )
    {
      z[i] = z[i] + a[i+j*4] * x[j];
    }
    z[i] = z[i] + a[i+3*4];
  }

  for ( i = 0; i < 3; i++ )
  {
    y[i] = z[i];
  }
  return;
}
//****************************************************************************80

void tmat_rot_axis ( double a[4*4], double b[4*4], double angle,
  char axis )

//****************************************************************************80
//
//  Purpose:
//
//    TMAT_ROT_AXIS applies an axis rotation to the geometric transformation matrix.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    James Foley, Andries vanDam, Steven Feiner, John Hughes,
//    Computer Graphics, Principles and Practice,
//    Second Edition,
//    Addison Wesley, 1990.
//
//  Parameters:
//
//    Input, double A[4*4], the current geometric transformation matrix.
//
//    Output, double B[4*4], the modified geometric transformation matrix.
//    A and B may share the same memory.
//
//    Input, double ANGLE, the angle, in degrees, of the rotation.
//
//    Input, character AXIS, is 'X', 'Y' or 'Z', specifying the coordinate
//    axis about which the rotation occurs.
//
{
  double c[4*4];
  double d[4*4];
  int i;
  int j;
  double theta;

  theta = degrees_to_radians ( angle );

  tmat_init ( c );

  if ( axis == 'X' || axis == 'x' )
  {
    c[1+1*4] =   cos ( theta );
    c[1+2*4] = - sin ( theta );
    c[2+1*4] =   sin ( theta );
    c[2+2*4] =   cos ( theta );
  }
  else if ( axis == 'Y' || axis == 'y' )
  {
    c[0+0*4] =   cos ( theta );
    c[0+2*4] =   sin ( theta );
    c[2+0*4] = - sin ( theta );
    c[2+2*4] =   cos ( theta );
  }
  else if ( axis == 'Z' || axis == 'z' )
  {
    c[0+0*4] =   cos ( theta );
    c[0+1*4] = - sin ( theta );
    c[1+0*4] =   sin ( theta );
    c[1+1*4] =   cos ( theta );
  }
  else
  {
    cerr << "\n";
    cerr << "TMAT_ROT_AXIS - Fatal error!\n";
    cerr << "  Illegal rotation axis: " << axis <<"\n";
    cerr << "  Legal choices are 'X', 'Y', or 'Z'.\n";
    exit ( 1 );
  }

  tmat_mxm ( c, a, d );

  for ( i = 0; i < 4; i++ )
  {
    for ( j = 0; j < 4; j++ )
    {
      b[i+j*4] = d[i+j*4];
    }
  }
  return;
}
//****************************************************************************80

void tmat_rot_vector ( double a[4*4], double b[4*4], double angle,
  double v[3] )

//****************************************************************************80
//
//  Purpose:
//
//    TMAT_ROT_VECTOR applies a rotation about a vector to the geometric transformation matrix.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    James Foley, Andries vanDam, Steven Feiner, John Hughes,
//    Computer Graphics, Principles and Practice,
//    Second Edition,
//    Addison Wesley, 1990.
//
//  Parameters:
//
//    Input, double A[4*4], the current geometric transformation matrix.
//
//    Output, double B[4*4], the modified geometric transformation matrix.
//    A and B may share the same memory.
//
//    Input, double ANGLE, the angle, in degrees, of the rotation.
//
//    Input, double V[3], the coordinates of a (nonzero)
//    point defining a vector from the origin.  The rotation will occur
//    about this axis.
//
{
  double c[4*4];
  double ca;
  double d[4*4];
  int i;
  int j;
  double sa;
  double theta;

  if ( pow ( v[0], 2 ) + pow ( v[1], 2 ) + pow ( v[2], 2 ) == 0.0 )
  {
    return;
  }

  theta = degrees_to_radians ( angle );

  tmat_init ( c );

  ca = cos ( theta );
  sa = sin ( theta );

  c[0+0*4] =                v[0] * v[0] + ca * ( 1.0 - v[0] * v[0] );
  c[0+1*4] = ( 1.0 - ca ) * v[0] * v[1] - sa * v[2];
  c[0+2*4] = ( 1.0 - ca ) * v[0] * v[2] + sa * v[1];

  c[1+0*4] = ( 1.0 - ca ) * v[1] * v[0] + sa * v[2];
  c[1+1*4] =                v[1] * v[1] + ca * ( 1.0 - v[1] * v[1] );
  c[1+2*4] = ( 1.0 - ca ) * v[1] * v[2] - sa * v[0];

  c[2+0*4] = ( 1.0 - ca ) * v[2] * v[0] - sa * v[1];
  c[2+1*4] = ( 1.0 - ca ) * v[2] * v[1] + sa * v[0];
  c[2+2*4] =                v[2] * v[2] + ca * ( 1.0 - v[2] * v[2] );

  tmat_mxm ( c, a, d );

  for ( i = 0; i < 4; i++ )
  {
    for ( j = 0; j < 4; j++ )
    {
      b[i+j*4] = d[i+j*4];
    }
  }
  return;
}
//****************************************************************************80

void tmat_scale ( double a[4*4], double b[4*4], double s[3] )

//****************************************************************************80
//
//  Purpose:
//
//    TMAT_SCALE applies a scaling to the geometric transformation matrix.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    James Foley, Andries vanDam, Steven Feiner, John Hughes,
//    Computer Graphics, Principles and Practice,
//    Second Edition,
//    Addison Wesley, 1990.
//
//  Parameters:
//
//    Input, double A[4*4], the current geometric transformation matrix.
//
//    Output, double B[4*4], the modified geometric transformation matrix.
//    A and B may share the same memory.
//
//    Input, double S[3], the scalings to be applied to the coordinates.
//
{
  double c[4*4];
  double d[4*4];
  int i;
  int j;

  tmat_init ( c );

  c[0+0*4] = s[0];
  c[1+1*4] = s[1];
  c[2+2*4] = s[2];

  tmat_mxm ( c, a, d );

  for ( i = 0; i < 4; i++ )
  {
    for ( j = 0; j < 4; j++ )
    {
      b[i+j*4] = d[i+j*4];
    }
  }
  return;
}
//****************************************************************************80

void tmat_shear ( double a[4*4], double b[4*4], string axis, double s )

//****************************************************************************80
//
//  Purpose:
//
//    TMAT_SHEAR applies a shear to the geometric transformation matrix.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    James Foley, Andries vanDam, Steven Feiner, John Hughes,
//    Computer Graphics, Principles and Practice,
//    Second Edition,
//    Addison Wesley, 1990.
//
//  Parameters:
//
//    Input, double A[4*4], the current geometric transformation matrix.
//
//    Output, double B[4*4], the modified geometric transformation matrix.
//    A and B may share the same memory.
//
//    Input, string AXIS, is 'XY', 'XZ', 'YX', 'YZ', 'ZX' or 'ZY',
//    specifying the shear equation:
//
//      XY:  x' = x + s * y;
//      XZ:  x' = x + s * z;
//      YX:  y' = y + s * x;
//      YZ:  y' = y + s * z;
//      ZX:  z' = z + s * x;
//      ZY:  z' = z + s * y.
//
//    Input, double S, the shear coefficient.
//
{
  double c[4*4];
  double d[4*4];
  int i;
  int j;

  tmat_init ( c );

  if ( axis == "XY" || axis == "xy" )
  {
    c[0+1*4] = s;
  }
  else if ( axis == "XZ" || axis == "xz" )
  {
    c[0+2*4] = s;
  }
  else if ( axis == "YX" || axis == "yx" )
  {
    c[1+0*4] = s;
  }
  else if ( axis == "YZ" || axis == "yz" )
  {
    c[1+2*4] = s;
  }
  else if ( axis == "ZX" || axis == "zx" )
  {
    c[2+0*4] = s;
  }
  else if ( axis == "ZY" || axis == "zy" )
  {
    c[2+1*4] = s;
  }
  else
  {
    cerr << "\n";
    cerr << "TMAT_SHEAR - Fatal error!\n";
    cerr << "  Illegal shear axis: " << axis << "\n";
    cerr << "  Legal choices are XY, XZ, YX, YZ, ZX, or ZY.\n";
    exit ( 1 );
  }

  tmat_mxm ( c, a, d );

  for ( i = 0; i < 4; i++ )
  {
    for ( j = 0; j < 4; j++ )
    {
      b[i+j*4] = d[i+j*4];
    }
  }
  return;
}
//****************************************************************************80

void tmat_trans ( double a[4*4], double b[4*4], double v[3] )

//****************************************************************************80
//
//  Purpose:
//
//    TMAT_TRANS applies a translation to the geometric transformation matrix.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    James Foley, Andries vanDam, Steven Feiner, John Hughes,
//    Computer Graphics, Principles and Practice,
//    Second Edition,
//    Addison Wesley, 1990.
//
//  Parameters:
//
//    Input, double A[4*4], the current geometric transformation matrix.
//
//    Output, double B[4*4], the modified transformation matrix.
//    A and B may share the same memory.
//
//    Input, double V[3], the translation.  This may be thought of as the
//    point that the origin moves to under the translation.
//
{
  int i;
  int j;

  for ( i = 0; i < 4; i++ )
  {
    for ( j = 0; j < 4; j++ )
    {
      b[i+j*4] = a[i+j*4];
    }
  }
  b[0+3*4] = b[0+3*4] + v[0];
  b[1+3*4] = b[1+3*4] + v[1];
  b[2+3*4] = b[2+3*4] + v[2];

  return;
}
//****************************************************************************80

double torus_area_3d ( double r1, double r2 )

//****************************************************************************80
//
//  Purpose:
//
//    TORUS_AREA_3D returns the area of a torus in 3D.
//
//  Discussion:
//
//    A torus with radii R1 and R2 is the set of points satisfying:
//
//      ( sqrt ( X^2 + Y^2 ) - R1 )^2 + Z^2 <= R2^2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R1, R2, the two radii that define the torus.
//
//    Output, double TORUS_AREA_3D, the area of the torus.
//
{
  double area;
  double r8_pi = 3.141592653589793;

  area = 4.0 * r8_pi * r8_pi * r1 * r2;

  return area;
}
//****************************************************************************80

double torus_volume_3d ( double r1, double r2 )

//****************************************************************************80
//
//  Purpose:
//
//    TORUS_VOLUME_3D computes the volume of a torus in 3D.
//
//  Discussion:
//
//    A torus with radii R1 and R2 is the set of points satisfying:
//
//      ( sqrt ( X^2 + Y^2 ) - R1 )^2 + Z^2 <= R2^2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double R1, R2, the "inner" and "outer" radii of the torus.
//
//    Output, double TORUS_VOLUME_3D, the volume of the torus.
//
{
  double r8_pi = 3.141592653589793;
  double volume;

  volume = 2.0 * r8_pi * r8_pi * r1 * r2 * r2;

  return volume;
}
//****************************************************************************80

double *tp_to_xyz ( double theta, double phi )

//****************************************************************************80
//
//  Purpose:
//
//    TP_TO_XYZ converts unit spherical TP coordinates to XYZ coordinates.
//
//  Discussion:
//
//    The point is assume to lie on the unit sphere centered at the origin.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 September 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double THETA, PHI, the angular coordinates of a point
//    on the unit sphere.
//
//    Output, double TP_TO_XYZ[3], the XYZ coordinates.
//
{
  double *v;

  v = new double[3];

  v[0] = cos ( theta ) * sin ( phi );
  v[1] = sin ( theta ) * sin ( phi );
  v[2] =                 cos ( phi );

  return v;
}
//****************************************************************************80

void triangle_angles_2d ( double t[2*3], double angle[3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_ANGLES_2D computes the angles of a triangle in 2D.
//
//  Discussion:
//
//    The law of cosines is used:
//
//      C * C = A * A + B * B - 2 * A * B * COS ( GAMMA )
//
//    where GAMMA is the angle opposite side C.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double ANGLE[3], the angles opposite
//    sides P1-P2, P2-P3 and P3-P1, in radians.
//
{
  double a;
  double b;
  double c;
  double r8_pi = 3.141592653589793;

  a = sqrt ( pow ( t[0+1*2] - t[0+0*2], 2 )
           + pow ( t[1+1*2] - t[1+0*2], 2 ) );

  b = sqrt ( pow ( t[0+2*2] - t[0+1*2], 2 )
           + pow ( t[1+2*2] - t[1+1*2], 2 ) );

  c = sqrt ( pow ( t[0+0*2] - t[0+2*2], 2 )
           + pow ( t[1+0*2] - t[1+2*2], 2 ) );
//
//  Take care of a ridiculous special case.
//
  if ( a == 0.0 && b == 0.0 && c == 0.0 )
  {
    angle[0] = 2.0 * r8_pi / 3.0;
    angle[1] = 2.0 * r8_pi / 3.0;
    angle[2] = 2.0 * r8_pi / 3.0;
    return;
  }

  if ( c == 0.0 || a == 0.0 )
  {
    angle[0] = r8_pi;
  }
  else
  {
    angle[0] = r8_acos ( ( c * c + a * a - b * b ) / ( 2.0 * c * a ) );
  }

  if ( a == 0.0 || b == 0.0 )
  {
    angle[1] = r8_pi;
  }
  else
  {
    angle[1] = r8_acos ( ( a * a + b * b - c * c ) / ( 2.0 * a * b ) );
  }

  if ( b == 0.0 || c == 0.0 )
  {
    angle[2] = r8_pi;
  }
  else
  {
    angle[2] = r8_acos ( ( b * b + c * c - a * a ) / ( 2.0 * b * c ) );
  }

  return;
}
//****************************************************************************80

double *triangle_angles_2d_new ( double t[2*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_ANGLES_2D_NEW computes the angles of a triangle in 2D.
//
//  Discussion:
//
//    The law of cosines is used:
//
//      C * C = A * A + B * B - 2 * A * B * COS ( GAMMA )
//
//    where GAMMA is the angle opposite side C.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double TRIANGLE_ANGLES_2D_NEW[3], the angles opposite
//    sides P1-P2, P2-P3 and P3-P1, in radians.
//
{
  double a;
  double *angle;
  double b;
  double c;
  double r8_pi = 3.141592653589793;

  angle = new double[3];

  a = sqrt ( pow ( t[0+1*2] - t[0+0*2], 2 )
           + pow ( t[1+1*2] - t[1+0*2], 2 ) );

  b = sqrt ( pow ( t[0+2*2] - t[0+1*2], 2 )
           + pow ( t[1+2*2] - t[1+1*2], 2 ) );

  c = sqrt ( pow ( t[0+0*2] - t[0+2*2], 2 )
           + pow ( t[1+0*2] - t[1+2*2], 2 ) );
//
//  Take care of a ridiculous special case.
//
  if ( a == 0.0 && b == 0.0 && c == 0.0 )
  {
    angle[0] = 2.0 * r8_pi / 3.0;
    angle[1] = 2.0 * r8_pi / 3.0;
    angle[2] = 2.0 * r8_pi / 3.0;
    return angle;
  }

  if ( c == 0.0 || a == 0.0 )
  {
    angle[0] = r8_pi;
  }
  else
  {
    angle[0] = r8_acos ( ( c * c + a * a - b * b ) / ( 2.0 * c * a ) );
  }

  if ( a == 0.0 || b == 0.0 )
  {
    angle[1] = r8_pi;
  }
  else
  {
    angle[1] = r8_acos ( ( a * a + b * b - c * c ) / ( 2.0 * a * b ) );
  }

  if ( b == 0.0 || c == 0.0 )
  {
    angle[2] = r8_pi;
  }
  else
  {
    angle[2] = r8_acos ( ( b * b + c * c - a * a ) / ( 2.0 * b * c ) );
  }

  return angle;
}
//****************************************************************************80

void triangle_angles_3d ( double t[3*3], double angle[3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_ANGLES_3D computes the angles of a triangle in 3D.
//
//  Discussion:
//
//    The law of cosines is used:
//
//      C * C = A * A + B * B - 2 * A * B * COS ( GAMMA )
//
//    where GAMMA is the angle opposite side C.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[3*3], the triangle vertices.
//
//    Output, double ANGLE[3], the angles opposite
//    sides P1-P2, P2-P3 and P3-P1, in radians.
//
{
# define DIM_NUM 3

  double a;
  double b;
  double c;
  double r8_pi = 3.141592653589793;

  a = sqrt ( pow ( t[0+1*DIM_NUM] - t[0+0*DIM_NUM], 2 )
           + pow ( t[1+1*DIM_NUM] - t[1+0*DIM_NUM], 2 )
           + pow ( t[2+1*DIM_NUM] - t[2+0*DIM_NUM], 2 ) );

  b = sqrt ( pow ( t[0+2*DIM_NUM] - t[0+1*DIM_NUM], 2 )
           + pow ( t[1+2*DIM_NUM] - t[1+1*DIM_NUM], 2 )
           + pow ( t[2+2*DIM_NUM] - t[2+1*DIM_NUM], 2 ) );

  c = sqrt ( pow ( t[0+0*DIM_NUM] - t[0+2*DIM_NUM], 2 )
           + pow ( t[1+0*DIM_NUM] - t[1+2*DIM_NUM], 2 )
           + pow ( t[2+0*DIM_NUM] - t[2+2*DIM_NUM], 2 ) );
//
//  Take care of a ridiculous special case.
//
  if ( a == 0.0 && b == 0.0 && c == 0.0 )
  {
    angle[0] = 2.0 * r8_pi / 3.0;
    angle[1] = 2.0 * r8_pi / 3.0;
    angle[2] = 2.0 * r8_pi / 3.0;
    return;
  }

  if ( c == 0.0 || a == 0.0 )
  {
    angle[0] = r8_pi;
  }
  else
  {
    angle[0] = r8_acos ( ( c * c + a * a - b * b ) / ( 2.0 * c * a ) );
  }

  if ( a == 0.0 || b == 0.0 )
  {
    angle[1] = r8_pi;
  }
  else
  {
    angle[1] = r8_acos ( ( a * a + b * b - c * c ) / ( 2.0 * a * b ) );
  }

  if ( b == 0.0 || c == 0.0 )
  {
    angle[2] = r8_pi;
  }
  else
  {
    angle[2] = r8_acos ( ( b * b + c * c - a * a ) / ( 2.0 * b * c ) );
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

double *triangle_angles_3d_new ( double t[3*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_ANGLES_3D_NEW computes the angles of a triangle in 3D.
//
//  Discussion:
//
//    The law of cosines is used:
//
//      C * C = A * A + B * B - 2 * A * B * COS ( GAMMA )
//
//    where GAMMA is the angle opposite side C.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    11 September 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[3*3], the triangle vertices.
//
//    Output, double TRIANGLE_ANGLES_3D_NEW[3], the angles opposite
//    sides P1-P2, P2-P3 and P3-P1, in radians.
//
{
# define DIM_NUM 3

  double a;
  double *angle;
  double b;
  double c;
  double r8_pi = 3.141592653589793;

  angle = new double[3];

  a = sqrt ( pow ( t[0+1*DIM_NUM] - t[0+0*DIM_NUM], 2 )
           + pow ( t[1+1*DIM_NUM] - t[1+0*DIM_NUM], 2 )
           + pow ( t[2+1*DIM_NUM] - t[2+0*DIM_NUM], 2 ) );

  b = sqrt ( pow ( t[0+2*DIM_NUM] - t[0+1*DIM_NUM], 2 )
           + pow ( t[1+2*DIM_NUM] - t[1+1*DIM_NUM], 2 )
           + pow ( t[2+2*DIM_NUM] - t[2+1*DIM_NUM], 2 ) );

  c = sqrt ( pow ( t[0+0*DIM_NUM] - t[0+2*DIM_NUM], 2 )
           + pow ( t[1+0*DIM_NUM] - t[1+2*DIM_NUM], 2 )
           + pow ( t[2+0*DIM_NUM] - t[2+2*DIM_NUM], 2 ) );
//
//  Take care of a ridiculous special case.
//
  if ( a == 0.0 && b == 0.0 && c == 0.0 )
  {
    angle[0] = 2.0 * r8_pi / 3.0;
    angle[1] = 2.0 * r8_pi / 3.0;
    angle[2] = 2.0 * r8_pi / 3.0;
    return angle;
  }

  if ( c == 0.0 || a == 0.0 )
  {
    angle[0] = r8_pi;
  }
  else
  {
    angle[0] = r8_acos ( ( c * c + a * a - b * b ) / ( 2.0 * c * a ) );
  }

  if ( a == 0.0 || b == 0.0 )
  {
    angle[1] = r8_pi;
  }
  else
  {
    angle[1] = r8_acos ( ( a * a + b * b - c * c ) / ( 2.0 * a * b ) );
  }

  if ( b == 0.0 || c == 0.0 )
  {
    angle[2] = r8_pi;
  }
  else
  {
    angle[2] = r8_acos ( ( b * b + c * c - a * a ) / ( 2.0 * b * c ) );
  }

  return angle;
# undef DIM_NUM
}
//****************************************************************************80

double triangle_area_2d ( double t[2*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_AREA_2D computes the area of a triangle in 2D.
//
//  Discussion:
//
//    If the triangle's vertices are given in counter clockwise order,
//    the area will be positive.  If the triangle's vertices are given
//    in clockwise order, the area will be negative!
//
//    An earlier version of this routine always returned the absolute
//    value of the computed area.  I am convinced now that that is
//    a less useful result!  For instance, by returning the signed
//    area of a triangle, it is possible to easily compute the area
//    of a nonconvex polygon as the sum of the (possibly negative)
//    areas of triangles formed by node 1 and successive pairs of vertices.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the vertices of the triangle.
//
//    Output, double TRIANGLE_AREA_2D, the area of the triangle.
//
{
  double area;

  area = 0.5 * (
    t[0+0*2] * ( t[1+1*2] - t[1+2*2] ) +
    t[0+1*2] * ( t[1+2*2] - t[1+0*2] ) +
    t[0+2*2] * ( t[1+0*2] - t[1+1*2] ) );

  return area;
}
//****************************************************************************80

double triangle_area_3d ( double t[3*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_AREA_3D computes the area of a triangle in 3D.
//
//  Discussion:
//
//    This routine uses the fact that the norm of the cross product vector
//    is the area of the parallelogram they form.  The triangle they
//    form has half that area.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double T[3*3], the vertices of the triangle.
//
//    Output, double TRIANGLE_AREA_3D, the area of the triangle.
//
{
# define DIM_NUM 3

  double area;
  double *cross;
  int i;
//
//  Compute the cross product vector.
//
  cross = new double[DIM_NUM];

  cross[0] = ( t[1+1*DIM_NUM] - t[1+0*DIM_NUM] )
           * ( t[2+2*DIM_NUM] - t[2+0*DIM_NUM] )
           - ( t[2+1*DIM_NUM] - t[2+0*DIM_NUM] )
           * ( t[1+2*DIM_NUM] - t[1+0*DIM_NUM] );

  cross[1] = ( t[2+1*DIM_NUM] - t[2+0*DIM_NUM] )
           * ( t[0+2*DIM_NUM] - t[0+0*DIM_NUM] )
           - ( t[0+1*DIM_NUM] - t[0+0*DIM_NUM] )
           * ( t[2+2*DIM_NUM] - t[2+0*DIM_NUM] );

  cross[2] = ( t[0+1*DIM_NUM] - t[0+0*DIM_NUM] )
           * ( t[1+2*DIM_NUM] - t[1+0*DIM_NUM] )
           - ( t[1+1*DIM_NUM] - t[1+0*DIM_NUM] )
           * ( t[0+2*DIM_NUM] - t[0+0*DIM_NUM] );

  area = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    area = area + pow ( cross[i], 2 );
  }

  area = 0.5 * sqrt ( area );

  delete [] cross;

  return area;
# undef DIM_NUM
}
//****************************************************************************80

double triangle_area_3d_2 ( double t[3*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_AREA_3D_2 computes the area of a triangle in 3D.
//
//  Discussion:
//
//    This routine computes the area "the hard way".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[3*3], the vertices of the triangle.
//
//    Output, double TRIANGLE_AREA_3D_2, the area of the triangle.
//
{
# define DIM_NUM 3

  double alpha;
  double area;
  double base;
  double dot;
  double height;
  double ph[DIM_NUM];
//
//  Find the projection of (P3-P1) onto (P2-P1).
//
  dot = ( t[0+1*DIM_NUM] - t[0+0*DIM_NUM] )
      * ( t[0+2*DIM_NUM] - t[0+0*DIM_NUM] )
      + ( t[1+1*DIM_NUM] - t[1+0*DIM_NUM] )
      * ( t[1+2*DIM_NUM] - t[1+0*DIM_NUM] )
      + ( t[2+1*DIM_NUM] - t[2+0*DIM_NUM] )
      * ( t[2+2*DIM_NUM] - t[2+0*DIM_NUM] );

  base = sqrt ( pow ( t[0+1*DIM_NUM] - t[0+0*DIM_NUM], 2 )
              + pow ( t[1+1*DIM_NUM] - t[1+0*DIM_NUM], 2 )
              + pow ( t[2+1*DIM_NUM] - t[2+0*DIM_NUM], 2 ) );
//
//  The height of the triangle is the length of (P3-P1) after its
//  projection onto (P2-P1) has been subtracted.
//
  if ( base == 0.0 )
  {
    height = 0.0;
  }
  else
  {
    alpha = dot / ( base * base );

    ph[0] = t[0+0*DIM_NUM] + alpha * ( t[0+1*DIM_NUM] - t[0+0*DIM_NUM] );
    ph[1] = t[1+0*DIM_NUM] + alpha * ( t[1+1*DIM_NUM] - t[1+0*DIM_NUM] );
    ph[2] = t[2+0*DIM_NUM] + alpha * ( t[2+1*DIM_NUM] - t[2+0*DIM_NUM] ),

    height = sqrt ( pow ( ph[0] - t[0+2*DIM_NUM], 2 )
                  + pow ( ph[1] - t[1+2*DIM_NUM], 2 )
                  + pow ( ph[2] - t[2+2*DIM_NUM], 2 ) );
  }

  area = 0.5 * base * height;

  return area;
# undef DIM_NUM
}
//****************************************************************************80

double triangle_area_3d_3 ( double t[3*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_AREA_3D_3 computes the area of a triangle in 3D.
//
//  Discussion:
//
//    This routine uses Heron's formula
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    31 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double T[3*3], the triangle vertices.
//
//    Output, double AREA, the area of the triangle.
//
{
# define DIM_NUM 3

  double area;
  int i;
  int j;
  int jp1;
  double s[DIM_NUM];

  for ( j = 0; j < DIM_NUM; j++ )
  {
    jp1 = ( j + 1 ) % DIM_NUM;
    s[j] = 0.0;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      s[j] = s[j] + pow ( t[i+j*DIM_NUM] - t[i+jp1*DIM_NUM], 2 );
    }
    s[j] = sqrt ( s[j] );
  }

  area = (   s[0] + s[1] + s[2] )
       * ( - s[0] + s[1] + s[2] )
       * (   s[0] - s[1] + s[2] )
       * (   s[0] + s[1] - s[2] );

  if ( area < 0.0 )
  {
    area = -1.0;
    return area;
  }

  area = 0.25 * sqrt ( area );

  return area;
# undef DIM_NUM
}
//****************************************************************************80

double triangle_area_heron ( double s[3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_AREA_HERON computes the area of a triangle using Heron's formula.
//
//  Discussion:
//
//    The formula is valid for any spatial dimension, depending only
//    on the lengths of the sides, and not the coordinates of the vertices.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    31 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double S[3], the lengths of the three sides.
//
//    Output, double TRIANGLE_AREA_HERON, the area of the triangle, or -1.0 if the
//    sides cannot constitute a triangle.
//
{
  double area;

  area = (   s[0] + s[1] + s[2] )
       * ( - s[0] + s[1] + s[2] )
       * (   s[0] - s[1] + s[2] )
       * (   s[0] + s[1] - s[2] );

  if ( area < 0.0 )
  {
    area = -1.0;
    return area;
  }

  area = 0.25 * sqrt ( area );

  return area;
}
//****************************************************************************80

double *triangle_area_vector_3d ( double t[3*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_AREA_VECTOR_3D computes the area vector of a triangle in 3D.
//
//  Discussion:
//
//    The "area vector" of a triangle is simply a cross product of,
//    for instance, the vectors (V2-V1) and (V3-V1), where V1, V2
//    and V3 are the vertices of the triangle.
//
//    The norm of the cross product vector of two vectors is the area
//    of the parallelogram they form.
//
//    Therefore, the area of the triangle is half of the norm of the
//    area vector:
//
//      area = 0.5 * sqrt ( sum ( area_vector(1:3)^2 ) )
//
//    The reason for looking at the area vector rather than the area
//    is that this makes it possible to compute the area of a flat
//    polygon in 3D by summing the areas of the triangles that form
//    a decomposition of the polygon, while allowing for both positive
//    and negative areas.  (Sum the vectors, THEN take the norm and
//    multiply by 1/2).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    18 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double T[3*3], the vertices of the triangle.
//
//    Output, double TRIANGLE_AREA_VECTOR_3D[3], the area vector of the triangle.
//
{
# define DIM_NUM 3

  double *cross;
//
//  Compute the cross product vector.
//
  cross = new double[DIM_NUM];

  cross[0] = ( t[1+1*DIM_NUM] - t[1+0*DIM_NUM] )
           * ( t[2+2*DIM_NUM] - t[2+0*DIM_NUM] )
           - ( t[2+1*DIM_NUM] - t[2+0*DIM_NUM] )
           * ( t[1+2*DIM_NUM] - t[1+0*DIM_NUM] );

  cross[1] = ( t[2+1*DIM_NUM] - t[2+0*DIM_NUM] )
           * ( t[0+2*DIM_NUM] - t[0+0*DIM_NUM] )
           - ( t[0+1*DIM_NUM] - t[0+0*DIM_NUM] )
           * ( t[2+2*DIM_NUM] - t[2+0*DIM_NUM] );

  cross[2] = ( t[0+1*DIM_NUM] - t[0+0*DIM_NUM] )
           * ( t[1+2*DIM_NUM] - t[1+0*DIM_NUM] )
           - ( t[1+1*DIM_NUM] - t[1+0*DIM_NUM] )
           * ( t[0+2*DIM_NUM] - t[0+0*DIM_NUM] );

  return cross;
# undef DIM_NUM
}
//****************************************************************************80

double *triangle_barycentric_2d ( double t[2*3], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_BARYCENTRIC_2D finds the barycentric coordinates of a point in 2D.
//
//  Discussion:
//
//    The barycentric coordinate of point X related to vertex A can be
//    interpreted as the ratio of the area of the triangle with
//    vertex A replaced by vertex X to the area of the original
//    triangle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the vertices of the triangle.
//
//    Input, double P[2], the point to be checked.
//
//    Output, double C[3], the barycentric coordinates of the point with respect
//    to the triangle.
//
{
# define N 2
# define RHS_NUM 1

  double a[N*(N+RHS_NUM)];
  double *c;
  int info;
//
//  Set up the linear system
//
//    ( X2-X1  X3-X1 ) C1  = X-X1
//    ( Y2-Y1  Y3-Y1 ) C2    Y-Y1
//
//  which is satisfied by the barycentric coordinates.
//
  a[0+0*N] = t[0+1*2] - t[0+0*2];
  a[1+0*N] = t[1+1*2] - t[1+0*2];

  a[0+1*N] = t[0+2*2] - t[0+0*2];
  a[1+1*N] = t[1+2*2] - t[1+0*2];

  a[0+2*N] = p[0]     - t[0+0*2];
  a[1+2*N] = p[1]     - t[1+0*2];
//
//  Solve the linear system.
//
  info = r8mat_solve ( N, RHS_NUM, a );

  if ( info != 0 )
  {
    cerr << "\n";
    cerr << "TRIANGLE_BARYCENTRIC_2D - Fatal error!\n";
    cerr << "  The linear system is singular.\n";
    cerr << "  The input data does not form a proper triangle.\n";
    exit ( 1 );
  }

  c = new double[3];

  c[0] = a[0+2*N];
  c[1] = a[1+2*N];
  c[2] = 1.0 - c[0] - c[1];

  return c;
# undef N
# undef RHS_NUM
}
//****************************************************************************80

double *triangle_centroid_2d ( double t[2*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CENTROID_2D computes the centroid of a triangle in 2D.
//
//  Discussion:
//
//    The centroid of a triangle can also be considered the center
//    of gravity, assuming that the triangle is made of a thin uniform
//    sheet of massy material.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double T[2*3], the vertices of the triangle.
//
//    Output, double TRIANGLE_CENTROID_2D[2], the coordinates of the centroid of the triangle.
//
{
# define DIM_NUM 2

  double *centroid;

  centroid = new double[DIM_NUM];

  centroid[0] = ( t[0+0*DIM_NUM] + t[0+1*DIM_NUM] + t[0+2*DIM_NUM] ) / 3.0;
  centroid[1] = ( t[1+0*DIM_NUM] + t[1+1*DIM_NUM] + t[1+2*DIM_NUM] ) / 3.0;

  return centroid;
# undef DIM_NUM
}
//****************************************************************************80

double *triangle_centroid_3d ( double t[3*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CENTROID_3D computes the centroid of a triangle in 3D.
//
//  Discussion:
//
//    The centroid of a triangle can also be considered the center
//    of gravity, assuming that the triangle is made of a thin uniform
//    sheet of massy material.
//
//    Thanks to Gordon Griesel for pointing out a typographical
//    error in an earlier version of this program, and for pointing
//    out a second oversight, as well.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    21 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[3*3], the vertices of the triangle.
//
//    Output, double TRIANGLE_CENTROID_3D[3], the coordinates of the centroid.
//
{
# define DIM_NUM 3

  double *centroid;

  centroid = new double[DIM_NUM];

  centroid[0] = ( t[0+0*DIM_NUM] + t[0+1*DIM_NUM] + t[0+2*DIM_NUM] ) / 3.0;
  centroid[1] = ( t[1+0*DIM_NUM] + t[1+1*DIM_NUM] + t[1+2*DIM_NUM] ) / 3.0;
  centroid[2] = ( t[2+0*DIM_NUM] + t[2+1*DIM_NUM] + t[2+2*DIM_NUM] ) / 3.0;

  return centroid;
# undef DIM_NUM
}
//****************************************************************************80

double *triangle_circumcenter_2d ( double t[2*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CIRCUMCENTER_2D computes the circumcenter of a triangle in 2D.
//
//  Discussion:
//
//    The circumcenter of a triangle is the center of the circumcircle, the
//    circle that passes through the three vertices of the triangle.
//
//    The circumcircle contains the triangle, but it is not necessarily the
//    smallest triangle to do so.
//
//    If all angles of the triangle are no greater than 90 degrees, then
//    the center of the circumscribed circle will lie inside the triangle.
//    Otherwise, the center will lie outside the triangle.
//
//    The circumcenter is the intersection of the perpendicular bisectors
//    of the sides of the triangle.
//
//    In geometry, the circumcenter of a triangle is often symbolized by "O".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 February 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double *TRIANGLE_CIRCUMCENTER_2D[2], the circumcenter.
//
{
# define DIM_NUM 2

  double asq;
  double bot;
  double *pc;
  double csq;
  double top1;
  double top2;

  pc = new double[DIM_NUM];

  asq = ( t[0+1*2] - t[0+0*2] ) * ( t[0+1*2] - t[0+0*2] )
      + ( t[1+1*2] - t[1+0*2] ) * ( t[1+1*2] - t[1+0*2] );

  csq = ( t[0+2*2] - t[0+0*2] ) * ( t[0+2*2] - t[0+0*2] )
      + ( t[1+2*2] - t[1+0*2] ) * ( t[1+2*2] - t[1+0*2] );

  top1 =   ( t[1+1*2] - t[1+0*2] ) * csq - ( t[1+2*2] - t[1+0*2] ) * asq;
  top2 = - ( t[0+1*2] - t[0+0*2] ) * csq + ( t[0+2*2] - t[0+0*2] ) * asq;

  bot  =  ( t[1+1*2] - t[1+0*2] ) * ( t[0+2*2] - t[0+0*2] )
        - ( t[1+2*2] - t[1+0*2] ) * ( t[0+1*2] - t[0+0*2] );

  pc[0] = t[0+0*2] + 0.5 * top1 / bot;
  pc[1] = t[1+0*2] + 0.5 * top2 / bot;

  return pc;

# undef DIM_NUM
}
//****************************************************************************80

double *triangle_circumcenter_2d_2 ( double t[2*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CIRCUMCENTER_2D_2 computes the circumcenter of a triangle in 2D.
//
//  Discussion:
//
//    The circumcenter of a triangle is the center of the circumcircle, the
//    circle that passes through the three vertices of the triangle.
//
//    The circumcircle contains the triangle, but it is not necessarily the
//    smallest triangle to do so.
//
//    If all angles of the triangle are no greater than 90 degrees, then
//    the center of the circumscribed circle will lie inside the triangle.
//    Otherwise, the center will lie outside the triangle.
//
//    The circumcenter is the intersection of the perpendicular bisectors
//    of the sides of the triangle.
//
//    Surprisingly, the diameter of the circle can be found by solving
//    a 2 by 2 linear system.  If we label the vertices of the triangle
//    P1, P2 and P3, then the vectors P2 - P1 and P3 - P1 are secants of
//    the circle, and each forms a right triangle with the diameter.
//    Hence, the dot product of P2 - P1 with the diameter vector is equal
//    to the square of the length of P2 - P1, and similarly for P3 - P1.
//    This determines the diameter vector originating at P1.
//
//    In geometry, the circumcenter of a triangle is often symbolized by "O".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 August 2003
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double *TRIANGLE_CIRCUMCENTER_2D[2], the circumcenter.
//
{
# define N 2
# define RHS_NUM 1

  double a[N*(N+RHS_NUM)];
  double *pc;
  int info;
//
//  Set up the linear system.
//
  a[0+0*N] = t[0+1*2] - t[0+0*2];
  a[0+1*N] = t[1+1*2] - t[1+0*2];
  a[0+2*N] = pow ( t[0+1*2] - t[0+0*2], 2 )
           + pow ( t[1+1*2] - t[1+0*2], 2 );

  a[1+0*N] = t[0+2*2] - t[0+0*2];
  a[1+1*N] = t[1+2*2] - t[1+0*2];
  a[1+2*N] = pow ( t[0+2*2] - t[0+0*2], 2 )
           + pow ( t[1+2*2] - t[1+0*2], 2 );
//
//  Solve the linear system.
//
  info = r8mat_solve ( N, RHS_NUM, a );
//
//  Compute the center.
//
  pc = new double[2];

  if ( info != 0 )
  {
    pc[0] = 0.0;
    pc[1] = 0.0;
  }
  else
  {
    pc[0] = t[0+0*2] + 0.5 * a[0+N*N];
    pc[1] = t[1+0*2] + 0.5 * a[1+N*N];
  }

  return pc;
# undef N
# undef RHS_NUM
}
//****************************************************************************80

double *triangle_circumcenter ( int n, double t[] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CIRCUMCENTER computes the circumcenter of a triangle in ND.
//
//  Discussion:
//
//    Three ND points A, B and C lie on a circle.
//
//    The circumcenter P has the formula
//
//      P = ( Area ( PBC ) * A + Area ( APC) * B + Area ( ABP ) * C )
//        / ( Area ( PBC )     + Area ( APC )    + Area ( ABP ) )
//
//    The details of the formula rely on information supplied
//    by Oscar Lanzi III.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 October 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the spatial dimension.
//
//    Input, double T[N*3], the triangle vertices.
//
//    Output, double TRIANGLE_CIRCUMCENTER[N], the circumcenter.
//
{
  double a;
  double abp;
  double apc;
  double b;
  double c;
  int i;
  double *p;
  double pbc;

  a = r8vec_normsq_affine ( n, t+1*n, t+2*n );
  b = r8vec_normsq_affine ( n, t+2*n, t+0*n );
  c = r8vec_normsq_affine ( n, t+0*n, t+1*n );

  pbc = a * ( - a + b + c );
  apc = b * (   a - b + c );
  abp = c * (   a + b - c );

  p = new double[n];

  for ( i = 0; i < n; i++ )
  {
    p[i] = ( pbc * t[i+0*n] + apc * t[i+1*n] + abp * t[i+2*n] )
         / ( pbc            + apc            + abp );
  }

  return p;
}
//****************************************************************************80

void triangle_circumcircle_2d ( double t[2*3], double *r, double pc[2] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CIRCUMCIRCLE_2D computes the circumcircle of a triangle in 2D.
//
//  Discussion:
//
//    The circumcenter of a triangle is the center of the circumcircle, the
//    circle that passes through the three vertices of the triangle.
//
//    The circumcircle contains the triangle, but it is not necessarily the
//    smallest triangle to do so.
//
//    If all angles of the triangle are no greater than 90 degrees, then
//    the center of the circumscribed circle will lie inside the triangle.
//    Otherwise, the center will lie outside the triangle.
//
//    The circumcenter is the intersection of the perpendicular bisectors
//    of the sides of the triangle.
//
//    In geometry, the circumcenter of a triangle is often symbolized by "O".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double *R, PC[2], the circumradius, and the coordinates of the
//    circumcenter of the triangle.
//
{
# define DIM_NUM 2

  double a;
  double b;
  double bot;
  double c;
  double top1;
  double top2;
//
//  Circumradius.
//
  a = sqrt ( pow ( t[0+1*2] - t[0+0*2], 2 ) + pow ( t[1+1*2] - t[1+0*2], 2 ) );
  b = sqrt ( pow ( t[0+2*2] - t[0+1*2], 2 ) + pow ( t[1+2*2] - t[1+1*2], 2 ) );
  c = sqrt ( pow ( t[0+0*2] - t[0+2*2], 2 ) + pow ( t[1+0*2] - t[1+2*2], 2 ) );

  bot = ( a + b + c ) * ( - a + b + c ) * (   a - b + c ) * (   a + b - c );

  if ( bot <= 0.0 )
  {
    *r = -1.0;
    pc[0] = 0.0;
    pc[1] = 0.0;
    return;
  }

  *r = a * b * c / sqrt ( bot );
//
//  Circumcenter.
//
  top1 =  ( t[1+1*2] - t[1+0*2] ) * c * c - ( t[1+2*2] - t[1+0*2] ) * a * a;
  top2 =  ( t[0+1*2] - t[0+0*2] ) * c * c - ( t[0+2*2] - t[0+0*2] ) * a * a;
  bot  =  ( t[1+1*2] - t[1+0*2] ) * ( t[0+2*2] - t[0+0*2] )
        - ( t[1+2*2] - t[1+0*2] ) * ( t[0+1*2] - t[0+0*2] );

  pc[0] = t[0+0*2] + 0.5 * top1 / bot;
  pc[1] = t[1+0*2] - 0.5 * top2 / bot;

  return;
# undef DIM_NUM
}
//****************************************************************************80

void triangle_circumcircle_2d_2 ( double t[2*3], double *r, double pc[2] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CIRCUMCIRCLE_2D_2 computes the circumcircle of a triangle in 2D.
//
//  Discussion:
//
//    The circumscribed circle of a triangle is the circle that passes through
//    the three vertices of the triangle.  The circumscribed circle contains
//    the triangle, but it is not necessarily the smallest triangle to do so.
//
//    Surprisingly, the diameter of the circle can be found by solving
//    a 2 by 2 linear system.  This is because the vectors P2 - P1
//    and P3 - P1 are secants of the circle, and each forms a right
//    triangle with the diameter.  Hence, the dot product of
//    P2 - P1 with the diameter is equal to the square of the length
//    of P2 - P1, and similarly for P3 - P1.  This determines the
//    diameter vector originating at P1.
//
//    If all angles of the triangle are no greater than 90 degrees, then
//    the center of the circumscribed circle will lie inside the triangle.
//    Otherwise, the center will lie outside the triangle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double *R, PC[2], the radius and coordinates of the center of the
//    circumscribed circle.  If the linear system is
//    singular, then R = -1, PC = 0.
//
{
# define DIM_NUM 2
# define N 2
# define RHS_NUM 1

  double a[N*(N+RHS_NUM)];
  int info;
//
//  Set up the linear system.
//
  a[0+0*N] = t[0+1*2] - t[0+0*2];
  a[1+0*N] = t[0+2*2] - t[0+0*2];

  a[0+1*N] = t[1+1*2] - t[1+0*2];
  a[1+1*N] = t[1+2*2] - t[1+0*2];

  a[0+2*N] = pow ( t[0+1*2] - t[0+0*2], 2 ) + pow ( t[1+1*2] - t[1+0*2], 2 );
  a[1+2*N] = pow ( t[0+2*2] - t[0+0*2], 2 ) + pow ( t[1+2*2] - t[1+0*2], 2 );
//
//  Solve the linear system.
//
  info = r8mat_solve ( N, RHS_NUM, a );
//
//  If the system was singular, return a consolation prize.
//
  if ( info != 0 )
  {
    *r = -1.0;
    pc[0] = 0.0;
    pc[1] = 0.0;
    return;
  }
//
//  Compute the radius and center.
//
  *r = 0.5 * sqrt ( a[0+N*N] * a[0+N*N] + a[1+N*N] * a[1+N*N] );
  pc[0] = t[0+0*2] + 0.5 * a[0+N*N];
  pc[1] = t[1+0*2] + 0.5 * a[1+N*N];

  return;
# undef DIM_NUM
# undef N
# undef RHS_NUM
}
//****************************************************************************80

double triangle_circumradius_2d ( double t[2*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CIRCUMRADIUS_2D computes the circumradius of a triangle in 2D.
//
//  Discussion:
//
//    The circumscribed circle of a triangle is the circle that passes through
//    the three vertices of the triangle.  The circumscribed circle contains
//    the triangle, but it is not necessarily the smallest triangle to do so.
//
//    The circumradius of a triangle is the radius of the circumscribed
//    circle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double TRIANGLE_CIRCUMRADIUS_2D, the circumradius of the
//    circumscribed circle.
//
{
# define DIM_NUM 2

  double a;
  double b;
  double bot;
  double c;
  double r;

  a = sqrt ( pow ( t[0+1*2] - t[0+0*2], 2 ) + pow ( t[1+1*2] - t[1+0*2], 2 ) );
  b = sqrt ( pow ( t[0+2*2] - t[0+1*2], 2 ) + pow ( t[1+2*2] - t[1+1*2], 2 ) );
  c = sqrt ( pow ( t[0+0*2] - t[0+2*2], 2 ) + pow ( t[1+0*2] - t[1+2*2], 2 ) );

  bot = ( a + b + c ) * ( - a + b + c ) * (   a - b + c ) * (   a + b - c );

  if ( bot <= 0.0 )
  {
    r = -1.0;
    return r;
  }

  r = a * b * c / sqrt ( bot );

  return r;
# undef DIM_NUM
}
//****************************************************************************80

void triangle_contains_line_exp_3d ( double t[3*3], double p1[3],
  double p2[3], bool *inside, double pint[3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CONTAINS_LINE_EXP_3D finds if a line is inside a triangle in 3D.
//
//  Discussion:
//
//    A line will "intersect" the plane of a triangle in 3D if
//    * the line does not lie in the plane of the triangle
//      (there would be infinitely many intersections), AND
//    * the line does not lie parallel to the plane of the triangle
//      (there are no intersections at all).
//
//    Therefore, if a line intersects the plane of a triangle, it does so
//    at a single point.  We say the line is "inside" the triangle if,
//    regarded as 2D objects, the intersection point of the line and the plane
//    is inside the triangle.
//
//    The explicit form of a line in 3D is:
//
//      the line through the points P1, P2.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    31 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Steve Marschner, Cornell University,
//    CS465 Notes: Simple Ray-Triangle Intersection
//
//  Parameters:
//
//    Input, double T[3*3], the triangle vertices.
//    The vertices should be given in counter clockwise order.
//
//    Input, double P1[3], P2[3], two points on the line.
//
//    Output, bool INSIDE, is TRUE if the line is inside the triangle.
//
//    Output, double PINT[3], the point where the line
//    intersects the plane of the triangle.
//
{
# define DIM_NUM 3

  int i;
  int ival;
  double normal[DIM_NUM];
  double normal2[DIM_NUM];
  double temp;
  double v1[DIM_NUM];
  double v2[DIM_NUM];
//
//  Make sure the line is not degenerate.
//
  if ( line_exp_is_degenerate_nd ( DIM_NUM, p1, p2 ) )
  {
    cerr << "\n";
    cerr << "TRIANGLE_CONTAINS_LINE_EXP_3D - Fatal error!\n";
    cerr << "  The explicit line is degenerate.\n";
    exit ( 1 );
  }
//
//  Make sure the triangle is not degenerate.
//
  if ( triangle_is_degenerate_nd ( DIM_NUM, t ) )
  {
    cerr << "\n";
    cerr << "TRIANGLE_CONTAINS_LINE_EXP_3D - Fatal error!\n";
    cerr << "  The triangle is degenerate.\n";
    exit ( 1 );
  }
//
//  Determine a unit normal vector associated with the plane of
//  the triangle.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v1[i] = t[i+1*DIM_NUM] - t[i+0*DIM_NUM];
  }
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v2[i] = t[i+2*DIM_NUM] - t[i+0*DIM_NUM];
  }

  normal[0] = v1[1] * v2[2] - v1[2] * v2[1];
  normal[1] = v1[2] * v2[0] - v1[0] * v2[2];
  normal[2] = v1[0] * v2[1] - v1[1] * v2[0];

  temp = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    temp = temp + pow ( normal[i], 2 );
  }
  temp = sqrt ( temp );

  for ( i = 0; i < DIM_NUM; i++ )
  {
    normal[i] = normal[i] / temp;
  }
//
//  Find the intersection of the plane and the line.
//
  ival = plane_normal_line_exp_int_3d ( t, normal, p1, p2, pint );

  if ( ival == 0 )
  {
    *inside = false;
    for ( i = 0; i < DIM_NUM; i++ )
    {
      pint[i] = HUGE_VAL;
    }
    return;
  }
  else if ( ival == 2 )
  {
    *inside = false;
    r8vec_copy ( DIM_NUM, p1, pint );
    return;
  }
//
//  Now, check that all three triangles made by two vertices and
//  the intersection point have the same "clock sense" as the
//  triangle's normal vector.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v1[i] = t[i+1*DIM_NUM] - t[i+0*DIM_NUM];
  }
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v2[i] = pint[i] - t[i+0*DIM_NUM];
  }

  normal2[0] = v1[1] * v2[2] - v1[2] * v2[1];
  normal2[1] = v1[2] * v2[0] - v1[0] * v2[2];
  normal2[2] = v1[0] * v2[1] - v1[1] * v2[0];

  if ( r8vec_dot_product ( DIM_NUM, normal, normal2 ) < 0.0 )
  {
    *inside = false;
    return;
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    v1[i] = t[i+2*DIM_NUM] - t[i+1*DIM_NUM];
  }
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v2[i] = pint[i] - t[i+1*DIM_NUM];
  }

  normal2[0] = v1[1] * v2[2] - v1[2] * v2[1];
  normal2[1] = v1[2] * v2[0] - v1[0] * v2[2];
  normal2[2] = v1[0] * v2[1] - v1[1] * v2[0];

  if ( r8vec_dot_product ( DIM_NUM, normal, normal2 ) < 0.0 )
  {
    *inside = false;
    return;
  }

  for ( i = 0; i < DIM_NUM; i++ )
  {
    v1[i] = t[i+0*DIM_NUM] - t[i+2*DIM_NUM];
  }
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v2[i] = pint[i] - t[i+2*DIM_NUM];
  }

  normal2[0] = v1[1] * v2[2] - v1[2] * v2[1];
  normal2[1] = v1[2] * v2[0] - v1[0] * v2[2];
  normal2[2] = v1[0] * v2[1] - v1[1] * v2[0];

  if ( r8vec_dot_product ( DIM_NUM, normal, normal2 ) < 0.0 )
  {
    *inside = false;
    return;
  }

  *inside = true;

  return;
# undef DIM_NUM
}
//****************************************************************************80

void triangle_contains_line_par_3d ( double t[], double p0[], double pd[],
  bool *inside, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CONTAINS_LINE_PAR_3D: finds if a line is inside a triangle in 3D.
//
//  Discussion:
//
//    A line will "intersect" the plane of a triangle in 3D if
//    * the line does not lie in the plane of the triangle
//      (there would be infinitely many intersections), AND
//    * the line does not lie parallel to the plane of the triangle
//      (there are no intersections at all).
//
//    Therefore, if a line intersects the plane of a triangle, it does so
//    at a single point.  We say the line is "inside" the triangle if,
//    regarded as 2D objects, the intersection point of the line and the plane
//    is inside the triangle.
//
//    A triangle in 3D is determined by three points:
//
//      T(1:3,1), T(1:3,2) and T(1:3,3).
//
//    The parametric form of a line in 3D is:
//
//      P(1:3) = P0(1:3) + PD(1:3) * T
//
//    We can normalize by requiring PD to have euclidean norm 1,
//    and the first nonzero entry positive.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    12 February 2007
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983, page 111.
//
//  Parameters:
//
//    Input, double T[3*3], the three points that define
//    the triangle.
//
//    Input, double P0[3], PD(3], parameters that define the
//    parametric line.
//
//    Output, bool *INSIDE, is TRUE if (the intersection point of)
//    the line is inside the triangle.
//
//    Output, double P[3], is the point of intersection of the line
//    and the plane of the triangle, unless they are parallel.
//
{
# define DIM_NUM 3

  double a;
  double angle_sum;
  double b;
  double c;
  double d;
  double denom;
  int dim;
//bool intersect;
  double norm;
  double norm1;
  double norm2;
  double r8_pi = 3.141592653589793;
  double t_int;
  double tol = 0.00001;
  double v1[DIM_NUM];
  double v2[DIM_NUM];
  double v3[DIM_NUM];
//
//  Determine the implicit form (A,B,C,D) of the plane containing the
//  triangle.
//
  a = ( t[1+1*3] - t[1+0*3] ) * ( t[2+2*3] - t[2+0*3] )
    - ( t[2+1*3] - t[2+0*3] ) * ( t[1+2*3] - t[1+0*3] );

  b = ( t[2+1*3] - t[2+0*3] ) * ( t[0+2*3] - t[0+0*3] )
    - ( t[0+1*3] - t[0+0*3] ) * ( t[2+2*3] - t[2+0*3] );

  c = ( t[0+1*3] - t[0+0*3] ) * ( t[1+2*3] - t[1+0*3] )
    - ( t[1+1*3] - t[1+0*3] ) * ( t[0+2*3] - t[0+0*3] );

  d = - t[0+1*3] * a - t[1+1*3] * b - t[2+1*3] * c;
//
//  Make sure the plane is well-defined.
//
  norm1 = sqrt ( a * a + b * b + c * c );

  if ( norm1 == 0.0 )
  {
    cerr << "\n";
    cerr << "TRIANGLE_LINE_PAR_INT_3D - Fatal error!\n";
    cerr << "  The plane normal vector is null.\n";
    *inside = false;
    r8vec_zero ( DIM_NUM, p );
    exit ( 1 );
  }
//
//  Make sure the implicit line is well defined.
//
  norm2 = r8vec_norm ( DIM_NUM, pd );

  if ( norm2 == 0.0 )
  {
    cerr << "\n";
    cerr << "TRIANGLE_LINE_PAR_INT_3D - Fatal error!\n";
    cerr << "  The line direction vector is null.\n";
    *inside = false;
    r8vec_zero ( DIM_NUM, p );
    exit ( 1 );
  }
//
//  Determine the denominator of the parameter in the
//  implicit line definition that determines the intersection
//  point.
//
  denom = a * pd[0] + b * pd[1] + c * pd[2];
//
//  If DENOM is zero, or very small, the line and the plane may be
//  parallel or almost so.
//
  if ( fabs ( denom ) < tol * norm1 * norm2 )
  {
//  The line may actually lie in the plane.  We're not going
//  to try to address this possibility.
//
    if ( a * p0[0] + b * p0[1] + c * p0[2] + d == 0.0 )
    {
      *inside = false;
      r8vec_copy ( DIM_NUM, p0, p );
    }
//
//  The line and plane are parallel and disjoint.
//
    else
    {
      *inside = false;
      r8vec_zero ( DIM_NUM, p );
    }
  }
//
//  The line and plane intersect at a single point P.
//
  else
  {
    t_int = - ( a * p0[0] + b * p0[1] + c * p0[2] + d ) / denom;
    for ( dim = 0; dim < DIM_NUM; dim++ )
    {
      p[dim] = p0[dim] + t_int * pd[dim];
    }
//
//  To see if P is included in the triangle, sum the angles
//  formed by P and pairs of the vertices.  If the point is in the
//  triangle, we get a total 360 degree view.  Otherwise, we
//  get less than 180 degrees.
//
    for ( dim = 0; dim < DIM_NUM; dim++ )
    {
      v1[dim] = t[dim+0*3] - p[dim];
    }
    for ( dim = 0; dim < DIM_NUM; dim++ )
    {
      v2[dim] = t[dim+1*3] - p[dim];
    }
    for ( dim = 0; dim < DIM_NUM; dim++ )
    {
      v3[dim] = t[dim+2*3] - p[dim];
    }

    norm = r8vec_norm ( DIM_NUM, v1 );

    if ( norm == 0.0 )
    {
      *inside = true;
      return;
    }

    for ( dim = 0; dim < DIM_NUM; dim++ )
    {
      v1[dim] = v1[dim] / norm;
    }

    norm = r8vec_norm ( DIM_NUM, v2 );

    if ( norm == 0.0 )
    {
      *inside = true;
      return;
    }

    for ( dim = 0; dim < DIM_NUM; dim++ )
    {
      v2[dim] = v2[dim] / norm;
    }

    norm = r8vec_norm ( DIM_NUM, v3 );

    if ( norm == 0.0 )
    {
      *inside = true;
      return;
    }

    for ( dim = 0; dim < DIM_NUM; dim++ )
    {
      v3[dim] = v3[dim] / norm;
    }

    angle_sum = r8_acos ( r8vec_dot_product ( DIM_NUM, v1, v2 ) )
              + r8_acos ( r8vec_dot_product ( DIM_NUM, v2, v3 ) )
              + r8_acos ( r8vec_dot_product ( DIM_NUM, v3, v1 ) );

    if ( r8_nint ( angle_sum / r8_pi ) == 2 )
    {
      *inside = true;
    }
    else
    {
      *inside = false;
    }

  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

bool triangle_contains_point_2d_1 ( double t[2*3], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CONTAINS_POINT_2D_1 finds if a point is inside a triangle in 2D.
//
//  Discussion:
//
//    It is conventional to list the triangle vertices in counter clockwise
//    order.  However, this routine does not require a particular order
//    for the vertices.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Input, double P[2], the point to be checked.
//
//    Output, bool TRIANGLE_CONTAINS_POINT_2D_1, is TRUE if the points
//    is inside the triangle or on its boundary, and FALSE otherwise.
//
{
  double *c;
  int i;
  bool value;

  c = triangle_barycentric_2d ( t, p );

  value = true;

  for ( i = 0; i < 3; i++ )
  {
    if ( c[i] < 0.0 )
    {
      value = false;
    }
  }
  delete [] c;

  return value;
}
//****************************************************************************80

bool triangle_contains_point_2d_2 ( double t[2*3], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CONTAINS_POINT_2D_2 finds if a point is inside a triangle in 2D.
//
//  Discussion:
//
//    The routine assumes that the vertices are given in counter clockwise
//    order.  If the triangle vertices are actually given in clockwise
//    order, this routine will behave as though the triangle contains
//    no points whatsoever!
//
//    The routine determines if P is "to the right of" each of the lines
//    that bound the triangle.  It does this by computing the cross product
//    of vectors from a vertex to its next vertex, and to P.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 June 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//    The vertices should be given in counter clockwise order.
//
//    Input, double P[2], the point to be checked.
//
//    Output, bool TRIANGLE_CONTAINS_POINT_2D_2, is TRUE if P is inside
//    the triangle or on its boundary.
//
{
# define DIM_NUM 2

  int j;
  int k;

  for ( j = 0; j < 3; j++ )
  {
    k = ( j + 1 ) % 3;
    if ( 0.0 < ( p[0] - t[0+j*2] ) * ( t[1+k*2] - t[1+j*2] )
             - ( p[1] - t[1+j*2] ) * ( t[0+k*2] - t[0+j*2] ) )
    {
      return false;
    }
  }

  return true;
# undef DIM_NUM
}
//****************************************************************************80

bool triangle_contains_point_2d_3 ( double t[2*3], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_CONTAINS_POINT_2D_3 finds if a point is inside a triangle in 2D.
//
//  Discussion:
//
//    This routine is the same as TRIANGLE_CONTAINS_POINT_2D_2, except
//    that it does not assume an ordering of the points.  It should
//    work correctly whether the vertices of the triangle are listed
//    in clockwise or counter clockwise order.
//
//    The routine determines if a point P is "to the right of" each of the lines
//    that bound the triangle.  It does this by computing the cross product
//    of vectors from a vertex to its next vertex, and to P.
//
//    The point is inside the triangle if it is to the right of all
//    the lines, or to the left of all the lines.
//
//    This version was suggested by Paulo Ernesto of Maptek Brasil.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 June 2006
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Input, double P[2], the point to be checked.
//
//    Output, bool TRIANGLE_CONTAINS_POINT_2D_3, is TRUE if P is inside
//    the triangle or on its boundary.
//
{
# define DIM_NUM 2

  double dir_new;
  double dir_old;
  int j;
  int k;

  dir_old = 0.0;

  for ( j = 0; j < 3; j++ )
  {
    k = ( j + 1 ) % 3;

    dir_new = ( p[0] - t[0+j*2] ) * ( t[1+k*2] - t[1+j*2] )
            - ( p[1] - t[1+j*2] ) * ( t[0+k*2] - t[0+j*2] );

    if ( dir_new * dir_old < 0.0 )
    {
      return false;
    }

    if ( dir_new != 0.0 )
    {
      dir_old = dir_new;
    }
  }

  return true;
# undef DIM_NUM
}
//****************************************************************************80

double triangle_diameter_2d ( double t[2*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_DIAMETER_2D computes the diameter of a triangle in 2D.
//
//  Discussion:
//
//    The diameter of a triangle is the diameter of the smallest circle
//    that can be drawn around the triangle.  At least two of the vertices
//    of the triangle will intersect the circle, but not necessarily
//    all three!
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double TRIANGLE_DIAMETER_2D, the diameter of the triangle.
//
{
# define DIM_NUM 2

  double a;
  double b;
  double c;
  double diam;
//
//  Compute the (squares of) the lengths of the sides.
//
  a = sqrt ( pow ( t[0+1*2] - t[0+0*2], 2 ) + pow ( t[1+1*2] - t[1+0*2], 2 ) );
  b = sqrt ( pow ( t[0+2*2] - t[0+1*2], 2 ) + pow ( t[1+2*2] - t[1+1*2], 2 ) );
  c = sqrt ( pow ( t[0+0*2] - t[0+2*2], 2 ) + pow ( t[1+0*2] - t[1+2*2], 2 ) );
//
//  Take care of a zero side.
//
  if ( a == 0.0 )
  {
    return sqrt ( b );
  }
  else if ( b == 0.0 )
  {
    return sqrt ( c );
  }
  else if ( c == 0.0 )
  {
    return sqrt ( a );
  }
//
//  Make A the largest.
//
  if ( a < b )
  {
    r8_swap ( &a, &b );
  }

  if ( a < c )
  {
    r8_swap ( &a, &c );
  }
//
//  If A is very large...
//
  if ( b + c < a )
  {
    diam = sqrt ( a );
  }
  else
  {
    a = sqrt ( a );
    b = sqrt ( b );
    c = sqrt ( c );
    diam = 2.0 * a * b * c / sqrt ( ( a + b + c ) * ( - a + b + c )
      * ( a - b + c ) * ( a + b - c ) );
  }

  return diam;
# undef DIM_NUM
}
//****************************************************************************80

double *triangle_edge_length_2d ( double t[2*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_EDGE_LENGTH_2D returns edge lengths of a triangle in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 August 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double TRIANGLE_EDGE_LENGTH[3], the length of the edges.
//
{
  double *edge_length;
  int j1;
  int j2;

  edge_length = new double[3];

  for ( j1 = 0; j1 < 3; j1++ )
  {
    j2 = i4_wrap ( j1 + 1, 0, 2 );
    edge_length[j1] = sqrt ( pow ( t[0+j2*2] - t[0+j1*2], 2 )
                           + pow ( t[1+j2*2] - t[1+j1*2], 2 ) );
  }

  return edge_length;
}
//****************************************************************************80

void triangle_gridpoints_2d ( double t[2*3], int sub_num, int grid_max,
  int *grid_num, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_GRIDPOINTS_2D computes gridpoints within a triangle in 2D.
//
//  Discussion:
//
//    The gridpoints are computed by repeated halving of the triangle.
//    The 0-th set of grid points is the vertices themselves.
//    The first set of grid points is the midpoints of the sides.
//    These points can be used to draw 4 triangles that make up the original
//    triangle.  The second set of grid points is the side midpoints and centers
//    of these four triangles.
//
//    SUB_NUM                     GRID_NUM
//    -----                        -----
//        0      1                  =  1  (centroid)
//        1      1 + 2              =  3  (vertices)
//        2      1 + 2 + 3          =  6
//        3      1 + 2 + 3 + 4      = 10
//        4      1 + 2 + 3 + 4 + 5  = 15
//
//    GRID_NUM is the sum of the integers from 1 to SUB_NUM+1 or
//
//      GRID_NUM = (SUB_NUM+1) * (SUB_NUM+2) / 2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Input, int SUB_NUM, the number of subdivisions.
//
//    Input, int GRID_MAX, the maximum number of grid points.
//
//    Output, int *GRID_NUM, the number of grid points returned.
//
//    Output, double P[2*(*GRID_NUM)], coordinates of the grid points.
//
{
# define DIM_NUM 2

  int i;
  int j;

  *grid_num = 0;
//
//  Special case, SUB_NUM = 0.
//
  if ( sub_num == 0 )
  {
    if ( *grid_num + 1 <= grid_max )
    {
      p[0+(*grid_num)*2] = ( t[0+0*2] + t[0+1*2] + t[0+2*2] ) / 3.0;
      p[1+(*grid_num)*2] = ( t[1+0*2] + t[1+1*2] + t[1+2*2] ) / 3.0;
      *grid_num = *grid_num + 1;
    }
    return;
  }

  for ( i = 0; i <= sub_num; i++ )
  {
    for ( j = 0; j <= sub_num - i; j++ )
    {
      if ( grid_max <= *grid_num )
      {
        return;
      }

      p[0+(*grid_num)*2] = ( ( double )             i      * t[0+0*2]
                           + ( double )                 j  * t[0+1*2]
                           + ( double ) ( sub_num - i - j )* t[0+2*2] )
                         / ( ( double )   sub_num               );

      p[1+(*grid_num)*2] = ( ( double )             i      * t[1+0*2]
                           + ( double )                 j  * t[1+1*2]
                           + ( double ) ( sub_num - i - j )* t[1+2*2] )
                         / ( ( double )   sub_num               );

      *grid_num = *grid_num + 1;
    }
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

void triangle_incenter_2d ( double t[2*3], double pc[2] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_INCENTER_2D computes the incenter of a triangle in 2D.
//
//  Discussion:
//
//    The incenter of a triangle is the center of the inscribed circle.
//
//    The inscribed circle of a triangle is the largest circle that can
//    be drawn inside the triangle.
//
//    The inscribed circle is tangent to all three sides of the triangle.
//
//    The angle bisectors of the triangle intersect at the center of the
//    inscribed circle.
//
//    In geometry, the incenter is often represented by "I".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double PC[2], the coordinates of the center of the
//    inscribed circle.
//
{
# define DIM_NUM 2

  double perim;
  double s12;
  double s23;
  double s31;

  s12 = sqrt ( pow ( t[0+1*2] - t[0+0*2], 2 )
             + pow ( t[1+1*2] - t[1+0*2], 2 ) );
  s23 = sqrt ( pow ( t[0+2*2] - t[0+1*2], 2 )
             + pow ( t[1+2*2] - t[1+1*2], 2 ) );
  s31 = sqrt ( pow ( t[0+0*2] - t[0+2*2], 2 )
             + pow ( t[1+0*2] - t[1+2*2], 2 ) );

  perim = s12 + s23 + s31;

  if ( perim == 0.0 )
  {
    pc[0] = t[0+0*2];
    pc[1] = t[1+0*2];
  }
  else
  {
    pc[0] = ( s23 * t[0+0*2] + s31 * t[0+1*2] + s12 * t[0+2*2] ) / perim;
    pc[1] = ( s23 * t[1+0*2] + s31 * t[1+1*2] + s12 * t[1+2*2] ) / perim;
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

void triangle_incircle_2d ( double t[2*3], double pc[2], double *r )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_INCIRCLE_2D computes the inscribed circle of a triangle in 2D.
//
//  Discussion:
//
//    The inscribed circle of a triangle is the largest circle that can
//    be drawn inside the triangle.  It is tangent to all three sides,
//    and the lines from its center to the vertices bisect the angles
//    made by each vertex.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double PC[2], *R, the center of the inscribed circle, and its radius.
//
{
# define DIM_NUM 2

  double perim;
  double s12;
  double s23;
  double s31;

  s12 = sqrt ( pow ( t[0+1*2] - t[0+0*2], 2 )
             + pow ( t[1+1*2] - t[1+0*2], 2 ) );
  s23 = sqrt ( pow ( t[0+2*2] - t[0+1*2], 2 )
             + pow ( t[1+2*2] - t[1+1*2], 2 ) );
  s31 = sqrt ( pow ( t[0+0*2] - t[0+2*2], 2 )
             + pow ( t[1+0*2] - t[1+2*2], 2 ) );

  perim = s12 + s23 + s31;

  if ( perim == 0.0 )
  {
    *r = 0.0;
    pc[0] = t[0+0*2];
    pc[1] = t[1+0*2];
  }
  else
  {
    pc[0] = ( s23 * t[0+0*2] + s31 * t[0+1*2] + s12 * t[0+2*2] ) / perim;
    pc[1] = ( s23 * t[1+0*2] + s31 * t[1+1*2] + s12 * t[1+2*2] ) / perim;

    *r = 0.5 * sqrt (
        ( - s12 + s23 + s31 )
      * ( + s12 - s23 + s31 )
      * ( + s12 + s23 - s31 ) / perim );
  }
  return;
# undef DIM_NUM
}
//****************************************************************************80

double triangle_inradius_2d ( double t[2*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_INRADIUS_2D computes the inradius of a triangle in 2D.
//
//  Discussion:
//
//    The inscribed circle of a triangle is the largest circle that can
//    be drawn inside the triangle.  It is tangent to all three sides,
//    and the lines from its center to the vertices bisect the angles
//    made by each vertex.
//
//    The inradius is the radius of the inscribed circle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double TRIANGLE_INRADIUS_2D, the inradius.
//
{
# define DIM_NUM 2

  double perim;
  double r;
  double s12;
  double s23;
  double s31;

  s12 = sqrt ( pow ( t[0+1*2] - t[0+0*2], 2 )
             + pow ( t[1+1*2] - t[1+0*2], 2 ) );
  s23 = sqrt ( pow ( t[0+2*2] - t[0+1*2], 2 )
             + pow ( t[1+2*2] - t[1+1*2], 2 ) );
  s31 = sqrt ( pow ( t[0+0*2] - t[0+2*2], 2 )
             + pow ( t[1+0*2] - t[1+2*2], 2 ) );

  perim = s12 + s23 + s31;

  if ( perim == 0.0 )
  {
    r = 0.0;
  }
  else
  {
    r = 0.5 * sqrt (
        ( - s12 + s23 + s31 )
      * ( + s12 - s23 + s31 )
      * ( + s12 + s23 - s31 ) / perim );
  }

  return r;
# undef DIM_NUM
}
//****************************************************************************80

bool triangle_is_degenerate_nd ( int dim_num, double t[] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_IS_DEGENERATE_ND finds if a triangle is degenerate in ND.
//
//  Discussion:
//
//    A triangle in ND is described by the coordinates of its 3 vertices.
//
//    A triangle in ND is degenerate if any two vertices are equal.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    31 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, double T[DIM_NUM*3], the triangle vertices.
//
//    Output, bool TRIANGLE_IS_DEGENERATE_ND, is TRUE if the
//    triangle is degenerate.
//
{
  bool value;

  value =
    r8vec_eq ( dim_num, t+0*dim_num, t+1*dim_num ) ||
    r8vec_eq ( dim_num, t+1*dim_num, t+2*dim_num ) ||
    r8vec_eq ( dim_num, t+2*dim_num, t+0*dim_num );

  return value;
}
//****************************************************************************80

void triangle_lattice_layer_point_next ( int c[], int v[], bool *more )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_LATTICE_LAYER_POINT_NEXT: next triangle lattice layer point.
//
//  Discussion:
//
//    The triangle lattice layer L is bounded by the lines
//
//      0 <= X,
//      0 <= Y,
//      L - 1 < X / C[0] + Y / C[1] <= L.
//
//    In particular, layer L = 0 always contains the single point (0,0).
//
//    This function returns, one at a time, the points that lie within
//    a given triangle lattice layer.
//
//    Thus, if we set C[0] = 2, C[1] = 3, then we get the following layers:
//
//    L = 0: (0,0)
//    L = 1: (1,0), (2,0), (0,1), (1,1), (0,2), (0,3)
//    L = 2: (3,0), (4,0), (2,1), (3,1), (1,2), (2,2), (1,3), (2,3),
//           (0,4), (1,4), (0,5), (0,6).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int C[3], coefficients defining the
//    lattice layer.  Entry C[2] contains the layer index.
//    C[0] and C[1] should be positive, and C[2] must be nonnegative.
//
//    Input/output, int V[2].  On first call for a given layer,
//    the input value of V is not important.  On a repeated call for the same
//    layer, the input value of V should be the output value from the previous
//    call.  On output, V contains the next lattice layer point.
//
//    Input/output, bool *MORE.  On input, set MORE to FALSE to indicate
//    that this is the first call for a given layer.  Thereafter, the input
//    value should be the output value from the previous call.  On output,
//    MORE is TRUE if the returned value V is a new point.
//    If the output value is FALSE, then no more points were found,
//    and V was reset to 0, and the lattice layer has been exhausted.
//
 {
  int c1n;
  int n = 2;
  int rhs1;
  int rhs2;
//
//  Treat layer C[N] = 0 specially.
//
  if ( c[n] == 0 )
  {
    if ( !(*more) )
    {
      v[0] = 0;
      v[1] = 0;
      *more = true;
    }
    else
    {
      *more = false;
    }
    return;
  }
//
//  Compute first point.
//
  if ( !(*more) )
  {
    v[0] = ( c[n] - 1 ) * c[0] + 1;
    v[1] = 0;
    *more = true;
  }
  else
  {
    c1n = i4vec_lcm ( n, c );
    rhs1 = c1n * ( c[n] - 1 );
    rhs2 = c1n *   c[n];

    if ( c[1] * ( v[0] + 1 ) + c[0] * v[1] <= rhs2 )
    {
      v[0] = v[0] + 1;
    }
    else
    {
      v[0] = ( rhs1 - c[0] * ( v[1] + 1 ) ) / c[1];
      v[0] = i4_max ( v[0], 0 );
      v[1] = v[1] + 1;
      if ( c[1] * v[0] + c[0] * v[1] <= rhs1 )
      {
        v[0] = v[0] + 1;
      }
      if ( c[1] * v[0] + c[0] * v[1] <= rhs2 )
      {
      }
      else
      {
        v[0] = 0;
        v[1] = 0;
        *more = false;
      }
    }
  }
  return;
}
//****************************************************************************80

void triangle_lattice_point_next ( int c[], int v[], bool *more )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_LATTICE_POINT_NEXT returns the next triangle lattice point.
//
//  Discussion:
//
//    The lattice triangle is defined by the vertices:
//
//      (0,0), (C[2]/C[0], 0) and (0,C[2]/C[1])
//
//    The lattice triangle is bounded by the lines
//
//      0 <= X,
//      0 <= Y
//      X / C[0] + Y / C[1] <= C[2]
//
//    Lattice points are listed one at a time, starting at the origin,
//    with X increasing first.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int C[3], coefficients defining the
//    lattice triangle.  These should be positive.
//
//    Input/output, int V[2].  On first call, the input
//    value is not important.  On a repeated call, the input value should
//    be the output value from the previous call.  On output, V contains
//    the next lattice point.
//
//    Input/output, bool *MORE.  On input, set MORE to FALSE to indicate
//    that this is the first call for a given triangle.  Thereafter, the input
//    value should be the output value from the previous call.  On output,
//    MORE is TRUE if the returned value V is a new lattice point.
//    If the output value is FALSE, then no more lattice points were found,
//    and V was reset to 0, and the routine should not be called further
//    for this triangle.
//
{
  int c1n;
  int n = 2;
  int rhs;

  if ( !(*more) )
  {
    v[0] = 0;
    v[1] = 0;
    *more = true;
  }
  else
  {
    c1n = i4vec_lcm ( n, c );
    rhs = c1n * c[n];

    if ( c[1] * ( v[0] + 1 ) + c[0] * v[1] <= rhs )
    {
      v[0] = v[0] + 1;
    }
    else
    {
      v[0] = 0;
      if ( c[1] * v[0] + c[0] * ( v[1] + 1 ) <= rhs )
      {
        v[1] = v[1] + 1;
      }
      else
      {
        v[1] = 0;
        *more = false;
      }
    }
  }
  return;
}
//****************************************************************************80

void triangle_line_imp_int_2d ( double t[2*3], double a, double b, double c,
  int *int_num, double pint[] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_LINE_IMP_INT_2D finds where an implicit line intersects a triangle in 2D.
//
//  Discussion:
//
//    An implicit line is the set of points P satisfying
//
//      A * P[0] + B * P[1] + C = 0
//
//    where at least one of A and B is not zero.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Input, double A, B, C, determine the equation of the line:
//    A*X + B*Y + C = 0.
//
//    Output, int *INT_NUM, the number of points of intersection
//    of the line with the triangle.  INT_NUM may be 0, 1, 2 or 3.
//
//    Output, double PINT[3*3], contains the intersection points.
//
{
# define DIM_NUM 2

  double a1;
  double b1;
  double c1;
  int ival;
  int n;
  double p[DIM_NUM];
  int r;
  int s;
  double test1;
  double test2;

  n = 0;

  for ( r = 0; r < 3; r++ )
  {
    s = i4_wrap ( r+1, 0, 2 );
//
//  Get the implicit form of the line through vertices R and R+1.
//
    line_exp2imp_2d ( t+0+r*2, t+0+s*2, &a1, &b1, &c1 );
//
//  Seek an intersection with the original line.
//
    lines_imp_int_2d ( a, b, c, a1, b1, c1, &ival, p );
//
//  If there is an intersection, then determine if it happens between
//  the two vertices.
//
    if ( ival == 1 )
    {
      test1 = ( p[0] - t[0+r*2] ) * ( t[0+s*2] - t[0+r*2] )
            + ( p[1] - t[1+r*2] ) * ( t[1+s*2] - t[1+r*2] );

      test2 = ( t[0+s*2] - t[0+r*2] ) * ( t[0+s*2] - t[0+r*2] )
            + ( t[1+s*2] - t[1+r*2] ) * ( t[1+s*2] - t[1+r*2] );

      if ( 0 <= test1 && test1 <= test2 )
      {
        pint[0+n*2] = p[0];
        pint[1+n*2] = p[1];
        n = n + 1;
      }
    }
  }

  *int_num = n;

  return;
# undef DIM_NUM
}
//****************************************************************************80

int triangle_orientation_2d ( double t[2*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_ORIENTATION_2D determines the orientation of a triangle in 2D.
//
//  Discussion:
//
//    Three distinct non-colinear points in the plane define a circle.
//    If the points are visited in the order (x1,y1), (x2,y2), and then
//    (x3,y3), this motion defines a clockwise or counter clockwise
//    rotation along the circle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, int TRIANGLE_ORIENTATION_2D, reports if the three points lie
//    clockwise on the circle that passes through them.  The possible
//    return values are:
//    0, the points are distinct, noncolinear, and lie counter clockwise
//    on their circle.
//    1, the points are distinct, noncolinear, and lie clockwise
//    on their circle.
//    2, the points are distinct and colinear.
//    3, at least two of the points are identical.
//
{
# define DIM_NUM 2

  double det;
  int value = 0;

  if ( r8vec_eq ( 2, t+0*2, t+1*2 ) ||
       r8vec_eq ( 2, t+1*2, t+2*2 ) ||
       r8vec_eq ( 2, t+2*2, t+0*2 ) )
  {
    value = 3;
    return value;
  }

  det = ( t[0+0*2] - t[0+2*2] ) * ( t[1+1*2] - t[1+2*2] )
      - ( t[0+1*2] - t[0+2*2] ) * ( t[1+0*2] - t[1+2*2] );

  if ( det == 0.0 )
  {
    value = 2;
  }
  else if ( det < 0.0 )
  {
    value = 1;
  }
  else if ( 0.0 < det )
  {
    value = 0;
  }
  return value;
# undef DIM_NUM
}
//****************************************************************************80

void triangle_orthocenter_2d ( double t[2*3], double p[2], bool *flag )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_ORTHOCENTER_2D computes the orthocenter of a triangle in 2D.
//
//  Discussion:
//
//    The orthocenter is defined as the intersection of the three altitudes
//    of a triangle.
//
//    An altitude of a triangle is the line through a vertex of the triangle
//    and perpendicular to the opposite side.
//
//    In geometry, the orthocenter of a triangle is often symbolized by "H".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    31 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double P[2], the coordinates of the orthocenter of the triangle.
//
//    Output, bool *FLAG, is TRUE if the value could not be computed.
//
{
# define DIM_NUM 2

  int ival;
  double *p23;
  double *p31;
//
//  Determine a point P23 common to the line through P2 and P3 and
//  its perpendicular through P1.
//
  p23 = line_exp_perp_2d ( t+1*2, t+2*2, t+0*2, flag );

  if ( *flag )
  {
    p[0] = HUGE_VAL;
    p[1] = HUGE_VAL;
    delete [] p23;
  }
//
//  Determine a point P31 common to the line through P3 and P1 and
//  its perpendicular through P2.
//
  p31 = line_exp_perp_2d ( t+2*2, t+0*2, t+1*2, flag );

  if ( *flag )
  {
    p[0] = HUGE_VAL;
    p[1] = HUGE_VAL;
    delete [] p23;
    delete [] p31;
  }
//
//  Determine P, the intersection of the lines through P1 and P23, and
//  through P2 and P31.
//
  lines_exp_int_2d ( t+0*2, p23, t+1*2, p31, &ival, p );

  if ( ival != 1 )
  {
    p[0] = HUGE_VAL;
    p[1] = HUGE_VAL;
    *flag = true;
    delete [] p23;
    delete [] p31;
    return;
  }
  delete [] p23;
  delete [] p31;

  return;
# undef DIM_NUM
}
//****************************************************************************80

double triangle_point_dist_2d ( double t[2*3], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_POINT_DIST_2D: distance ( triangle, point ) in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Input, double P[2], the point which is to be checked.
//
//    Output, double TRIANGLE_POINT_DIST_2D, the distance from the point to the triangle.
//    DIST is zero if the point lies exactly on the triangle.
//
{
# define DIM_NUM 2

  double value;

  value =
    segment_point_dist_2d ( t+0*DIM_NUM, t+1*DIM_NUM, p );
  value = fmin ( value,
    segment_point_dist_2d ( t+1*DIM_NUM, t+2*DIM_NUM, p ) );
  value = fmin ( value,
    segment_point_dist_2d ( t+2*DIM_NUM, t+0*DIM_NUM, p ) );

  return value;
# undef DIM_NUM
}
//****************************************************************************80

double triangle_point_dist_3d ( double t[3*3], double p[3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_POINT_DIST_3D: distance ( triangle, point ) in 3D.
//
//  Discussion:
//
//    Thanks to Gordon Griesel for pointing out that a triangle in 3D
//    has to have coordinates in 3D as well.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    21 October 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[3*3], the triangle vertices.
//
//    Input, double P[3], the point which is to be checked.
//
//    Output, double TRIANGLE_POINT_DIST_3D, the distance from the point
//    to the triangle.
//
{
# define DIM_NUM 3

  double value;

  value =
    segment_point_dist_3d ( t+0*DIM_NUM, t+1*DIM_NUM, p );

  value = fmin ( value,
    segment_point_dist_3d ( t+1*DIM_NUM, t+2*DIM_NUM, p ) );

  value = fmin ( value,
    segment_point_dist_3d ( t+2*DIM_NUM, t+0*DIM_NUM, p ) );

  return value;
# undef DIM_NUM
}
//****************************************************************************80

double triangle_point_dist_signed_2d ( double t[2*3], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_POINT_DIST_SIGNED_2D: signed distance ( triangle, point ) in 2D.
//
//  Discussion:
//
//    If the signed distance is:
//    0, the point is on the boundary of the triangle;
//    negative, the point is in the triangle;
//    positive, the point is outside the triangle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//    These should be given in counter clockwise order.
//
//    Input, double P[2], the point which is to be checked.
//
//    Output, double TRIANGLE_POINT_DIST_SIGNED_2D, the signed distance from the
//    point to the triangle.
//
{
# define DIM_NUM 2

  double dis12;
  double dis23;
  double dis31;
  double value;
//
//  Compute the signed line-distances to the point.
//
  dis12 = line_exp_point_dist_signed_2d ( t+0*2, t+1*2, p );
  dis23 = line_exp_point_dist_signed_2d ( t+1*2, t+2*2, p );
  dis31 = line_exp_point_dist_signed_2d ( t+2*2, t+0*2, p );
//
//  If the point is inside the triangle, all the line-distances are negative.
//  The largest (negative) line-distance has the smallest magnitude,
//  and is the signed triangle-distance.
//
  if ( dis12 <= 0.0 && dis23 <= 0.0 && dis31 <= 0.0 )
  {
    value =                 dis12;
    value = fmax ( value, dis23 );
    value = fmax ( value, dis31 );
  }
//
//  If the point is outside the triangle, then we have to compute
//  the (positive) line-segment distances and take the minimum.
//
  else
  {
    value =                 segment_point_dist_2d ( t+0*2, t+1*2, p );
    value = fmin ( value, segment_point_dist_2d ( t+1*2, t+2*2, p ) );
    value = fmin ( value, segment_point_dist_2d ( t+2*2, t+0*2, p ) );
  }

  return value;
# undef DIM_NUM
}
//****************************************************************************80

void triangle_point_near_2d ( double t[2*3], double p[2], double pn[2],
  double *dist )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_POINT_NEAR_2D computes the nearest triangle point to a point in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Input, double P[2], the point whose nearest neighbor
//    on the line is to be determined.
//
//    Output, double PN[2], the nearest point to P.
//
//    Output, double *DIST, the distance from the point to the triangle.
//
{
# define DIM_NUM 2

  double dist12;
  double dist23;
  double dist31;
  double tval;
  double pn12[DIM_NUM];
  double pn23[DIM_NUM];
  double pn31[DIM_NUM];
//
//  Find the distance to each of the line segments that make up the edges
//  of the triangle.
//
  segment_point_near_2d ( t+0*DIM_NUM, t+1*DIM_NUM, p, pn12, &dist12, &tval );

  segment_point_near_2d ( t+1*DIM_NUM, t+2*DIM_NUM, p, pn23, &dist23, &tval );

  segment_point_near_2d ( t+2*DIM_NUM, t+0*DIM_NUM, p, pn31, &dist31, &tval );

  if ( dist12 <= dist23 && dist12 <= dist31 )
  {
    *dist = dist12;
    r8vec_copy ( DIM_NUM, pn12, pn );
  }
  else if ( dist23 <= dist12 && dist23 <= dist31 )
  {
    *dist = dist23;
    r8vec_copy ( DIM_NUM, pn23, pn );
  }
  else
  {
    *dist = dist31;
    r8vec_copy ( DIM_NUM, pn31, pn );
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

double triangle_quality_2d ( double t[2*3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_QUALITY_2D: "quality" of a triangle in 2D.
//
//  Discussion:
//
//    The quality of a triangle is 2 times the ratio of the radius of the
//    inscribed circle divided by that of the circumscribed circle.  An
//    equilateral triangle achieves the maximum possible quality of 1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    31 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Adrian Bowyer, John Woodwark,
//    A Programmer's Geometry,
//    Butterworths, 1983.
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Output, double TRIANGLE_QUALITY_2D, the quality of the triangle.
//
{
# define DIM_NUM 2

  double a;
  double b;
  double c;
  int i;
  double value;
//
//  Compute the length of each side.
//
  a = 0.0;
  b = 0.0;
  c = 0.0;

  for ( i = 0; i < DIM_NUM; i++ )
  {
    a = a + pow ( t[i+0*DIM_NUM] - t[i+1*DIM_NUM], 2 );
    b = b + pow ( t[i+1*DIM_NUM] - t[i+2*DIM_NUM], 2 );
    c = c + pow ( t[i+2*DIM_NUM] - t[i+0*DIM_NUM], 2 );
  }
  a = sqrt ( a );
  b = sqrt ( b );
  c = sqrt ( c );

  if ( a * b * c == 0.0 )
  {
    value = 0.0;
  }
  else
  {
    value = ( - a + b + c ) * ( a - b + c ) * ( a + b - c )
      / ( a * b * c );
  }

  return value;
# undef DIM_NUM
}
//****************************************************************************80

int triangle_right_lattice_point_num_2d ( int a, int b )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_RIGHT_LATTICE_POINT_NUM_2D: count lattice points.
//
//  Discussion:
//
//    The triangle is assumed to be a right triangle which, without loss
//    of generality, has the coordinates:
//
//    ( (0,0), (a,0), (0,b) )
//
//    The routine returns the number of integer lattice points that appear
//    inside the triangle or on its edges or vertices.
//
//    The formula for this function occurred to me (JVB) after some thought,
//    on 06 July 2009.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int A, B, define the vertices.
//
//    Output, int N, the number of lattice points.
//
{
  int n;

  n = ( ( a + 1 ) * ( b + 1 ) + i4_gcd ( a, b ) + 1 ) / 2;

  return n;
}
//****************************************************************************80

void triangle_sample ( double t[2*3], int n, int &seed, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_SAMPLE returns random points in a triangle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 January 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Input, int N, the number of points to sample.
//
//    Input/output, int &SEED, a seed for the random number generator.
//
//    Output, double P[2*N], a random point in the triangle.
//
{
# define DIM_NUM 2

  double alpha;
  double beta;
  int j;
  double r;
  double p12[DIM_NUM];
  double p13[DIM_NUM];

  for ( j = 0; j < n; j++ )
  {
    r = r8_uniform_01 ( seed );
//
//  Interpret R as a percentage of the triangle's area.
//
//  Imagine a line L, parallel to side 1, so that the area between
//  vertex 1 and line L is R percent of the full triangle's area.
//
//  The line L will intersect sides 2 and 3 at a fraction
//  ALPHA = SQRT ( R ) of the distance from vertex 1 to vertices 2 and 3.
//
    alpha = sqrt ( r );
//
//  Determine the coordinates of the points on sides 2 and 3 intersected
//  by line L.
//
    p12[0] = ( 1.0 - alpha ) * t[0+0*2] + alpha * t[0+1*2];
    p12[1] = ( 1.0 - alpha ) * t[1+0*2] + alpha * t[1+1*2];

    p13[0] = ( 1.0 - alpha ) * t[0+0*2] + alpha * t[0+2*2];;
    p13[1] = ( 1.0 - alpha ) * t[1+0*2] + alpha * t[1+2*2];;
//
//  Now choose, uniformly at random, a point on the line L.
//
    beta = r8_uniform_01 ( seed );

    p[0+j*2] = ( 1.0 - beta ) * p12[0] + beta * p13[0];
    p[1+j*2] = ( 1.0 - beta ) * p12[1] + beta * p13[1];
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

int triangle_unit_lattice_point_num_2d ( int s )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_UNIT_LATTICE_POINT_NUM_2D: count lattice points.
//
//  Discussion:
//
//    The triangle is assumed to be the unit triangle:
//
//    ( (0,0), (1,0), (0,1) )
//
//    or a copy of this triangle scaled by an integer S:
//
//    ( (0,0), (S,0), (0,S) ).
//
//    The routine returns the number of integer lattice points that appear
//    inside the triangle or on its edges or vertices.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2009
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Matthias Beck, Sinai Robins,
//    Computing the Continuous Discretely,
//    Springer, 2006,
//    ISBN13: 978-0387291390,
//    LC: QA640.7.B43.
//
//  Parameters:
//
//    Input, int S, the scale factor.
//
//    Output, int TRIANGLE_UNIT_LATTICE_POINT_NUM_2D, the number of lattice points.
//
{
  int n;

  n = ( ( s + 2 ) * ( s + 1 ) ) / 2;

  return n;
}

//****************************************************************************80

void triangle_xsi_to_xy_2d ( double t[2*3], double xsi[3], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_XSI_TO_XY_2D converts from barycentric to XY coordinates in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Input, double XSI[3], the barycentric coordinates of a point.
//
//    Output, double P[2], the Cartesian coordinates of the point.
//
{
# define DIM_NUM 2

  p[0] = xsi[0] * t[0+0*2] + xsi[1] * t[0+1*2] + xsi[2] * t[0+2*2];
  p[1] = xsi[0] * t[1+0*2] + xsi[1] * t[1+1*2] + xsi[2] * t[1+2*2];

  return;
# undef DIM_NUM
}
//****************************************************************************80

void triangle_xy_to_xsi_2d ( double t[2*3], double p[2], double xsi[3] )

//****************************************************************************80
//
//  Purpose:
//
//    TRIANGLE_XY_TO_XSI_2D converts from XY to barycentric in 2D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double T[2*3], the triangle vertices.
//
//    Input, double P[2], the XY coordinates of a point.
//
//    Output, double XSI[3], the barycentric coordinates of the point.
//
{
# define DIM_NUM 2

  double det;

  det = ( t[0+0*2] - t[0+2*2] ) * ( t[1+1*2] - t[1+2*2] )
      - ( t[0+1*2] - t[0+2*2] ) * ( t[1+0*2] - t[1+2*2] );

  xsi[0] = (   ( t[1+1*2] - t[1+2*2] ) * ( p[0] - t[0+2*2] )
             - ( t[0+1*2] - t[0+2*2] ) * ( p[1] - t[1+2*2] ) ) / det;

  xsi[1] = ( - ( t[1+0*2] - t[1+2*2] ) * ( p[0] - t[0+2*2] )
             + ( t[0+0*2] - t[0+2*2] ) * ( p[1] - t[1+2*2] ) ) / det;

  xsi[2] = 1.0 - xsi[0] - xsi[1];

  return;
# undef DIM_NUM
}
//****************************************************************************80

void truncated_octahedron_shape_3d ( int point_num, int face_num,
  int face_order_max, double point_coord[], int face_order[], int face_point[] )

//****************************************************************************80
//
//  Purpose:
//
//    TRUNCATED_OCTAHEDRON_SHAPE_3D describes a truncated octahedron in 3D.
//
//  Discussion:
//
//    The shape is a truncated octahedron.  There are 8 hexagons and 6
//    squares.
//
//    The truncated octahedron is an interesting shape because it
//    is "space filling".  In other words, all of 3D space can be
//    filled by a regular lattice of these shapes.
//
//    Call TRUNCATED_OCTAHEDRON_SIZE_3D to get the values of POINT_NUM,
//    FACE_NUM, and FACE_ORDER_MAX, so you can allocate space for the arrays.
//
//    For each face, the face list must be of length FACE_ORDER_MAX.
//    In cases where a face is of lower than maximum order (the
//    squares, in this case), the extra entries are listed as "-1".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    15 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int POINT_NUM, the number of points (24).
//
//    Input, int FACE_NUM, the number of faces (14).
//
//    Input, int FACE_ORDER_MAX, the maximum order of any face (6).
//
//    Output, double POINT_COORD[3*POINT_NUM], the vertices.
//
//    Output, int FACE_ORDER[FACE_NUM], the number of vertices per face.
//
//    Output, int FACE_POINT[FACE_ORDER_MAX*FACE_NUM]; FACE_POINT(I,J)
//    contains the index of the I-th point in the J-th face.  The
//    points are listed in the counter clockwise direction defined
//    by the outward normal at the face.
//
{
# define DIM_NUM 3

  static int face_order_save[14] = {
    4, 4, 4, 4, 4, 4, 6, 6, 6, 6,
    6, 6, 6, 6 };
  static int face_point_save[6*14] = {
    17, 11,  9, 15, -1, -1,
    14,  8, 10, 16, -1, -1,
    22, 24, 21, 18, -1, -1,
    12,  5,  2,  6, -1, -1,
    13, 19, 23, 20, -1, -1,
     4,  1,  3,  7, -1, -1,
    19, 13,  7,  3,  8, 14,
    15,  9,  4,  7, 13, 20,
    16, 10,  5, 12, 18, 21,
    22, 18, 12,  6, 11, 17,
    20, 23, 24, 22, 17, 15,
    14, 16, 21, 24, 23, 19,
     9, 11,  6,  2,  1,  4,
     3,  1,  2,  5, 10,  8 };
  static double point_coord_save[DIM_NUM*24] = {
    -1.5, -0.5,  0.0,
    -1.5,  0.5,  0.0,
    -1.0, -1.0, -0.70710677,
    -1.0, -1.0,  0.70710677,
    -1.0,  1.0, -0.70710677,
    -1.0,  1.0,  0.70710677,
    -0.5, -1.5,  0.0,
    -0.5, -0.5, -1.4142135,
    -0.5, -0.5,  1.4142135,
    -0.5,  0.5, -1.4142135,
    -0.5,  0.5,  1.4142135,
    -0.5,  1.5,  0.0,
     0.5, -1.5,  0.0,
     0.5, -0.5, -1.4142135,
     0.5, -0.5,  1.4142135,
     0.5,  0.5, -1.4142135,
     0.5,  0.5,  1.4142135,
     0.5,  1.5,  0.0,
     1.0, -1.0, -0.70710677,
     1.0, -1.0,  0.70710677,
     1.0,  1.0, -0.70710677,
     1.0,  1.0,  0.70710677,
     1.5, -0.5,  0.0,
     1.5,  0.5,  0.0 };

  i4vec_copy ( face_num, face_order_save, face_order );
  i4vec_copy ( face_order_max*face_num, face_point_save, face_point );
  r8vec_copy ( DIM_NUM*point_num, point_coord_save, point_coord );

  return;
# undef DIM_NUM
}
//****************************************************************************80

void truncated_octahedron_size_3d ( int *point_num, int *edge_num,
  int *face_num, int *face_order_max )

//****************************************************************************80
//
//  Purpose:
//
//    TRUNCATED_OCTAHEDRON_SIZE_3D gives "sizes" for a truncated octahedron in 3D.
//
//  Discussion:
//
//    The truncated octahedron is "space-filling".
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 July 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, int *POINT_NUM, the number of points.
//
//    Output, int *EDGE_NUM, the number of edges.
//
//    Output, int *FACE_NUM, the number of faces.
//
//    Output, int *FACE_ORDER_MAX, the maximum order of any face.
//
{
  *point_num = 24;
  *edge_num = 36;
  *face_num = 14;
  *face_order_max = 6;

  return;
}
//****************************************************************************80

void tube_2d ( double dist, int n, double p[], double p1[], double p2[] )

//****************************************************************************80
//
//  Purpose:
//
//    TUBE_2D constructs a "tube" of given width around a path in 2D.
//
//  Discussion:
//
//    The routine is given a sequence of N points, and a distance DIST.
//
//    It returns the coordinates of the corners of the top and bottom
//    of a tube of width 2*DIST, which envelopes the line connecting
//    the points.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double DIST, the radius of the tube.
//
//    Input, int N, the number of points defining the line.
//    N must be at least 2.
//
//    Input, double P[2*N], the points which comprise the broken
//    line which is to be surrounded by the tube.  Points should
//    not be immediately repeated.
//
//    Output, double P1[N], P2[N], the points P1
//    form one side of the tube, and P2 the other.
//
{
# define DIM_NUM 2

  double a;
  double b;
  double c;
  double dis1;
  double dis2;
  int j;
  double *pi;
  double *pim1;
  double *pip1;
  double p4[DIM_NUM];
  double p5[DIM_NUM];
  double temp;
//
//  Check that N is at least 3.
//
  if ( n < 3 )
  {
    cerr << "\n";
    cerr << "TUBE_2D - Fatal error!\n";
    cerr << "  N must be at least 3\n";
    cerr << "  but your input value was N = " << n << "\n";
    exit ( 1 );
  }
//
//  Check that consecutive points are distinct.
//
  for ( j = 0; j < n-1; j++ )
  {
    if ( r8vec_eq ( DIM_NUM, p+j*DIM_NUM, p+(j+1)*DIM_NUM ) )
    {
      cerr << "\n";
      cerr << "TUBE_2D - Fatal error!\n";
      cerr << "  P[1:2,J] = P[1:2,J+1] for J = " << j << "\n";
      cerr << "  P[0,J] = " << p[0+j*DIM_NUM] << "\n";
      cerr << "  P[1,J] = " << p[1+j*DIM_NUM] << "\n";
      exit ( 1 );
    }
  }

  for ( j = 1; j <= n; j++ )
  {
    if ( j == 1 )
    {
      pim1 = p + (j-1) * DIM_NUM;
    }
    else
    {
      pim1 = p + (j-2) * DIM_NUM;
    }
    pi = p + (j-1) * DIM_NUM;

    if ( j < n )
    {
      pip1 = p + j * DIM_NUM;
    }
    else
    {
      pip1 = p + ( j-1 ) * DIM_NUM;
    }

    angle_box_2d ( dist, pim1, pi, pip1, p4, p5 );

    p1[0+(j-1)*DIM_NUM] = p4[0];
    p1[1+(j-1)*DIM_NUM] = p4[1];
    p2[0+(j-1)*DIM_NUM] = p5[0];
    p2[1+(j-1)*DIM_NUM] = p5[1];
//
//  On the first and last steps, translate the corner points DIST units
//  along the line, to make an extra buffer.
//
    if ( j == 1 )
    {
      temp = sqrt ( pow ( p[0+1*DIM_NUM] - p[0+0*DIM_NUM], 2 )
                  + pow ( p[1+1*DIM_NUM] - p[1+0*DIM_NUM], 2 ) );

      p1[0+0*DIM_NUM] = p1[0+0*DIM_NUM]
        - dist * ( p[0+1*DIM_NUM] - p[0+0*DIM_NUM] ) / temp;
      p1[1+0*DIM_NUM] = p1[1+0*DIM_NUM]
        - dist * ( p[1+1*DIM_NUM] - p[1+0*DIM_NUM] ) / temp;
      p2[0+0*DIM_NUM] = p2[0+0*DIM_NUM]
        - dist * ( p[0+1*DIM_NUM] - p[0+0*DIM_NUM] ) / temp;
      p2[1+0*DIM_NUM] = p2[1+0*DIM_NUM]
        - dist * ( p[1+1*DIM_NUM] - p[1+0*DIM_NUM] ) / temp;
    }
    else if ( j == n )
    {
      temp = sqrt ( pow ( p[0+(n-1)*DIM_NUM] - p[0+(n-2)*DIM_NUM], 2 )
                  + pow ( p[1+(n-1)*DIM_NUM] - p[1+(n-2)*DIM_NUM], 2 ) );

      p1[0+(n-1)*DIM_NUM] = p1[0+(n-1)*DIM_NUM]
        + dist * ( p[0+(n-1)*DIM_NUM] - p[0+(n-2)*DIM_NUM] ) / temp;
      p1[1+(n-1)*DIM_NUM] = p1[1+(n-1)*DIM_NUM]
        + dist * ( p[1+(n-1)*DIM_NUM] - p[1+(n-2)*DIM_NUM] ) / temp;
      p2[0+(n-1)*DIM_NUM] = p2[0+(n-1)*DIM_NUM]
        + dist * ( p[0+(n-1)*DIM_NUM] - p[0+(n-2)*DIM_NUM] ) / temp;
      p2[1+(n-1)*DIM_NUM] = p2[1+(n-1)*DIM_NUM]
        + dist * ( p[1+(n-1)*DIM_NUM] - p[1+(n-2)*DIM_NUM] ) / temp;
    }
//
//  The new points may need to be swapped.
//
//  Compute the signed distance from the points to the line.
//
    if ( 1 < j )
    {
      a = p[1+(j-2)*DIM_NUM] - p[1+(j-1)*DIM_NUM];
      b = p[0+(j-1)*DIM_NUM] - p[0+(j-2)*DIM_NUM];
      c = p[0+(j-2)*DIM_NUM] * p[1+(j-1)*DIM_NUM]
        - p[0+(j-1)*DIM_NUM] * p[1+(j-2)*DIM_NUM];

      dis1 = ( a * p1[0+(j-2)*DIM_NUM] + b * p1[1+(j-2)*DIM_NUM] + c )
        / sqrt ( a * a + b * b );

      dis2 = ( a * p1[0+(j-1)*DIM_NUM] + b * p1[1+(j-1)*DIM_NUM] + c )
        / sqrt ( a * a + b * b );

      if ( r8_sign ( dis1 ) != r8_sign ( dis2 ) )
      {
        temp                = p1[0+(j-1)*DIM_NUM];
        p1[0+(j-1)*DIM_NUM] = p2[0+(j-1)*DIM_NUM];
        p2[0+(j-1)*DIM_NUM] = temp;
        temp                = p1[1+(j-1)*DIM_NUM];
        p1[1+(j-1)*DIM_NUM] = p2[1+(j-1)*DIM_NUM];
        p2[1+(j-1)*DIM_NUM] = temp;
      }
    }
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

void tuple_next2 ( int n, int xmin[], int xmax[], int x[], int *rank )

//****************************************************************************80
//
//  Purpose:
//
//    TUPLE_NEXT2 computes the next element of an integer tuple space.
//
//  Discussion:
//
//    The elements X are N vectors.
//
//    Each entry X(I) is constrained to lie between XMIN(I) and XMAX(I).
//
//    The elements are produced one at a time.
//
//    The first element is
//      (XMIN(1), XMIN(2), ..., XMIN(N)),
//    the second is (probably)
//      (XMIN(1), XMIN(2), ..., XMIN(N)+1),
//    and the last element is
//      (XMAX(1), XMAX(2), ..., XMAX(N))
//
//    Intermediate elements are produced in a lexicographic order, with
//    the first index more important than the last, and the ordering of
//    values at a fixed index implicitly defined by the sign of
//    XMAX(I) - XMIN(I).
//
//  Example:
//
//    N = 2,
//    XMIN = (/ 1, 10 /)
//    XMAX = (/ 3,  8 /)
//
//    RANK    X
//    ----  -----
//      1   1 10
//      2   1  9
//      3   1  8
//      4   2 10
//      5   2  9
//      6   2  8
//      7   3 10
//      8   3  9
//      9   3  8
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 April 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of components.
//
//    Input, int XMIN[N], XMAX[N], the "minimum" and "maximum" entry values.
//    These values are minimum and maximum only in the sense of the lexicographic
//    ordering.  In fact, XMIN(I) may be less than, equal to, or greater
//    than XMAX(I).
//
//    Input/output, int X[N], on input the previous tuple.
//    On output, the next tuple.
//
//    Input/output, int *RANK, the rank of the item.  On first call,
//    set RANK to 0 to start up the sequence.  On return, if RANK is zero,
//    there are no more items in the sequence.
//
{
  int i;
  int test;

  if ( *rank < 0 )
  {
    cerr << "\n";
    cerr << "TUPLE_NEXT2 - Fatal error!\n";
    cerr << "  Illegal value of RANK = " << *rank << "\n";
    exit ( 1 );
  }

  test = 1;
  for ( i = 0; i < n; i++ )
  {
    test = test * ( 1 + abs ( xmax[i] - xmin[i] ) );
  }

  if ( test < *rank )
  {
    cerr << "\n";
    cerr << "TUPLE_NEXT2 - Fatal error!\n";
    cerr << "  Illegal value of RANK = " << *rank << "\n";
    exit ( 1 );
  }

  if ( *rank == 0 )
  {
    for ( i = 0; i < n; i++ )
    {
      x[i] = xmin[i];
    }
    *rank = 1;
    return;
  }

  *rank = *rank + 1;
  i = n - 1;

  for ( ; ; )
  {
    if ( x[i] != xmax[i] )
    {
      if ( xmin[i] < xmax[i] )
      {
        x[i] = x[i] + 1;
      }
      else
      {
        x[i] = x[i] - 1;
      }
      break;
    }

    x[i] = xmin[i];

    if ( i == 0 )
    {
      *rank = 0;
      break;
    }

    i = i - 1;

  }

  return;
}
//****************************************************************************80

void vector_directions_nd ( int dim_num, double v[], double angle[] )

//****************************************************************************80
//
//  Purpose:
//
//    VECTOR_DIRECTIONS_ND returns the direction angles of a vector in ND.
//
//  Discussion:
//
//    Let V be the vector, and let E(I) be the I-th unit coordinate axis vector.
//    The I-th direction angle is the angle between V and E(I), which is
//    the angle whose cosine is equal to the direction cosine:
//
//      Direction_Cosine(I) = V dot E(I) / |V|.
//
//    If V is the null or zero vector, then the direction cosines and
//    direction angles are undefined, and this routine simply returns
//    zeroes.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, double V[DIM_NUM], the vector.
//
//    Output, double ANGLE[DIM_NUM], the direction angles, in radians,
//    that the vector V makes with the coordinate axes.
//
{
  int i;
  double vnorm;
//
//  Get the norm of the vector.
//
  vnorm = r8vec_norm ( dim_num, v );

  if ( vnorm == 0.0 )
  {
    r8vec_zero ( dim_num, angle );
    return;
  }

  for ( i = 0; i < dim_num; i++ )
  {
    angle[i] = acos ( v[i] / vnorm );
  }

  return;
}
//****************************************************************************80

void vector_rotate_2d ( double v1[2], double angle, double v2[2] )

//****************************************************************************80
//
//  Purpose:
//
//    VECTOR_ROTATE_2D rotates a vector around the origin in 2D.
//
//  Discussion:
//
//    To see why this formula is so, consider that the original point
//    has the form ( R cos Theta, R sin Theta ), and the rotated point
//    has the form ( R cos ( Theta + Angle ), R sin ( Theta + Angle ) ).
//    Now use the addition formulas for cosine and sine to relate
//    the new point to the old one:
//
//      ( X2 ) = ( cos Angle  - sin Angle ) * ( X1 )
//      ( Y2 )   ( sin Angle    cos Angle )   ( Y1 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V1[2], the vector to be rotated.
//
//    Input, double ANGLE, the angle, in radians, of the rotation to be
//    carried out.  A positive angle rotates the vector in the
//    counter clockwise direction.
//
//    Output, double V2[2], the rotated vector.
//
{
  v2[0] = cos ( angle ) * v1[0] - sin ( angle ) * v1[1];
  v2[1] = sin ( angle ) * v1[0] + cos ( angle ) * v1[1];

  return;
}
//****************************************************************************80

void vector_rotate_3d ( double p1[3], double pa[3], double angle, double p2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    VECTOR_ROTATE_3D rotates a vector around an axis vector in 3D.
//
//  Discussion:
//
//    Thanks to Cody Farnell for correcting some errors in a previous
//    version of this routine!
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    18 May 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[3], the components of the vector to be rotated.
//
//    Input, double PA[3], the vector about which the rotation is to
//    be carried out.
//
//    Input, double ANGLE, the angle, in radians, of the rotation to be
//    carried out.
//
//    Output, double P2[3], the rotated vector.
//
{
# define DIM_NUM 3

  double axis_norm;
  double dot;
  double normn;
  double pn[DIM_NUM];
  double *pn2;
  double pp[DIM_NUM];
  double pr[DIM_NUM];
//
//  Compute the length of the rotation axis.
//
  axis_norm = r8vec_norm ( DIM_NUM, pa );

  if ( axis_norm == 0.0 )
  {
    r8vec_copy ( DIM_NUM, p1, p2 );
    return;
  }
//
//  Compute the dot product of the vector and the (unit) rotation axis.
//
  dot = r8vec_dot_product ( DIM_NUM, p1, pa ) / axis_norm;
//
//  Compute the parallel component of the vector.
//
  pp[0] = dot * pa[0] / axis_norm;
  pp[1] = dot * pa[1] / axis_norm;
  pp[2] = dot * pa[2] / axis_norm;
//
//  Compute the normal component of the vector.
//
  pn[0] = p1[0] - pp[0];
  pn[1] = p1[1] - pp[1];
  pn[2] = p1[2] - pp[2];

  normn = r8vec_norm ( DIM_NUM, pn );

  if ( normn == 0.0 )
  {
    r8vec_copy ( DIM_NUM, pp, p2 );
    return;
  }

  vector_unit_nd ( 3, pn );
//
//  Compute a second vector, lying in the plane, perpendicular
//  to P1, and forming a right-handed system...
//
  pn2 = r8vec_cross_product_3d ( pa, pn );

  vector_unit_nd ( 3, pn2 );
//
//  Rotate the normal component by the angle.
//
  pr[0] = normn * ( cos ( angle ) * pn[0] + sin ( angle ) * pn2[0] );
  pr[1] = normn * ( cos ( angle ) * pn[1] + sin ( angle ) * pn2[1] );
  pr[2] = normn * ( cos ( angle ) * pn[2] + sin ( angle ) * pn2[2] );

  delete [] pn2;
//
//  The rotated vector is the parallel component plus the rotated
//  component.
//
  p2[0] = pp[0] + pr[0];
  p2[1] = pp[1] + pr[1];
  p2[2] = pp[2] + pr[2];

  return;
# undef DIM_NUM
}
//****************************************************************************80

void vector_rotate_base_2d ( double p1[2], double pb[2], double angle,
  double p2[2] )

//****************************************************************************80
//
//  Purpose:
//
//    VECTOR_ROTATE_BASE_2D rotates a vector around a base point in 2D.
//
//  Discussion:
//
//    The original vector is assumed to be P1-PB, and the
//    rotated vector is P2-PB.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    30 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P1[2], the endpoint of the original vector.
//
//    Input, double PB[2], the location of the base point.
//
//    Input, double ANGLE, the angle, in radians, of the rotation to be
//    carried out.  A positive angle rotates the vector in the
//    counter clockwise direction.
//
//    Output, double P2[2], the endpoint of the rotated vector.
//
{
  p2[0] = pb[0] + cos ( angle ) * ( p1[0] - pb[0] )
                - sin ( angle ) * ( p1[1] - pb[1] );
  p2[1] = pb[1] + sin ( angle ) * ( p1[0] - pb[0] )
                + cos ( angle ) * ( p1[1] - pb[1] );

  return;
}
//****************************************************************************80

double vector_separation_2d ( double v1[], double v2[] )

//****************************************************************************80
//
//  Purpose:
//
//    VECTOR_SEPARATION_2D finds the angular separation between vectors in 2D.
//
//  Discussion:
//
//    Any two vectors lie in a plane, and are separated by a plane angle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V1[2], V2[2], the two vectors.
//
//    Output, double VECTOR_SEPARATION_2D, the angle between the two vectors.
//
{
# define DIM_NUM 2

  double cos_theta;
  double v1_norm;
  double v2_norm;

  v1_norm = r8vec_norm ( DIM_NUM, v1 );
  v2_norm = r8vec_norm ( DIM_NUM, v2 );

  cos_theta = r8vec_dot_product ( DIM_NUM, v1, v2 ) / ( v1_norm * v2_norm );

  return ( r8_acos ( cos_theta ) );
# undef DIM_NUM
}
//****************************************************************************80

double vector_separation_3d ( double v1[], double v2[] )

//****************************************************************************80
//
//  Purpose:
//
//    VECTOR_SEPARATION_3D finds the angular separation between vectors in 3D.
//
//  Discussion:
//
//    Any two vectors lie in a plane, and are separated by a plane angle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double V1[3], V2[3], the two vectors.
//
//    Output, double VECTOR_SEPARATION_3D, the angle between the two vectors.
//
{
# define DIM_NUM 3

  double cos_theta;
  double v1_norm;
  double v2_norm;

  v1_norm = r8vec_norm ( DIM_NUM, v1 );
  v2_norm = r8vec_norm ( DIM_NUM, v2 );

  cos_theta = r8vec_dot_product ( DIM_NUM, v1, v2 ) / ( v1_norm * v2_norm );

  return ( r8_acos ( cos_theta ) );
# undef DIM_NUM
}
//****************************************************************************80

double vector_separation_nd ( int dim_num, double v1[], double v2[] )

//****************************************************************************80
//
//  Purpose:
//
//    VECTOR_SEPARATION_ND finds the angular separation between vectors in ND.
//
//  Discussion:
//
//    Any two vectors lie in a plane, and are separated by a plane angle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the vectors.
//
//    Input, double V1[DIM_NUM], V2[DIM_NUM], the two vectors.
//
//    Output, double VECTOR_SEPARATION_ND, the angle between the two vectors.
//
{
  double cos_theta;
  double v1_norm;
  double v2_norm;

  v1_norm = r8vec_norm ( dim_num, v1 );
  v2_norm = r8vec_norm ( dim_num, v2 );
  cos_theta = r8vec_dot_product ( dim_num, v1, v2 ) / ( v1_norm * v2_norm );

  return ( r8_acos ( cos_theta ) );
}
//****************************************************************************80

void vector_unit_nd ( int dim_num, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    VECTOR_UNIT_ND normalizes a vector in ND.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    29 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the dimension of the vector.
//
//    Input/output, double P[DIM_NUM], the vector to be normalized.  On output,
//    the vector should have unit Euclidean norm.
//    However, if the input vector has zero Euclidean norm, it is
//    not altered.
//
{
  int i;
  double norm;

  norm = r8vec_norm ( dim_num, p );

  if ( norm != 0.0 )
  {
    for ( i = 0; i < dim_num; i++ )
    {
      p[i] = p[i] / norm;
    }
  }

  return;
}
//****************************************************************************80

int voxels_dist_l1_3d ( int v1[3], int v2[3] )

//****************************************************************************80
//
//  Purpose:
//
//    VOXELS_DIST_L1_3D computes the L1 distance between voxels in 3D.
//
//  Discussion:
//
//    We can imagine that, in traveling from (X1,Y1,Z1) to (X2,Y2,Z2),
//    we are allowed to increment or decrement just one coordinate at
//    at time.  The minimum number of such changes required is the
//    L1 distance.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int V1[3], the voxel that begins the line.
//
//    Input, int V2[3], the voxel that ends the line.
//
//    Output, int VOXELS_DIST_L1_3D, the L1 distance between the voxels.
//
{
  int value;

  value = ( abs ( v1[0] - v2[0] )
          + abs ( v1[1] - v2[1] )
          + abs ( v1[2] - v2[2] ) );

  return value;
}
//****************************************************************************80

int voxels_dist_l1_nd ( int dim_num, int v1[], int v2[] )

//****************************************************************************80
//
//  Purpose:
//
//    VOXELS_DIST_L1_ND computes the L1 distance between voxels in ND.
//
//  Discussion:
//
//    A voxel is generally a point in 3D space with integer coordinates.
//    There's no reason to stick with 3D, so this routine will handle
//    any dimension.
//
//    We can imagine that, in traveling from V1 to V2, we are allowed to
//    increment or decrement just one coordinate at a time.  The minimum number
//    of such changes required is the L1 distance.
//
//    More formally,
//
//      DIST_L1 ( V1, V2 ) = sum ( 1 <= I <= N ) | V1(I) - V2(I) |
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int DIM_NUM, the spatial dimension.
//
//    Input, int V1[DIM_NUM], the voxel that begins the line.
//
//    Input, int V2[DIM_NUM], the voxel that ends the line.
//
//    Output, int VOXELS_DIST_L1_ND, the L1 distance between the voxels.
//
{
  int i;
  int value;

  value = 0;
  for ( i = 0; i < dim_num; i++ )
  {
    value = value + abs ( v1[i] - v2[i] );
  }

  return value;
}
//****************************************************************************80

void voxels_line_3d ( int v1[3], int v2[3], int n, int v[] )

//****************************************************************************80
//
//  Purpose:
//
//    VOXELS_LINE_3D computes voxels along a line in 3D.
//
//  Discussion:
//
//    The line itself is defined by two voxels.  The line will begin
//    at the first voxel, and move towards the second.  If the value of
//    N is equal to the L1 distance between the two voxels, then the
//    line will "almost" reach the second voxel.  Depending on the
//    direction, 1, 2 or 3 more steps may be needed.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Daniel Cohen,
//    Voxel Traversal along a 3D Line,
//    Graphics Gems IV,
//    edited by Paul Heckbert,
//    AP Professional, 1994, T385.G6974.
//
//  Parameters:
//
//    Input, int V1[3], the voxel that begins the line.
//
//    Input, int V2[3], the voxel that ends the line.
//
//    Input, int N, the number of voxels to compute.
//
//    Output, int V[3*N], a sequence of voxels, whose
//    first value is V1 and which proceeds towards V2.
//
{
# define DIM_NUM 3

  int a[DIM_NUM];
  int exy;
  int exz;
  int ezy;
  int i;
  int j;
  int s[DIM_NUM];

  if ( n <= 0 )
  {
    return;
  }
//
//  Determine the number of voxels on the line.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    if ( v2[i] < v1[i] )
    {
      s[i] = -1;
    }
    else
    {
      s[i] = +1;
    }
    a[i] = abs ( v2[i] - v1[i] );
  }

  exy = a[1] - a[0];
  exz = a[2] - a[0];
  ezy = a[1] - a[2];
//
//  We start at the starting point.
//
  for ( i = 0; i < DIM_NUM; i++ )
  {
    v[i+0*DIM_NUM] = v1[i];
  }

  for ( j = 1; j < n; j++ )
  {
    for ( i = 0; i < DIM_NUM; i++ )
    {
      v[i+j*DIM_NUM] = v[i+(j-1)*DIM_NUM];
    }

    if ( exy < 0 )
    {
      if ( exz < 0 )
      {
        v[0+j*DIM_NUM] = v[0+j*DIM_NUM] + s[0];
        exy = exy + 2 * a[1];
        exz = exz + 2 * a[2];
      }
      else
      {
        v[2+j*DIM_NUM] = v[2+j*DIM_NUM] + s[2];
        exz = exz - 2 * a[0];
        ezy = ezy + 2 * a[1];
      }
    }
    else if ( ezy < 0 )
    {
      v[2+j*DIM_NUM] = v[2+j*DIM_NUM] + s[2];
      exz = exz - 2 * a[0];
      ezy = ezy + 2 * a[1];
    }
    else
    {
      v[1+j*DIM_NUM] = v[1+j*DIM_NUM] + s[1];
      exy = exy - 2 * a[0];
      ezy = ezy - 2 * a[2];
    }
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

void voxels_region_3d ( int list_max, int nx, int ny, int nz, int ishow[],
  int *list_num, int list[], int *region_num )

//****************************************************************************80
//
//  Purpose:
//
//    VOXELS_REGION_3D arranges a set of voxels into contiguous regions in 3D.
//
//  Discussion:
//
//    On input, the ISHOW array contains zero and nonzero values.  The nonzero
//    values are taken to be active voxels.  On output, the zero voxels remain
//    zero, and all the active voxels have been assigned a value which now
//    indicates membership in a region, or group of contiguous voxels.
//
//    On output, the array LIST contains information about the regions.
//    The last used element of LIST is LIST_NUM.
//
//    The number of elements in region REGION_NUM is NELEM = LIST(LIST_NUM).
//    The (I,J,K) indices of the last element in this region are in
//    LIST(LIST_NUM-3) through LIST(LIST_NUM-1), and the first element is
//    listed in LIST(LIST_NUM-3*NELEM), LIST(LIST_NUM-3*NELEM+1),
//    LIST(LIST_NUM-3*NELEM+2).
//
//    The number of elements in REGION_NUM-1 is listed in LIST(LIST_NUM-3*NELEM-1),
//    and the (I,J,K) indices of the these elements are listed there.
//
//  Picture:
//
//    Input:
//
//      0  2  0  0 17  0  3
//      0  0  3  0  1  0  4
//      1  0  4  8  8  0  7
//      3  0  6 45  0  0  0
//      3 17  0  5  9  2  5
//
//    Output:
//
//      0  1  0  0  2  0  3
//      0  0  2  0  2  0  3
//      4  0  2  2  2  0  3
//      4  0  2  2  0  0  0
//      4  4  0  2  2  2  2
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input/output, int ISHOW[NX*NY*NZ].  On input, the only significance to
//    the entries is whether they are zero or nonzero.  On output, the nonzero
//    entries have now been revalued so that contiguous entries have the same
//    value, indicating a grouping into a region.
//
//    Output, int LIST[LIST_MAX], contains, in stack form, a list
//    of the indices of the elements in each region.
//
//    Input, int LIST_MAX, the maximum length of the array used to
//    list the elements of the regions.
//
//    Output, int *LIST_NUM, the number of entries of LIST that were used.
//    However, if LIST_MAX < LIST_NUM, then there was not enough space in
//    LIST to store the data properly, and LIST should not be used,
//    although the data in ISHOW should be correct.
//
//    Output, int *REGION_NUM, the number of regions discovered.
//
//    Input, int NX, NY, NZ, the number of voxels in the X, Y and
//    Z directions.
//
{
# define STACK_MAX 100

  int i;
  int i2;
  int ibase;
  int ihi;
  int ilo;
  int j;
  int j2;
  int jbase;
  int jhi;
  int jlo;
  int k;
  int k2;
  int kbase;
  int khi;
  int klo;
  int nabes;
  int ncan;
  int nelements;
  int nstack;
  int stack[STACK_MAX];
//
//  Reset all nonzero entries of ISHOW to -1.
//
  for ( k = 0; k < nz; k++ )
  {
    for ( j = 0; j < ny; j++ )
    {
      for ( i = 0; i < nx; i++ )
      {
        if ( ishow[i+nx*(j+ny*k)] != 0 )
        {
          ishow[i+nx*(j+ny*k)] = -1;
        }
      }
    }
  }
//
//  Start the number of items in the region list at 0.
//
  *list_num = 0;
//
//  Start the number of regions at 0.
//
  *region_num = 0;
//
//  The stack begins empty.
//
  nstack = 0;
//
//  Search for an unused "ON" voxel from which we can "grow" a new region.
//
  for ( k = 1; k <= nz; k++ )
  {
    for ( j = 1; j <= ny; j++ )
    {
      for ( i = 1; i <= nx; i++ )
      {
//
//  We found a voxel that is "ON", and does not belong to any region.
//
        if ( ishow[i-1+nx*(j-1+ny*(k-1))] == -1 )
        {
//
//  Increase the number of regions.
//
          *region_num = *region_num + 1;
//
//  Add this voxel to the region.
//
          ishow[i-1+nx*(j-1+ny*(k-1))] = *region_num;
//
//  Add this voxel to the stack.
//
          if ( STACK_MAX < nstack + 4 )
          {
            cerr << "\n";
            cerr << "VOXELS_REGION - Fatal error!\n";
            cerr << "  The internal stack overflowed.\n";
            cerr << "  The algorithm has failed.\n";
            exit ( 1 );
          }

          stack[nstack+1-1] = i;
          stack[nstack+2-1] = j;
          stack[nstack+3-1] = k;
          stack[nstack+4-1] = 1;

          nstack = nstack + 4;
//
//  Add this voxel to the description of the region.
//
          nelements = 1;

          if ( *list_num + 3 <= list_max )
          {
            list[*list_num+1-1] = i;
            list[*list_num+2-1] = j;
            list[*list_num+3-1] = k;
          }

          *list_num = *list_num + 3;

          for ( ; ; )
          {
//
//  Find all neighbors of BASE that are "ON" but unused.
//  Mark them as belonging to this region, and stack their indices.
//
            ibase = stack[nstack-3-1];
            jbase = stack[nstack-2-1];
            kbase = stack[nstack-1-1];

            ilo = i4_max ( ibase-1, 1 );
            ihi = i4_min ( ibase+1, nx );
            jlo = i4_max ( jbase-1, 1 );
            jhi = i4_min ( jbase+1, ny );
            klo = i4_max ( kbase-1, 1 );
            khi = i4_min ( kbase+1, nz );

            nabes = 0;

            for ( k2 = klo; k2 <= khi; k2++ )
            {
              for ( j2 = jlo; j2 <= jhi; j2++ )
              {
                for ( i2 = ilo; i2 <= ihi; i2++ )
                {
//
//  We found a neighbor to our current search point, which is "ON" and unused.
//
                  if ( ishow[i2-1+nx*(j2-1+ny*(k2-1))] == -1 )
                  {
//
//  Increase the number of neighbors.
//
                    nabes = nabes + 1;
//
//  Mark the neighbor as belonging to the region.
//
                    ishow[i2-1+nx*(j2-1+ny*(k2-1))] = *region_num;
//
//  Add the neighbor to the stack.
//
                    if ( STACK_MAX < nstack + 3 )
                    {
                      cerr << "\n";
                      cerr << "VOXELS_REGION - Fatal error!\n";
                      cerr << "  The internal stack overflowed.\n";
                      cerr << "  The algorithm has failed.\n";
                      exit ( 1 );
                    }

                    stack[nstack+1-1] = i2;
                    stack[nstack+2-1] = j2;
                    stack[nstack+3-1] = k2;

                    nstack = nstack + 3;
//
//  Add the neighbor to the description of the region.
//
                    nelements = nelements + 1;

                    if ( *list_num+3 <= list_max )
                    {
                      list[*list_num+1-1] = i2;
                      list[*list_num+2-1] = j2;
                      list[*list_num+3-1] = k2;
                    }

                    *list_num = *list_num + 3;
                  }
                }
              }
            }
//
//  If any new neighbors were found, take the last one as the basis
//  for a deeper search.
//
            if ( 0 < nabes )
            {
              if ( STACK_MAX < nstack + 1 )
              {
                cerr << "\n";
                cerr << "VOXELS_REGION - Fatal error!\n";
                cerr << "  The internal stack overflowed.\n";
                cerr << "  The algorithm has failed.\n";
                exit ( 1 );
              }

              stack[nstack+1-1] = nabes;
              nstack = nstack + 1;
              continue;
            }
//
//  If the current search point had no new neighbors, drop it from the stack.
//
            ncan = stack[nstack-1] - 1;
            nstack = nstack - 3;
            stack[nstack-1] = ncan;
//
//  If there are still any unused candidates at this level, take the
//  last one as the basis for a deeper search.
//
            if ( 0 < stack[nstack-1] )
            {
              continue;
            }
//
//  If there are no more unused candidates at this level, then we need
//  to back up a level in the stack.  If there are any candidates at
//  that earlier level, then we can still do more searching.
//
            nstack = nstack - 1;

            if ( nstack <= 0 )
            {
              break;
            }
          }
//
//  If we have exhausted the stack, we have completed this region.
//  Tag the number of elements to the end of the region description list.
//
          *list_num = *list_num + 1;
          if ( *list_num <= list_max )
          {
            list[*list_num-1] = nelements;
          }
        }
      }
    }
  }
//
//  Print some warnings.
//
  if ( list_max < *list_num )
  {
    cout << "\n";
    cout << "VOXELS_REGION - Warning!\n";
    cout << "  LIST_MAX was too small to list the regions.\n";
    cout << "  Do not try to use the LIST array!\n";
    cout << "  The ISHOW data is OK, however.\n";
  }

  return;
# undef STACK_MAX
}
//****************************************************************************80

void voxels_step_3d ( int v1[3], int v2[3], int inc, int jnc, int knc,
  int v3[3] )

//****************************************************************************80
//
//  Purpose:
//
//    VOXELS_STEP_3D computes voxels along a line from a given point in 3D.
//
//  Discussion:
//
//    If you input INC = JNC = KNC, then no movement is possible,
//    and none is made.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 August 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int V1[3], the coordinates of the base voxel from
//    which the line begins.
//
//    Input, int V2[3], the coordinates of the current voxel on the
//    line.  For the first call, these might be equal to V1.
//
//    Input, int INC, JNC, KNC, the increments to the voxels.
//    These values define the direction along which the line proceeds.
//    However, the voxels on the line will typically be incremented
//    by a fractional value of the vector (INC,JNC,KNC), and the
//    result is essentially rounded.
//
//    Output, int V3[3], the coordinates of the next voxel along
//    the line.
//
{
# define DIM_NUM 3

  double alpha = 0.0;
  double alphai = 0.0;
  double alphaj = 0.0;
  double alphak = 0.0;

  i4vec_copy ( DIM_NUM, v2, v3 );
//
//  Assuming for the moment that (I,J,K) can take on real values,
//  points on the line have the form:
//
//    I = V1[0] + alpha * inc
//    J = V1[1] + alpha * jnc
//    K = V1[2] + alpha * knc
//
  if ( inc == 0 && jnc == 0 && knc == 0 )
  {
    return;
  }

  alpha = 0.0;
//
//  Compute the smallest ALPHA that will change I2, J2 or K2 by +-0.5.
//
  if ( 0 < inc )
  {
    alphai = ( ( double ) ( v2[0] - v1[0] ) + 0.5 ) / ( ( double ) inc );
  }
  else if ( inc < 0 )
  {
    alphai = ( ( double ) ( v2[0] - v1[0] ) - 0.5 ) / ( ( double ) inc );
  }
  else
  {
    alphai = HUGE_VAL;
  }

  if ( 0 < jnc )
  {
    alphaj = ( ( double ) ( v2[1] - v1[1] ) + 0.5 ) / ( ( double ) jnc );
  }
  else if ( jnc < 0 )
  {
    alphaj = ( ( double ) ( v2[1] - v1[1] ) - 0.5 ) / ( ( double ) jnc );
  }
  else
  {
    alphaj = HUGE_VAL;
  }

  if ( 0 < knc )
  {
    alphak = ( ( double ) ( v2[2] - v1[2] ) + 0.5 ) / ( ( double ) knc );
  }
  else if ( knc < 0 )
  {
    alphak = ( ( double ) ( v2[2] - v1[2] ) - 0.5 ) / ( ( double ) knc );
  }
  else
  {
    alphaj = HUGE_VAL;
  }
//
//  The ALPHA of smallest positive magnitude represents the closest next voxel.
//
  alpha = HUGE_VAL;

  if ( 0.0 < alphai )
  {
    alpha = fmin ( alpha, alphai );
  }

  if ( 0.0 < alphaj )
  {
    alpha = fmin ( alpha, alphaj );
  }

  if ( 0.0 < alphak )
  {
    alpha = fmin ( alpha, alphak );
  }
//
//  Move to the new voxel.  Whichever index just made the half
//  step must be forced to take a whole step.
//
  if ( alpha == alphai )
  {
    v3[0] = v2[0] + i4_sign ( inc );
    v3[1] = v1[1] + r8_nint ( alpha * ( double ) jnc );
    v3[2] = v1[2] + r8_nint ( alpha * ( double ) knc );
  }
  else if ( alpha == alphaj )
  {
    v3[0] = v1[0] + r8_nint ( alpha * ( double ) inc );
    v3[1] = v2[1] + i4_sign ( jnc );
    v3[2] = v1[2] + r8_nint ( alpha * ( double ) knc );
  }
  else if ( alpha == alphak )
  {
    v3[0] = v1[0] + r8_nint ( alpha * ( double ) inc );
    v3[1] = v1[1] + r8_nint ( alpha * ( double ) jnc );
    v3[2] = v2[2] + i4_sign ( knc );
  }

  return;
# undef DIM_NUM
}
//****************************************************************************80

double wedge01_volume ( )

//****************************************************************************80
//
//  Purpose:
//
//    WEDGE01_VOLUME returns the volume of the unit wedge in 3D.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    18 January 2018
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, double WEDGE01_VOLUME, the volume.
//
{
  double volume;
  
  volume = 1.0;

  return volume;
}
//****************************************************************************80

void xy_to_polar ( double xy[2], double *r, double *t )

//****************************************************************************80
//
//  Purpose:
//
//    XY_TO_POLAR converts XY coordinates to polar coordinates.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 January 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double XY[2], the Cartesian coordinates.
//
//    Output, double *R, *T, the radius and angle (in radians).
//
{
  *r = sqrt ( xy[0] * xy[0] + xy[1] * xy[1] );

  if ( *r == 0.0 )
  {
    *t = 0.0;
  }
  else
  {
    *t = atan2 ( xy[0], xy[1] );
  }

  return;
}
//****************************************************************************80

void xyz_to_radec ( double p[3], double *ra, double *dec )

//****************************************************************************80
//
//  Purpose:
//
//    XYZ_TO_RADEC converts (X,Y,Z) to right ascension/declination coordinates.
//
//  Discussion:
//
//    Given an XYZ point, compute its distance R from the origin, and
//    regard it as lying on a sphere of radius R, whose axis is the Z
//    axis.
//
//    The right ascension of the point is the "longitude", measured in hours,
//    between 0 and 24, with the X axis having right ascension 0, and the
//    Y axis having right ascension 6.
//
//    Declination measures the angle from the equator towards the north pole,
//    and ranges from -90 (South Pole) to 90 (North Pole).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    28 June 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double P[3], the coordinates of a point in 3D.
//
//    Output, double *RA, *DEC, the corresponding right ascension
//    and declination.
//
{
# define DIM_NUM 3

  double norm_v;
  double phi;
  double theta;

  norm_v = r8vec_norm ( DIM_NUM, p );

  phi = asin ( p[2] / norm_v );

  if ( cos ( phi ) == 0.0 )
  {
    theta = 0.0;
  }
  else
  {
    theta = r8_atan ( p[1], p[0] );
  }

  *dec = radians_to_degrees ( phi );
  *ra = radians_to_degrees ( theta ) / 15.0;

  return;
# undef DIM_NUM
}
//****************************************************************************80

void xyz_to_rtp ( double xyz[3], double *r, double *theta, double *phi )

//****************************************************************************80
//
//  Purpose:
//
//    XYZ_TO_RTP converts (X,Y,Z) to (R,Theta,Phi) coordinates.
//
//  Discussion:
//
//    Given an XYZ point, compute its distance R from the origin, and
//    regard it as lying on a sphere of radius R, whose axis is the Z
//    axis.
//
//    Theta measures the "longitude" of the point, between 0 and 2 PI.
//
//    PHI measures the angle from the "north pole", between 0 and PI.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 January 2007
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double XYZ[3], the coordinates of a point in 3D.
//
//    Output, double *R, *THETA, *PHI, the radius, longitude and
//    declination of the point.
//
{
  *r = sqrt ( pow ( xyz[0], 2 )
            + pow ( xyz[1], 2 )
            + pow ( xyz[2], 2 ) );

  if ( *r == 0.0 )
  {
    *theta = 0.0;
    *phi = 0.0;
    return;
  }

  *phi = r8_acos ( xyz[2] / *r );

  *theta = r8_atan ( xyz[1], xyz[0] );

  return;
}
//****************************************************************************80

void xyz_to_tp ( double xyz[3], double *theta, double *phi )

//****************************************************************************80
//
//  Purpose:
//
//    XYZ_TO_TP converts (X,Y,Z) to (Theta,Phi) coordinates.
//
//  Discussion:
//
//    Given an XYZ point, regard it as lying on a sphere of radius R,
//    centered at the origin, whose axis is the Z axis.
//
//    We assume that the actual value of R is of no interest, and do
//    not report it.  This is especially appropriate if the point is
//    expected to lie on the unit sphere, for instance.
//
//    THETA measures the "longitude" of the point, between 0 and 2 PI.
//
//    PHI measures the angle from the "north pole", between 0 and PI.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 September 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double XYZ[3], the coordinates of a point in 3D.
//
//    Output, double *THETA, *PHI, the longitude and declination of the point.
//
{
  double r;

  r = sqrt ( pow ( xyz[0], 2 )
           + pow ( xyz[1], 2 )
           + pow ( xyz[2], 2 ) );

  if ( r == 0.0 )
  {
    *theta = 0.0;
    *phi = 0.0;
    return;
  }

  *phi = r8_acos ( xyz[2] / r );

  *theta = r8_atan ( xyz[1], xyz[0] );

  return;
}
