# include <cfloat>
# include <cmath>
# include <cstdlib>
# include <ctime>
# include <cstring>
# include <iostream>
# include <iomanip>

using namespace std;

# include "polygon.hpp"

//****************************************************************************80

double *angle_half ( double p1[2], double p2[2], double p3[2] )

//****************************************************************************80
//
//  Purpose:
//
//    angle_half() finds half an angle.
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
//  Input:
//
//    double P1[2], P2[2], P3[2], points defining the angle.
//
//  Output:
//
//    double ANGLE_HALF[2], a point P4 defining the half angle.
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

double angle_rad ( double p1[2], double p2[2], double p3[2] )

//****************************************************************************80
//
//  Purpose:
//
//    angle_rad() returns the angle in radians swept out between two rays.
//
//  Discussion:
//
//      ANGLE_RAD ( P1, P2, P3 ) + ANGLE_RAD ( P3, P2, P1 ) = 2 * PI
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
//  Input:
//
//    double P1[2], P2[2], P3[2], define the rays
//    P1 - P2 and P3 - P2 which define the angle.
//
//  Output:
//
//    double ANGLE_RAD, the angle between the two rays,
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

bool between ( double xa, double ya, double xb, double yb, double xc, 
  double yc )

//****************************************************************************80
//
//  Purpose:
//
//    between() is TRUE if vertex C is between vertices A and B.
//
//  Discussion:
//
//    The points must be (numerically) collinear.
//
//    Given that condition, we take the greater of XA - XB and YA - YB
//    as a "scale" and check where C's value lies.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 May 2014
//
//  Author:
//
//    Original C version by Joseph ORourke.
//    This C++ version by John Burkardt.
//
//  Reference:
//
//    Joseph ORourke,
//    Computational Geometry in C,
//    Cambridge, 1998,
//    ISBN: 0521649765,
//    LC: QA448.D38.
//
//  Input:
//
//    double XA, YA, XB, YB, XC, YC, the coordinates of 
//    the vertices.
//
//  Output:
//
//    bool BETWEEN, is TRUE if C is between A and B.
//
{
  bool value;
  double xmax;
  double xmin;
  double ymax;
  double ymin;

  if ( ! collinear ( xa, ya, xb, yb, xc, yc ) )
  {
    value = false;
  }
  else if ( fabs ( ya - yb ) < fabs ( xa - xb ) )
  {
    xmax = fmax ( xa, xb );
    xmin = fmin ( xa, xb );
    value = ( xmin <= xc && xc <= xmax );
  }
  else
  {
    ymax = fmax ( ya, yb );
    ymin = fmin ( ya, yb );
    value = ( ymin <= yc && yc <= ymax );
  }

  return value;
}
//****************************************************************************80

bool collinear ( double xa, double ya, double xb, double yb, double xc, 
  double yc )

//****************************************************************************80
//
//  Purpose:
//
//    collinear() returns a measure of collinearity for three points.
//
//  Discussion:
//
//    In order to deal with collinear points whose coordinates are not
//    numerically exact, we compare the area of the largest square
//    that can be created by the line segment between two of the points
//    to (twice) the area of the triangle formed by the points.
//
//    If the points are collinear, their triangle has zero area.
//    If the points are close to collinear, then the area of this triangle
//    will be small relative to the square of the longest segment.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 May 2014
//
//  Author:
//
//    Original C version by Joseph ORourke.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Joseph ORourke,
//    Computational Geometry in C,
//    Cambridge, 1998,
//    ISBN: 0521649765,
//    LC: QA448.D38.
//
//  Input:
//
//    double XA, YA, XB, YB, XC, YC, the coordinates of 
//    the vertices.
//
//  Output:
//
//    bool COLLINEAR, is TRUE if the points are judged to be collinear.
//
{
  double area;
  double side_ab_sq;
  double side_bc_sq;
  double side_ca_sq;
  double side_max_sq;
  bool value;

  area = 0.5 * ( 
      ( xb - xa ) * ( yc - ya ) 
    - ( xc - xa ) * ( yb - ya ) );

  side_ab_sq = pow ( xa - xb, 2 ) + pow ( ya - yb, 2 );
  side_bc_sq = pow ( xb - xc, 2 ) + pow ( yb - yc, 2 );
  side_ca_sq = pow ( xc - xa, 2 ) + pow ( yc - ya, 2 );

  side_max_sq = fmax ( side_ab_sq, fmax ( side_bc_sq, side_ca_sq ) );

  if ( side_max_sq <= DBL_EPSILON )
  {
    value = true;
  }
  else if ( 2.0 * fabs ( area ) <= DBL_EPSILON * side_max_sq )
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

bool diagonal ( int im1, int ip1, int n, int prev[], int next[], double x[], 
  double y[] )

//****************************************************************************80
//
//  Purpose:
//
//    diagonal(): VERTEX(IM1) to VERTEX(IP1) is a proper internal diagonal.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 May 2014
//
//  Author:
//
//    Original C version by Joseph ORourke.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Joseph ORourke,
//    Computational Geometry in C,
//    Cambridge, 1998,
//    ISBN: 0521649765,
//    LC: QA448.D38.
//
//  Input:
//
//    int IM1, IP1, the indices of two vertices.
//
//    int N, the number of vertices.
//
//    int PREV[N], the previous neighbor of each vertex.
//
//    int NEXT[N], the next neighbor of each vertex.
//
//    double X[N], Y[N], the coordinates of each vertex.
//
//  Output:
//
//    bool DIAGONAL, the value of the test.
//
{
  bool value;
  bool value1;
  bool value2;
  bool value3;

  value1 = in_cone ( im1, ip1, n, prev, next, x, y );
  value2 = in_cone ( ip1, im1, n, prev, next, x, y );
  value3 = diagonalie ( im1, ip1, n, next, x, y );

  value = ( value1 && value2 && value3 );

  return value;
}
//****************************************************************************80

bool diagonalie ( int im1, int ip1, int n, int next[], double x[], double y[] )

//****************************************************************************80
//
//  Purpose:
//
//    diagonalie() is true if VERTEX(IM1):VERTEX(IP1) is a proper diagonal.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 May 2014
//
//  Author:
//
//    Original C version by Joseph ORourke.
//    This C++ version by John Burkardt.
//
//  Reference:
//
//    Joseph ORourke,
//    Computational Geometry in C,
//    Cambridge, 1998,
//    ISBN: 0521649765,
//    LC: QA448.D38.
//
//  Input:
//
//    int IM1, IP1, the indices of two vertices.
//
//    int N, the number of vertices.
//
//    int NEXT[N], the next neighbor of each vertex.
//
//    double X[N], Y[N], the coordinates of each vertex.
//
//  Output:
//
//    bool DIAGONALIE, the value of the test.
//
{
  int first;
  int j;
  int jp1;
  bool value;
  bool value2;

  first = im1;
  j = first;
  jp1 = next[first];

  value = true;
//
//  For each edge VERTEX(J):VERTEX(JP1) of the polygon:
//
  while ( 1 )
  {
//
//  Skip any edge that includes vertex IM1 or IP1.
//
    if ( j == im1 || j == ip1 || jp1 == im1 || jp1 == ip1 )
    {
    }
    else
    {
      value2 = intersect ( x[im1], y[im1], x[ip1], y[ip1], x[j], y[j], 
        x[jp1], y[jp1] );

      if ( value2 )
      {
        value = false;
        break;
      }
    }
    j = jp1;
    jp1 = next[j];

    if ( j == first )
    {
      break;
    }
  }

  return value;
}
//****************************************************************************80

int i4_ceiling ( double x )

//****************************************************************************80
//
//  Purpose:
//
//    i4_ceiling() rounds an R8 up to the next I4.
//
//  Example:
//
//    X        I4_CEILING(X)
//
//   -1.1      -1
//   -1.0      -1
//   -0.9       0
//   -0.1       0
//    0.0       0
//    0.1       1
//    0.9       1
//    1.0       1
//    1.1       2
//    2.9       3
//    3.0       3
//    3.14159   4
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 November 2011
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    double X, the number whose ceiling is desired.
//
//  Output:
//
//    int I4_CEILING, the ceiling of X.
//
{
  int value;

  value = ( int ) x;

  if ( value < x )
  {
    value = value + 1;
  }
  return value;
}
//****************************************************************************80

int i4_modp ( int i, int j )

//****************************************************************************80
//
//  Purpose:
//
//    i4_modp() returns the nonnegative remainder of I4 division.
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
//  Input:
//
//    int I, the number to be divided.
//
//    int J, the number that divides I.
//
//  Output:
//
//    int I4_MODP, the nonnegative remainder when I is
//    divided by J.
//
{
  int value;

  if ( j == 0 )
  {
    cerr << "\n";
    cerr << "I4_MODP(): Fatal error!\n";
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

int i4_wrap ( int ival, int ilo, int ihi )

//****************************************************************************80
//
//  Purpose:
//
//    i4_wrap() forces an I4 to lie between given limits by wrapping.
//
//  Example:
//
//    ILO = 4, IHI = 8
//
//    I   Value
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
//    26 December 2012
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int IVAL, an integer value.
//
//    int ILO, IHI, the desired bounds for the integer value.
//
//  Output:
//
//    int I4_WRAP, a "wrapped" version of IVAL.
//
{
  int jhi;
  int jlo;
  int value;
  int wide;

  if ( ilo <= ihi )
  {
    jlo = ilo;
    jhi = ihi;
  }
  else
  {
    jlo = ihi;
    jhi = ilo;
  }

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

bool in_cone ( int im1, int ip1, int n, int prev[], int next[], double x[], 
  double y[] )

//****************************************************************************80
//
//  Purpose:
//
//    in_cone() is TRUE if the diagonal VERTEX(IM1):VERTEX(IP1) is strictly internal.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 May 2014
//
//  Author:
//
//    Original C version by Joseph ORourke.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Joseph ORourke,
//    Computational Geometry in C,
//    Cambridge, 1998,
//    ISBN: 0521649765,
//    LC: QA448.D38.
//
//  Input:
//
//    int IM1, IP1, the indices of two vertices.
//
//    int N, the number of vertices.
//
//    int PREV[N], the previous neighbor of each vertex.
//
//    int NEXT[N], the next neighbor of each vertex.
//
//    double X[N], Y[N], the coordinates of each vertex.
//
//  Output:
//
//    bool IN_CONE, the value of the test.
//
{
  int i;
  int im2;
  double t1;
  double t2;
  double t3;
  double t4;
  double t5;
  bool value;

  im2 = prev[im1];
  i = next[im1];

  t1 = triangle_area ( x[im1], y[im1], x[i], y[i], x[im2], y[im2] );

  if ( 0.0 <= t1 )
  {
    t2 = triangle_area ( x[im1], y[im1], x[ip1], y[ip1], x[im2], y[im2] );
    t3 = triangle_area ( x[ip1], y[ip1], x[im1], y[im1], x[i], y[i] );
    value = ( ( 0.0 < t2 ) && ( 0.0 < t3 ) );
  }
  else
  {
    t4 = triangle_area ( x[im1], y[im1], x[ip1], y[ip1], x[i], y[i] );
    t5 = triangle_area ( x[ip1], y[ip1], x[im1], y[im1], x[im2], y[im2] );
    value = ! ( ( 0.0 <= t4 ) && ( 0.0 <= t5 ) );
  }
  return value;
}
//****************************************************************************80

bool intersect ( double xa, double ya, double xb, double yb, double xc, 
  double yc, double xd, double yd )

//****************************************************************************80
//
//  Purpose:
//
//    intersect() is true if lines VA:VB and VC:VD intersect.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 May 2014
//
//  Author:
//
//    Original C version by Joseph ORourke.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Joseph ORourke,
//    Computational Geometry in C,
//    Cambridge, 1998,
//    ISBN: 0521649765,
//    LC: QA448.D38.
//
//  Input:
//
//    double XA, YA, XB, YB, XC, YC, XD, YD, the X and Y 
//    coordinates of the four vertices.
//
//  Output:
//
//    bool INTERSECT, the value of the test.
//
{
  bool value;

  if ( intersect_prop ( xa, ya, xb, yb, xc, yc, xc, yd ) )
  {
    value = true;
  }
  else if ( between ( xa, ya, xb, yb, xc, yc ) )
  {
    value = true;
  }
  else if ( between ( xa, ya, xb, yb, xd, yd ) )
  {
    value = true;
  }
  else if ( between ( xc, yc, xd, yd, xa, ya ) )
  {
    value = true;
  }
  else if ( between ( xc, yc, xd, yd, xb, yb ) )
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

bool intersect_prop ( double xa, double ya, double xb, double yb, double xc, 
  double yc, double xd, double yd )

//****************************************************************************80
//
//  Purpose:
//
//    intersect_prop() is TRUE if lines VA:VB and VC:VD have a proper intersection.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 May 2014
//
//  Author:
//
//    Original C version by Joseph ORourke.
//    This C++ version by John Burkardt.
//
//  Reference:
//
//    Joseph ORourke,
//    Computational Geometry in C,
//    Cambridge, 1998,
//    ISBN: 0521649765,
//    LC: QA448.D38.
//
//  Input:
//
//    double XA, YA, XB, YB, XC, YC, XD, YD, the X and Y 
//    coordinates of the four vertices.
//
//  Output:
//
//    bool INTERSECT_PROP, the result of the test.
//
{
  double t1;
  double t2;
  double t3;
  double t4;
  bool value;
  bool value1;
  bool value2;
  bool value3;
  bool value4;

  if ( collinear ( xa, ya, xb, yb, xc, yc ) )
  {
    value = false;
  }
  else if ( collinear ( xa, ya, xb, yb, xd, yd ) )
  {
    value = false;
  }
  else if ( collinear ( xc, yc, xd, yd, xa, ya ) )
  {
    value = false;
  }
  else if ( collinear ( xc, yc, xd, yd, xb, yb ) )
  {
    value = false;
  }
  else
  {
    t1 = triangle_area ( xa, ya, xb, yb, xc, yc );
    t2 = triangle_area ( xa, ya, xb, yb, xd, yd );
    t3 = triangle_area ( xc, yc, xd, yd, xa, ya );
    t4 = triangle_area ( xc, yc, xd, yd, xb, yb );

    value1 = ( 0.0 < t1 );
    value2 = ( 0.0 < t2 );
    value3 = ( 0.0 < t3 );
    value4 = ( 0.0 < t4 );

    value = ( l4_xor ( value1, value2 ) ) && ( l4_xor ( value3, value4 ) );
  }
  return value;
}
//****************************************************************************80

bool l4_xor ( bool l1, bool l2 )

//****************************************************************************80
//
//  Purpose:
//
//    l4_xor() returns the exclusive OR of two L4's.
//
//  Discussion:
//
//    An L4 is a logical value.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 May 2014
//
//  Author:
//
//   John Burkardt
//
//  Input:
//
//    bool L1, L2, two values whose exclusive OR is needed.
//
//  Output:
//
//    bool L4_XOR, the exclusive OR of L1 and L2.
//
{
  bool value;
  bool value1;
  bool value2;

  value1 = (     l1   && ( ! l2 ) );
  value2 = ( ( ! l1 ) &&     l2   );

  value = ( value1 || value2 );

  return value;
}
//****************************************************************************80

double *polygon_angles ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_angles() computes the interior angles of a polygon.
//
//  Discussion:
//
//    The vertices should be listed in counter clockwise order.
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
//  Input:
//
//    int N, the number of vertices of the polygon.
//
//    double V[2*N], the vertices.
//
//  Output:
//
//    double POLYGON_ANGLES[N], the angles of the polygon, in radians.
//
{
  double *angle;
  int i;
  int im1;
  int ip1;

  if ( n < 1 )
  {
    return NULL;
  }

  angle = new double[n];

  if ( n <= 2 )
  {
    for ( i = 0; i < n; i++ )
    {
      angle[i] = 0.0;
    }
    return angle;
  }

  for ( i = 0; i < n; i++ )
  {
    im1 = i4_wrap ( i - 1, 0, n-1 );
    ip1 = i4_wrap ( i + 1, 0, n-1 );

    angle[i] = angle_rad ( v+im1*2, v+i*2, v+ip1*2 );
  }

  return angle;
}
//****************************************************************************80

double polygon_area ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_area() computes the area of a polygon.
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
//  Input:
//
//    int N, the number of vertices of the polygon.
//
//    double V[2*N], the coordinates of the vertices.
//
//  Output:
//
//    double POLYGON_AREA, the area of the polygon.
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

double polygon_area_2 ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_area_2() computes the area of a polygon.
//
//  Discussion:
//
//    The area is the sum of the areas of the triangles formed by
//    node N with consecutive pairs of nodes.
//
//    If the vertices are given in counter clockwise order, the area
//    will be positive.  If the vertices are given in clockwise order,
//    the area will be negative.
//
//    Thanks to Martin Pineault for noticing that an earlier version
//    of this routine would not correctly compute the area of a nonconvex
//    polygon.
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
//  Input:
//
//    int N, the number of vertices of the polygon.
//
//    double V[2*N], the coordinates of the vertices.
//
//  Output:
//
//    double POLYGON_AREA_2, the area of the polygon.
//
{
  double area;
  double area_triangle;
  int i;

  area = 0.0;
 
  for ( i = 0; i < n - 2; i++ )
  {
    area_triangle = triangle_area ( 
      v[0+i*2],     v[1+i*2], 
      v[0+(i+1)*2], v[1+(i+1)*2], 
      v[0+(n-1)*2], v[1+(n-1)*2] );

    area = area + area_triangle;
  }
  
  return area;
}
//****************************************************************************80

double polygon_area_lattice ( int i, int b )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_area_lattice() computes the area of a lattice polygon.
//
//  Discussion:
//
//    We define a lattice to be the plane, in which the points
//    whose coordinates are both integers are given a special
//    status as "lattice points".
//
//    A lattice polygon is a polygon whose vertices are lattice points.
//
//    The area of a lattice polygon can be computed by Pick's Theorem:
//
//      Area = I + B / 2 - 1
//
//    where
//
//      I = the number of lattice points contained strictly inside the polygon;
//
//      B = the number of lattice points that lie exactly on the boundary.
//  
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Branko Gruenbaum, G C Shephard,
//    Pick's Theorem,
//    The American Mathematical Monthly,
//    Volume 100, 1993, pages 150-161.
//
//  Input:
//
//    int I, the number of interior lattice points.
//
//    int B, the number of boundary lattice points.
//
//  Output:
//
//    double POLYGON_AREA_LATTICE, the area of the lattice polygon.
//
{
  double value;

  value = ( double ) i + ( ( double ) b ) / 2.0 - 1.0;

  return value;
}
//****************************************************************************80

double *polygon_centroid ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_centroid() computes the centroid of a polygon.
//
//  Discussion:
//
//    Denoting the centroid coordinates by (CX,CY), then
//
//      CX = Integral ( Polygon interior ) x dx dy / Area ( Polygon )
//      CY = Integral ( Polygon interior ) y dx dy / Area ( Polygon ).
//
//    Green's theorem states that
//
//      Integral ( Polygon boundary ) ( M dx + N dy ) =
//      Integral ( Polygon interior ) ( dN/dx - dM/dy ) dx dy.
//
//    Using M = 0 and N = x^2/2, we get:
//
//      CX = 0.5 * Integral ( Polygon boundary ) x^2 dy,
//
//    which becomes
//
//      CX = 1/6 SUM ( 1 <= I <= N ) 
//        ( X[I+1] + X[I] ) * ( X[I] * Y[I+1] - X[I+1] * Y[I] )
//
//    where, when I = N, the index "I+1" is replaced by 1.
//
//    A similar calculation gives us a formula for CY.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Gerard Bashein, Paul Detmer,
//    Centroid of a Polygon,
//    Graphics Gems IV, edited by Paul Heckbert,
//    AP Professional, 1994, T385.G6974.
//
//  Input:
//
//    int N, the number of sides of the polygon.
//
//    double V[2*N], the coordinates of the vertices.
//
//  Output:
//
//    double *POLYGON_CENTROID[2], the coordinates of the centroid.
//
{
  double area;
  double *centroid;
  int i;
  int ip1;
  int j;
  double temp;

  area = 0.0;
  centroid = new double[2];

  for ( j = 0; j < 2; j++ )
  {
    centroid[j] = 0.0;
  }

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

    temp = ( v[0+i*2] * v[1+ip1*2] - v[0+ip1*2] * v[1+i*2] );

    area = area + temp;

    centroid[0] = centroid[0] + ( v[0+ip1*2] + v[0+i*2] ) * temp;
    centroid[1] = centroid[1] + ( v[1+ip1*2] + v[1+i*2] ) * temp;
  }

  area = area / 2.0;

  for ( j = 0; j < 2; j++ )
  {
    centroid[j] = centroid[j] / ( 6.0 * area );
  }

  return centroid;
}
//****************************************************************************80

double *polygon_centroid_2 ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_centroid_2() computes the centroid of a polygon.
//
//  Method:
//
//    The centroid is the area-weighted sum of the centroids of
//    disjoint triangles that make up the polygon.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
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
//  Input:
//
//    int N, the number of vertices of the polygon.
//
//    double V[2*N], the coordinates of the vertices. 
//
//  Output:
//
//    double POLYGON_CENTROID_2[2], the coordinates of the centroid.
//
{
  double area;
  double area_triangle;
  double *centroid;
  int i;
  int j;

  area = 0.0;
  centroid = new double[2];

  for ( j = 0; j < 2; j++ )
  {
    centroid[j] = 0.0;
  }
 
  for ( i = 0; i < n-2; i ++ )
  {
    area_triangle = triangle_area (
      v[0+i*2],     v[1+i*2], 
      v[0+(i+1)*2], v[1+(i+1)*2],
      v[0+(n-1)*2], v[1+(n-1)*2] );

    area = area + area_triangle;
    centroid[0] = centroid[0] 
      + area_triangle * ( v[0+i*2] + v[0+(i+1)*2] + v[0+(n-1)*2] ) / 3.0;
    centroid[1] = centroid[1] 
      + area_triangle * ( v[1+i*2] + v[1+(i+1)*2] + v[1+(n-1)*2] ) / 3.0; 
  }

  for ( j = 0; j < 2; j++ )
  {
    centroid[j] = centroid[j] / area;
  }
  
  return centroid;
}
//****************************************************************************80

bool polygon_contains_point ( int n, double v[], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_contains_point() finds if a point is inside a simple polygon .
//
//  Discussion:
//
//    A simple polygon is one whose boundary never crosses itself.
//    The polygon does not need to be convex.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    M Shimrat,
//    Position of Point Relative to Polygon,
//    ACM Algorithm 112,
//    Communications of the ACM,
//    Volume 5, Number 8, page 434, August 1962.
//
//  Input:
//
//    int N, the number of nodes or vertices in the polygon.
//    N must be at least 3.
//
//    double V[2*N], the coordinates of the vertices.
//
//    double P[2], the coordinates of the point to be tested.
//
//  Output:
//
//    bool POLYGON_CONTAINS_POINT, is TRUE if the point
//    is inside the polygon or on its boundary, and FALSE otherwise.
//
{
  int i;
  bool value;
  double x1;
  double x2;
  double y1;
  double y2;

  value = false;

  for ( i = 0; i < n; i++ )
  {
    x1 = v[0+i*2];
    y1 = v[1+i*2];

    if ( i < n - 1 )
    {
      x2 = v[0+(i+1)*2];
      y2 = v[1+(i+1)*2];
    }
    else
    {
      x2 = v[0+0*2];
      y2 = v[1+0*2];
    }

    if ( ( y1   <  p[1] && p[1] <= y2  ) ||
         ( p[1] <= y1   && y2   < p[1] ) )
    {
      if ( ( p[0] - x1 ) - ( p[1] - y1 ) * ( x2 - x1 ) / ( y2 - y1 ) < 0 )
      {
        value = !value;
      }
    }
  }

  return value;
}
//****************************************************************************80

bool polygon_contains_point_2 ( int n, double v[], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_contains_point_2() finds if a point is inside a convex polygon.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of nodes or vertices in the polygon.
//    N must be at least 3.
//
//    double V[2*N], the coordinates of the vertices.
//
//    double P[2], the coordinates of the point to be tested.
//
//  Output:
//
//    bool POLYGON_CONTAINS_POINT_2, is TRUE if the point
//    is inside the polygon or on its boundary, and FALSE otherwise.
//
{
  int i;
  double t[2*3];
//
//  A point is inside a convex polygon if and only if it is inside
//  one of the triangles formed by the first vertex and any two consecutive
//  vertices.
//
  t[0+0*2] = v[0+0*2];
  t[1+0*2] = v[1+0*2];

  for ( i = 1; i < n - 1; i++ )
  {
    t[0+1*2] = v[0+i*2];
    t[1+1*2] = v[1+i*2];
    t[0+2*2] = v[0+(i+1)*2];
    t[1+2*2] = v[1+(i+1)*2];

    if ( triangle_contains_point_1 ( t, p ) )
    {
      return true;
    }
  }
  return false;
}
//****************************************************************************80

void polygon_data_inrad ( int n, double radin, double &area, double &radout, 
  double &side )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_data_inrad() determines polygonal data from its inner radius.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of sides of the polygon.
//    N must be at least 3.
//
//    double RADIN, the inner radius of the polygon, that is,
//    the radius of the largest circle that can be inscribed within
//    the polygon.
//
//  Output:
//
//    double &AREA, the area of the regular polygon.
//
//    double &RADOUT, the outer radius of the polygon, that is,
//    the radius of the smallest circle that can be described about
//    the polygon.
//
//    double &SIDE, the length of one side of the polygon.
//
{
  double angle;
  const double r8_pi = 3.141592653589793;

  if ( n < 3 )
  {
    cerr << "\n";
    cerr << "polygon_data_inrad(): Fatal error!\n";
    cerr << "  Input value of N must be at least 3,\n";
    cerr << "  but your input value was N = " << n << "\n";
    exit ( 1 );
  }

  angle = r8_pi / ( ( double ) n );
  area = ( ( double ) n ) * radin * radin * tan ( angle );
  side = 2.0 * radin * tan ( angle );
  radout = 0.5 * ( side ) / sin ( angle );

  return;
}
//****************************************************************************80

void polygon_data_outrad ( int n, double radout, double &area, double &radin, 
  double &side )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_data_outrad() determines polygonal data from its outer radius.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of sides of the polygon.
//    N must be at least 3.
//
//    double RADOUT, the outer radius of the polygon, that is,
//    the radius of the smallest circle that can be described
//    around the polygon.
//
//  Output:
//
//    double &AREA, the area of the regular polygon.
//
//    double &RADIN, the inner radius of the polygon, that is,
//    the radius of the largest circle that can be inscribed
//    within the polygon.
//
//    double &SIDE, the length of one side of the polygon.
//
{
  double angle;
  const double r8_pi = 3.141592653589793;

  if ( n < 3 )
  {
    cerr << "\n";
    cerr << "polygon_data_outrad(): Fatal error!\n";
    cerr << "  Input value of N must be at least 3,\n";
    cerr << "  but your input value was N = " << n << "\n";
    exit ( 1 );
  }

  angle = r8_pi / ( ( double ) n );
  area = 0.5 * ( ( double ) n ) * radout * radout * sin ( 2.0 * angle );
  side = 2.0 * radout * sin ( angle );
  radin = 0.5 * side / tan ( angle );

  return;
}
//****************************************************************************80

void polygon_data_side ( int n, double side, double &area, double &radin, 
  double &radout )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_data_side() determines polygonal data from its side length.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of sides of the polygon.
//    N must be at least 3.
//
//    double SIDE, the length of one side of the polygon.
//
//  Output:
//
//    double *AREA, the area of the regular polygon.
//
//    double *RADIN, the inner radius of the polygon, that is,
//    the radius of the largest circle that can be inscribed within
//    the polygon.
//
//    double *RADOUT, the outer radius of the polygon, that is,
//    the radius of the smallest circle that can be described about
//    the polygon.
//
{
  double angle;
  const double r8_pi = 3.141592653589793;

  if ( n < 3 )
  {
    cerr << "\n";
    cerr << "polygon_data_side(): Fatal error!\n";
    cerr << "  Input value of N must be at least 3,\n";
    cerr << "  but your input value was N = " << n << "\n";
    exit ( 1 );
  }

  angle = r8_pi / ( ( double ) n );
  area = 0.25 * n * side * side / tan ( angle );
  radin = 0.5 * side / tan ( angle );
  radout = 0.5 * side / sin ( angle );

  return;
}
//****************************************************************************80

double polygon_diameter ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_diameter() computes the diameter of a polygon.
//
//  Discussion:
//
//    The diameter of a polygon is the maximum distance between any
//    two points on the polygon.  It is guaranteed that this maximum
//    distance occurs between two vertices of the polygon.  It is
//    sufficient to check the distance between all pairs of vertices.
//    This is an N^2 algorithm.  There is an algorithm by Shamos which
//    can compute this quantity in order N time instead.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of vertices of the polygon.
//
//    double V[2*N], the coordinates of the vertices.
//
//  Output:
//
//    double POLYGON_DIAMETER, the diameter of the polygon.
//
{
  double diameter;
  int i;
  int j;
  double t;

  diameter = 0.0;

  for ( i = 0; i < n; i++ )
  {
    for ( j = i+1; j < n; j++ )
    {
      t = 
        sqrt ( ( v[0+i*2] - v[0+j*2] ) * ( v[0+i*2] - v[0+j*2] ) 
             + ( v[1+i*2] - v[1+j*2] ) * ( v[1+i*2] - v[1+j*2] ) );
      if ( diameter < t )
      {
        diameter = t;
      }
    }
  }

  return diameter;
}
//****************************************************************************80

double *polygon_expand ( int n, double v[], double h )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_expand() expands a polygon.
//
//  Discussion:
//
//    This routine simply moves each vertex of the polygon outwards
//    in such a way that the sides of the polygon advance by H.
//
//    This approach should always work if the polygon is convex, or
//    star-shaped.  But for general polygons, it is possible
//    that this procedure, for large enough H, will create a polygon
//    whose sides intersect.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//   21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of sides of the polygon.
//
//    double V[2*N], the coordinates of the vertices.
//
//    double H, the expansion amount.
//
//  Output:
//
//    double POLYGON_EXPAND[2*N], the "expanded" coordinates.
//
{
  double angle;
  double h2;
  int i;
  int j;
  int jm1;
  int jp1;
  double *p4;
  double *w;

  w = new double[2*n];
//
//  Consider each angle, formed by the nodes P(I-1), P(I), P(I+1).
//
  for ( j = 0; j < n; j++ )
  {
    jm1 = i4_wrap ( j-1, 0, n-1 );
    jp1 = i4_wrap ( j+1, 0, n-1 );
//
//        P1
//        /
//       /   P4
//      /  .
//     / .
//    P2--------->P3
//
    p4 = angle_half ( v+jm1*2, v+j*2, v+jp1*2 );
//
//  Compute the value of the half angle.
//
    angle = angle_rad ( v+jm1*2, v+j*2, p4 );
//
//  The stepsize along the ray must be adjusted so that the sides
//  move out by H.
//
    h2 = h / sin ( angle );

    for ( i = 0; i < 2; i++ )
    {
      w[i+j*2] = v[i+j*2] - h2 * ( p4[i] - v[i+j*2] );
    }

    delete [] p4;
  }

  return w;
}
//****************************************************************************80

double polygon_integral_1 ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_integral_1() integrates the function 1 over a polygon.
//
//  Discussion:
//
//    INTEGRAL = 0.5 * SUM ( 1 <= I <= N ) (X(I)+X(I-1)) * (Y(I)-Y(I-1))
//
//    where X[N] and Y[N] should be replaced by X[0] and Y[0].
//
//    The integral of 1 over a polygon is the area of the polygon.
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
//  Reference:
//
//    SF Bockman,
//    Generalizing the Formula for Areas of Polygons to Moments,
//    American Mathematical Society Monthly,
//    1989, pages 131-132.
//
//  Input:
//
//    int N, the number of vertices of the polygon.
//    N should be at least 3 for a nonzero result.
//
//    double V[2*N], the coordinates of the vertices
//    of the polygon.  These vertices should be given in
//    counter clockwise order.
//
//  Output:
//
//    double POLYGON_INTEGRAL_1, the value of the integral.
//
{
  int i;
  int im1;
  double result;

  result = 0.0;
 
  if ( n < 3 )
  {
    cerr << "\n";
    cerr << "polygon_integral_1(): Fatal error!\n";
    cerr << "  The number of vertices must be at least 3.\n";
    cerr << "  The input value of N = " << n << "\n";
    exit ( 1 );
  }
 
  for ( i = 0; i < n; i++ )
  {
    if ( i == 0 )
    {
      im1 = n - 1;
    }
    else
    {
      im1 = i - 1;
    }
    result = result + 0.5 * ( v[0+i*2] + v[0+im1*2] ) 
                          * ( v[1+i*2] - v[1+im1*2] );
  }
 
  return result;
}
//****************************************************************************80

double polygon_integral_x ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_integral_x() integrates the function X over a polygon.
//
//  Discussion:
//
//    INTEGRAL = (1/6) * SUM ( I = 1 to N )
//      ( X[I]^2 + X[I] * X[I-1] + X[I-1]^2 ) * ( Y[I] - Y[I-1] )
//
//    where X[N] and Y[N] should be replaced by X[0] and Y[0].
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    SF Bockman,
//    Generalizing the Formula for Areas of Polygons to Moments,
//    American Mathematical Society Monthly,
//    1989, pages 131-132.
//
//  Input:
//
//    int N, the number of vertices of the polygon.
//    N should be at least 3 for a nonzero result.
//
//    double V[2*N], the coordinates of the vertices.
//
//  Output:
//
//    double POLYGON_INTEGRAL_X, the value of the integral.
//
{
  int i;
  int im1;
  double result;

  result = 0.0;
 
  if ( n < 3 )
  {
    cerr << "\n";
    cerr << "polygon_integral_x(): Fatal error!\n";
    cerr << "  The number of vertices must be at least 3.\n";
    cerr << "  The input value of N = " << n << "\n";
    exit ( 1 );
  }
 
  for ( i = 0; i < n; i++ )
  {
    if ( i == 0 )
    {
      im1 = n - 1;
    }
    else
    {
      im1 = i - 1;
    }
 
    result = result + ( v[0+i*2]   * v[0+i*2] 
                      + v[0+i*2]   * v[0+im1*2] 
                      + v[0+im1*2] * v[0+im1*2] )
                      * ( v[1+i*2] - v[1+im1*2] );
  }
 
  result = result / 6.0;
 
  return result;
}
//****************************************************************************80

double polygon_integral_xx ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_integral_xx() integrates the function X*X over a polygon.
//
//  Discussion:
//
//    INTEGRAL = (1/12) * SUM ( I = 1 to N )
//      ( X[I]^3 + X[I]^2 * X[I-1] + X[I] * X[I-1]^2 + X[I-1]^3 )
//      * ( Y[I] - Y[I-1] )
//
//    where X[N] and Y[N] should be replaced by X[0] and Y[0].
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    SF Bockman,
//    Generalizing the Formula for Areas of Polygons to Moments,
//    American Mathematical Society Monthly,
//    1989, pages 131-132.
//
//  Input:
//
//    int N, the number of vertices of the polygon.
//    N should be at least 3 for a nonzero result.
//
//    double V[2*N], the coordinates of the vertices.
//
//  Output:
//
//    double POLYGON_INTEGRAL_XX, the value of the integral.
//
{
  int i;
  int im1;
  double result;

  result = 0.0;
 
  if ( n < 3 )
  {
    cerr << "\n";
    cerr << "polygon_integral_xx(): Fatal error!\n";
    cerr << "  The number of vertices must be at least 3.\n";
    cerr << "  The input value of N = " << n << "\n";
    exit ( 1 );
  }
 
  for ( i = 0; i < n; i++ )
  {
    if ( i == 0 )
    {
      im1 = n - 1;
    }
    else
    {
      im1 = i - 1;
    }
 
    result = result + ( 
        v[0+i*2]   * v[0+i*2]   * v[0+i*2] 
      + v[0+i*2]   * v[0+i*2]   * v[0+im1*2] 
      + v[0+i*2]   * v[0+im1*2] * v[0+im1*2] 
      + v[0+im1*2] * v[0+im1*2] * v[0+im1*2] ) * ( v[1+i*2] - v[1+im1*2] );
  }
 
  result = result / 12.0;
 
  return result;
}
//****************************************************************************80

double polygon_integral_xy ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_integral_xy() integrates the function X*Y over a polygon.
//
//  Discussion:
//
//    INTEGRAL = (1/24) * SUM (I=1 to N)
//      ( Y[I] * 
//        ( 3 * X[I]^2 + 2 * X[I] * X[I-1] + X[I-1]^2 )
//      + Y[I-1] *
//        ( X[I]^2 + 2 * X[I] * X[I-1] + 3 * X[I-1]^2 ) 
//      ) * ( Y[I] - Y[I-1] )
//
//    where X[N] and Y[N] should be replaced by X[0] and Y[0].
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    SF Bockman,
//    Generalizing the Formula for Areas of Polygons to Moments,
//    American Mathematical Society Monthly,
//    1989, pages 131-132.
//
//  Input:
//
//    int N, the number of vertices of the polygon.
//    N should be at least 3 for a nonzero result.
//
//    double V[2*N], the coordinates of the vertices.
//
//  Output:
//
//    double POLYGON_INTEGRAL_XY, the value of the integral.
//
{
  int i;
  int im1;
  double result;

  result = 0.0;
 
  if ( n < 3 )
  {
    cerr << "\n";
    cerr << "polygon_integral_xy(): Fatal error!\n";
    cerr << "  The number of vertices must be at least 3.\n";
    cerr << "  The input value of N = " << n << "\n";
    exit ( 1 );
  }
 
  for ( i = 0; i < n; i++ )
  {
    if ( i == 0 )
    {
      im1 = n - 1;
    }
    else
    {
      im1 = i - 1;
    }
 
    result = result + (
      v[1+i*2]   * ( 3.0 *   v[0+i*2]   * v[0+i*2] 
                   + 2.0 *   v[0+i*2]   * v[0+im1*2] 
                   +         v[0+im1*2] * v[0+im1*2] )
    + v[1+im1*2] * (         v[0+i*2]   * v[0+i*2] 
                   + 2.0 *   v[0+i*2]   * v[0+im1*2] 
                   + 3.0 *   v[0+im1*2] * v[0+im1*2] )
      ) * ( v[1+i*2] - v[1+im1*2] );
  }
 
  result = result / 24.0;
 
  return result;
}
//****************************************************************************80

double polygon_integral_y ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_y() integrates the function Y over a polygon.
//
//  Discussion:
//
//    INTEGRAL = (1/6) * SUM ( I = 1 to N )
//      - ( Y[I]^2 + Y[I] * Y[I-1] + Y[I-1]^2 ) * ( X[I] - X[I-1] )
//
//    where X[N] and Y[N] should be replaced by X[0] and Y[0].
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    SF Bockman,
//    Generalizing the Formula for Areas of Polygons to Moments,
//    American Mathematical Society Monthly,
//    1989, pages 131-132.
//
//  Input:
//
//    int N, the number of vertices of the polygon.
//    N should be at least 3 for a nonzero result.
//
//    double V[2*N], the coordinates of the vertices.
//
//  Output:
//
//    double POLYGON_INTEGRAL_Y, the value of the integral.
//
{
  int i;
  int im1;
  double result;

  result = 0.0;
 
  if ( n < 3 )
  {
    cerr << "\n";
    cerr << "polygon_integral_y(): Fatal error!\n";
    cerr << "  The number of vertices must be at least 3.\n";
    cerr << "  The input value of N = " << n << "\n";
    exit ( 1 );
  }
 
  for ( i = 0; i < n; i++ )
  {
    if ( i == 0 )
    {
      im1 = n - 1;
    }
    else
    {
      im1 = i - 1;
    }
 
    result = result - ( v[1+i*2]   * v[1+i*2] 
                      + v[1+i*2]   * v[1+im1*2] 
                      + v[1+im1*2] * v[1+im1*2] )
                      * ( v[0+i*2] - v[0+im1*2] );
  }
 
  result = result / 6.0;
 
  return result;
}
//****************************************************************************80

double polygon_integral_yy ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_integral_yy() integrates the function Y*Y over a polygon.
//
//  Discussion:
//
//    INTEGRAL = (1/12) * SUM ( I = 1 to N )
//      - ( Y[I]^3 + Y[I]^2 * Y[I-1] + Y[I] * Y[I-1]^2 + Y[I-1]^3 )
//      * ( X[I] - X[I-1] )
//
//    where X[N] and Y[N] should be replaced by X[0] and Y[0].
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    21 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    SF Bockman,
//    Generalizing the Formula for Areas of Polygons to Moments,
//    American Mathematical Society Monthly,
//    1989, pages 131-132.
//
//  Input:
//
//    int N, the number of vertices of the polygon.
//    N should be at least 3 for a nonzero result.
//
//    double V[2*N], the coordinates of the vertices.
//
//  Output:
//
//    double POLYGON_INTEGRAL_YY, the value of the integral.
//
//
{
  int i;
  int im1;
  double result;

  result = 0.0;
 
  if ( n < 3 )
  {
    cerr << "\n";
    cerr << "polygon_integral_yy(): Fatal error!\n";
    cerr << "  The number of vertices must be at least 3.\n";
    cerr << "  The input value of N = " << n << "\n";
    exit ( 1 );
  }
 
  for ( i = 0; i < n; i++ )
  {

    if ( i == 0 )
    {
      im1 = n - 1;
    }
    else
    {
      im1 = i - 1;
    }
 
    result = result -
      ( v[1+i*2]   * v[1+i*2]   * v[1+i*2]
      + v[1+i*2]   * v[1+i*2]   * v[1+im1*2] 
      + v[1+i*2]   * v[1+im1*2] * v[1+im1*2]
      + v[1+im1*2] * v[1+im1*2] * v[1+im1*2] ) 
      * ( v[0+i*2] - v[0+im1*2] );
  }
 
  result = result / 12.0;
 
  return result;
}
//****************************************************************************80

int polygon_is_convex ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_is_convex() determines whether a polygon is convex in 2D.
//
//  Discussion:
//
//    If the polygon has less than 3 distinct vertices, it is
//    classified as convex degenerate.
//
//    If the polygon "goes around" more than once, it is classified
//    as NOT convex.
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
//    Peter Schorn, Frederick Fisher,
//    Testing the Convexity of a Polygon,
//    Graphics Gems IV,
//    edited by Paul Heckbert,
//    AP Professsional, 1994, T385.G6974.
//
//  Parameters
//
//    int N, the number of vertices.
//
//    double V[2*N], the coordinates of the vertices of the
//    polygon.  
//
//  Output:
//
//    int POLYGON_IS_CONVEX:
//    -1, the polygon is not convex;
//     0, the polygon has less than 3 vertices; it is "degenerately" convex;
//     1, the polygon is convex and counter clockwise;
//     2, the polygon is convex and clockwise.
//
//    double V[2*N], duplicate consecutive points have been deleted,
//    and the vertices have been reordered so that the lexicographically
//    least point comes first.
//
{
# define NOT_CONVEX         -1
# define DEGENERATE_CONVEX   0
# define CONVEX_CCW          1
# define CONVEX_CW           2

  double angle;
  double cross;
  double dot;
  double exterior_total;
  int i;
  int ip1;
  int ip2;
  double sense;
  double TOL = 1.0;
  int value = 0;

  exterior_total = 0.0;
//
//  If there are not at least 3 distinct vertices, we are done.
//
  if ( n < 3 )
  {
    return DEGENERATE_CONVEX;
  }

  sense = 0.0;
//
//  Consider each polygonal vertex I.
//
  for ( i = 0; i < n; i++ )
  {
    ip1 = i4_wrap ( i + 1, 0, n-1 );
    ip2 = i4_wrap ( i + 2, 0, n-1 );

    dot =   ( v[0+ip2*2] - v[0+ip1*2] ) * ( v[0+i*2] - v[0+ip1*2] )
          + ( v[1+ip2*2] - v[1+ip1*2] ) * ( v[1+i*2] - v[1+ip1*2] );

    cross =   ( v[0+ip2*2] - v[0+ip1*2] ) * ( v[1+i*2]   - v[1+ip1*2] )
            - ( v[0+i*2]   - v[0+ip1*2] ) * ( v[1+ip2*2] - v[1+ip1*2] );

    angle = atan2 ( cross, dot );
//
//  See if the turn defined by this vertex is our first indication of
//  the "sense" of the polygon, or if it disagrees with the previously
//  defined sense.
//
    if ( sense == 0.0 )
    {
      if ( angle < 0.0 )
      {
        sense = -1.0;
      }
      else if ( 0.0 < angle )
      {
        sense = +1.0;
      }
    }
    else if ( sense == 1.0 )
    {
      if ( angle < 0.0 )
      {
        return NOT_CONVEX;
      }
    }
    else if ( sense == -1.0 )
    {
      if ( 0.0 < angle )
      {
        return NOT_CONVEX;
      }
    }
//
//  If the exterior total is greater than 360, then the polygon is
//  going around again.
//
    angle = atan2 ( -cross, -dot );

    exterior_total = exterior_total + angle;

    if ( 360.0 + TOL < r8_degrees ( fabs ( exterior_total ) ) )
    {
      return NOT_CONVEX;
    }

  }

  if ( sense == +1.0 )
  {
    value = CONVEX_CCW;
  }
  else if ( sense == -1.0 )
  {
    value = CONVEX_CW;
  }
  return value;

# undef NOT_CONVEX
# undef DEGENERATE_CONVEX
# undef CONVEX_CCW
# undef CONVEX_CW
}
//****************************************************************************80

double polygon_perimeter ( int n, double v[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_perimeter() computes the perimeter of a polygon.
//
//  Discussion:
//
//    The perimeter is simply the sum of the side lengths.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    16 October 2015
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of vertices of the polygon.
//
//    double V(2,N), the vertices.
//
//  Output:
//
//    double POLYGON_PERIMETER, the perimeter.
//
{
  int i;
  int im1;
  double l;
  double value;

  value = 0.0;

  im1 = n - 1;

  for ( i = 0; i < n; i++ )
  {
    l = sqrt ( pow ( v[0+im1*2] - v[0+i*2], 2 ) 
             + pow ( v[1+im1*2] - v[1+i*2], 2 ) );
    value = value + l;
    im1 = i;
  }

  return value;
}
//****************************************************************************80

double polygon_perimeter_quad ( int n, double v[], double hmax, 
  double f ( double x, double y ) )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_perimeter_quad() estimates an integral over the perimeter of a polygon.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    20 October 2015
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of vertices of the polygon.
//
//    double V(2,N), the vertices.
//
//    double HMAX, the maximum length of a quadrature interval.
//
//    double F ( double X, double Y ), a function whose integral 
//    over the perimeter is desired.
//
//  Output:
//
//    double POLYGON_PERIMETER_QUAD, the estimated integral.
//
{
  double dxy;
  double fxy;
  int i;
  int ip1;
  int j;
  double l;
  int m;
  double value;
  double x;
  double y;

  value = 0.0;

  for ( i = 0; i < n; i++ )
  {
    ip1 = i4_wrap ( i + 1, 0, n - 1 );
    l = sqrt ( pow ( v[0+ip1*2] - v[0+i*2], 2 ) + pow ( v[1+ip1*2] - v[1+i*2], 2 ) );
    m = i4_ceiling ( l / hmax );
    dxy = l / ( double ) ( m );

    for ( j = 1; j <= 2 * m - 1; j = j + 2 )
    {
      x = ( ( double ) ( 2 * m - j ) * v[0+i*2] 
          + ( double ) (         j ) * v[0+ip1*2] ) 
          / ( double ) ( 2 * m     );
      y = ( ( double ) ( 2 * m - j ) * v[1+i*2] 
          + ( double ) (         j ) * v[1+ip1*2] ) 
          / ( double ) ( 2 * m     );
      fxy = f ( x, y );
      value = value + fxy * dxy;
    }
  }

  return value;
}
//****************************************************************************80

double polygon_point_dist ( int n, double v[], double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_point_dist(): distance ( polygon, point ).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    07 May 2014
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of vertices.
//
//    double V[2*N], the triangle vertices.
//
//    double P[2], the point to be checked.
//
//  Output:
//
//    double POLYGON_POINT_DIST, the distance from the point to the
//    polygon.
//
{
  double dist;
  double dist2;
  int j;
  int jp1;
//
//  Find the distance to each of the line segments.
//
  dist = HUGE_VAL;

  for ( j = 0; j < n; j++ )
  {
    jp1 = i4_wrap ( j+1, 0, n - 1 );

    dist2 = segment_point_dist ( v+j*2, v+jp1*2, p );

    if ( dist2 < dist )
    {
      dist = dist2;
    }
  }

  return dist;
}
//****************************************************************************80

double *polygon_point_near ( int n, double v[], double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_point_near() computes the nearest point on a polygon.
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
//  Input:
//
//    int N, the number of vertices.
//
//    double V[2*N], the polygon vertices.
//
//    double P[2], the point whose nearest polygon point
//    is to be determined.
//
//  Output:
//
//    double POLYGON_POINT_NEAR[2], the nearest point to P.
//
{
  double dist;
  double dist2;
  int j;
  int jp1;
  double *pn;
  double *pn2;
//
//  Find the distance to each of the line segments that make up the edges
//  of the polygon.
//
  dist = HUGE_VAL;

  pn = new double[2];
  pn[0] = 0.0;
  pn[1] = 0.0;

  for ( j = 0; j < n; j++ )
  {
    jp1 = i4_wrap ( j+1, 0, n - 1 );

    pn2 = segment_point_near ( v+j*2, v+jp1*2, p );

    dist2 = sqrt ( pow ( pn2[0] - p[0], 2 ) + pow ( pn2[1] - p[1], 2 ) );

    if ( dist2 < dist )
    {
      dist = dist2;
      pn[0] = pn2[0];
      pn[1] = pn2[1];
    }
    delete [] pn2;
  }

  return pn;
}
//****************************************************************************80

double *polygon_sample ( int nv, double v[], int n, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_sample() uniformly samples a polygon.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 May 2014
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int NV, the number of vertices.
//
//    double V[2*NV], the vertices of the polygon, listed in
//    counterclockwise order.
//
//    int N, the number of points to create.
//
//    int *SEED, a seed for the random number generator.
//
//  Output:
//
//    double POLYGON_SAMPLE[2*N], the points.
//
//    int *SEED, an updated random number seed.
//
{
  double *area_cumulative;
  double area_polygon;
  double *area_relative;
  double *area_triangle;
  double area_percent;
  int i;
  int j;
  int k;
  double *r;
  double *s;
  int *triangles;
  double *x;
  double *y;
//
//  Triangulate the polygon.
//
  x = new double[nv];
  y = new double[nv];
  for ( i = 0; i < nv; i++ )
  {
    x[i] = v[0+i*2];
    y[i] = v[1+i*2];
  }

  triangles = polygon_triangulate ( nv, x, y );
//
//  Determine the areas of each triangle.
//
  area_triangle = new double[nv-2];

  for ( i = 0; i < nv - 2; i++ )
  {
    area_triangle[i] = triangle_area ( 
      v[0+triangles[0+i*3]*2], v[1+triangles[0+i*3]*2], 
      v[0+triangles[1+i*3]*2], v[1+triangles[1+i*3]*2], 
      v[0+triangles[2+i*3]*2], v[1+triangles[2+i*3]*2] );
  }
//
//  Normalize the areas.
//
  area_polygon = r8vec_sum ( nv - 2, area_triangle );

  area_relative = new double[nv-2];

  for ( i = 0; i < nv - 2; i++ )
  {
    area_relative[i] = area_triangle[i] / area_polygon;
  }
//
//  Replace each area by the sum of itself and all previous ones.
//
  area_cumulative = new double[nv-2];

  area_cumulative[0] = area_relative[0];
  for ( i = 1; i < nv - 2; i++ )
  {
    area_cumulative[i] = area_relative[i] + area_cumulative[i-1];
  }

  s = new double[2*n];

  for ( j = 0; j < n; j++ )
  {
//
//  Choose triangle I at random, based on areas.
//
    area_percent = r8_uniform_01 ( seed );

    for ( k = 0; k < nv - 2; k++ )
    {
      i = k;

      if ( area_percent <= area_cumulative[k] )
      {
        break;
      }
    }
//
//  Now choose a point at random in triangle I.
//
    r = r8vec_uniform_01_new ( 2, seed );

    if ( 1.0 < r[0] + r[1] )
    {
      r[0] = 1.0 - r[0];
      r[1] = 1.0 - r[1];
    }

    s[0+j*2] = ( 1.0 - r[0] - r[1] ) * v[0+triangles[0+i*3]*2]
                     + r[0]          * v[0+triangles[1+i*3]*2]
                            + r[1]   * v[0+triangles[2+i*3]*2];

    s[1+j*2] = ( 1.0 - r[0] - r[1] ) * v[1+triangles[0+i*3]*2]
                     + r[0]          * v[1+triangles[1+i*3]*2]
                            + r[1]   * v[1+triangles[2+i*3]*2];
    delete [] r;
  }

  delete [] area_cumulative;
  delete [] area_relative;
  delete [] area_triangle;
  delete [] triangles;
  delete [] x;
  delete [] y;

  return s;
}
//****************************************************************************80

int *polygon_triangulate ( int n, double x[], double y[] )

//****************************************************************************80
//
//  Purpose:
//
//    polygon_triangulate() determines a triangulation of a polygon.
//
//  Discussion:
//
//    There are N-3 triangles in the triangulation.
//
//    For the first N-2 triangles, the first edge listed is always an
//    internal diagonal.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 May 2014
//
//  Author:
//
//    Original C version by Joseph ORourke.
//    This C++ version by John Burkardt.
//
//  Reference:
//
//    Joseph ORourke,
//    Computational Geometry in C,
//    Cambridge, 1998,
//    ISBN: 0521649765,
//    LC: QA448.D38.
//
//  Input:
//
//    int N, the number of vertices.
//
//    double X[N], Y[N], the coordinates of each vertex.
//
//  Output:
//
//    int TRIANGLES[3*(N-2)], the triangles of the 
//    triangulation.
//
{
  double area;
  bool *ear;
  int i;
  int i0;
  int i1;
  int i2;
  int i3;
  int i4;
  int *next;
  int node;
  int node_m1;
  int *prev;
  int triangle_num;
  int *triangles;
//
//  We must have at least 3 vertices.
//
  if ( n < 3 )
  {
    cerr << "\n";
    cerr << "polygon_triangulate(): Fatal error!\n";
    cerr << "  N < 3.\n";
    exit ( 1 );
  }
//
//  Consecutive vertices cannot be equal.
//
  node_m1 = n - 1;
  for ( node = 0; node < n; node++ )
  {
    if ( x[node_m1] == x[node] && y[node_m1] == y[node] )
    {
      cerr << "\n";
      cerr << "polygon_triangulate(): Fatal error!\n";
      cerr << "  Two consecutive nodes are identical.\n";
      exit ( 1 );
    }
    node_m1 = node;
  }
//
//  Area must be positive.
//
  area = 0.0;
  for ( node = 0; node < n - 2; node++ )
  {
    area = area + 0.5 * 
    ( 
        ( x[node+1] - x[node] ) * ( y[node+2] - y[node] ) 
      - ( x[node+2] - x[node] ) * ( y[node+1] - y[node] )
    );
  }

  if ( area <= 0.0 )
  {
    cerr << "\n";
    cerr << "polygon_triangulate(): Fatal error!\n";
    cerr << "  Polygon has zero or negative area.\n";
    exit ( 1 );
  }

  triangles = new int[3*(n-2)];
//
//  PREV and NEXT point to the previous and next nodes.
//
  prev = new int[n];
  next = new int[n];

  i = 0;
  prev[i] = n - 1;
  next[i] = i + 1;

  for ( i = 1; i < n - 1; i++ )
  {
    prev[i] = i - 1;
    next[i] = i + 1;
  }

  i = n - 1;
  prev[i] = i - 1;
  next[i] = 0;
//
//  EAR indicates whether the node and its immediate neighbors form an ear
//  that can be sliced off immediately.
//
  ear = new bool[n];
  for ( i = 0; i < n; i++ )
  {
    ear[i] = diagonal ( prev[i], next[i], n, prev, next, x, y );
  }

  triangle_num = 0;

  i2 = 0;

  while ( triangle_num < n - 3 )
  {
//
//  If I2 is an ear, gather information necessary to carry out
//  the slicing operation and subsequent "healing".
//
    if ( ear[i2] )
    {
      i3 = next[i2];
      i4 = next[i3];
      i1 = prev[i2];
      i0 = prev[i1];
//
//  Make vertex I2 disappear.
//
      next[i1] = i3;
      prev[i3] = i1;
//
//  Update the earity of I1 and I3, because I2 disappeared.
//
      ear[i1] = diagonal ( i0, i3, n, prev, next, x, y );
      ear[i3] = diagonal ( i1, i4, n, prev, next, x, y );
//
//  Add the diagonal [I3, I1, I2] to the list.
//
      triangles[0+triangle_num*3] = i3;
      triangles[1+triangle_num*3] = i1;
      triangles[2+triangle_num*3] = i2;
      triangle_num = triangle_num + 1;
    }
//
//  Try the next vertex.
//
    i2 = next[i2];
  }
//
//  The last triangle is formed from the three remaining vertices.
//
  i3 = next[i2];
  i1 = prev[i2];

  triangles[0+triangle_num*3] = i3;
  triangles[1+triangle_num*3] = i1;
  triangles[2+triangle_num*3] = i2;
  triangle_num = triangle_num + 1;

  delete [] ear;
  delete [] next;
  delete [] prev;

  return triangles;
}
//****************************************************************************80

double r8_degrees ( double radians )

//****************************************************************************80
//
//  Purpose:
//
//    r8_degrees() converts an angle from radian to degree measure.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    15 May 2013
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    double RADIANS, the angle measurement in radians.
//
//  Output:
//
//    double R8_DEGREES, the angle measurement in degrees.
//
{
  const double r8_pi = 3.1415926535897932384626434;
  double value;

  value = radians * 180.0 / r8_pi;

  return value;
}
//****************************************************************************80

double r8_uniform_01 ( int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    r8_uniform_01() returns a unit pseudorandom R8.
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
//    09 April 2012
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int &SEED, the "seed" value.  Normally, this
//    value should not be 0.  
//
//  Output:
//
//    double R8_UNIFORM_01, a new pseudorandom variate, 
//    strictly between 0 and 1.
//
//    int &SEED, an updated random number seed.
{
  const int i4_huge = 2147483647;
  int k;
  double r;

  if ( seed == 0 )
  {
    cerr << "\n";
    cerr << "r8_uniform_01(): Fatal error!\n";
    cerr << "  Input value of SEED = 0.\n";
    exit ( 1 );
  }

  k = seed / 127773;

  seed = 16807 * ( seed - k * 127773 ) - k * 2836;

  if ( seed < 0 )
  {
    seed = seed + i4_huge;
  }
  r = ( double ) ( seed ) * 4.656612875E-10;

  return r;
}
//****************************************************************************80

int r8mat_solve ( int n, int rhs_num, double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    r8mat_solve() uses Gauss-Jordan elimination to solve an N by N linear system.
//
//  Discussion:
//
//    An R8MAT is a doubly dimensioned array of R8 values, stored as a vector
//    in column-major order.
//
//    Entry A(I,J) is stored as A[I+J*N]
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
//  Input:
//
//    int N, the order of the matrix.
//
//    int RHS_NUM, the number of right hand sides.  RHS_NUM
//    must be at least 0.
//
//    double A[N*(N+RHS_NUM)], contains in rows and columns 1
//    to N the coefficient matrix, and in columns N+1 through
//    N+RHS_NUM, the right hand sides.  
//
//  Output:
//
//    int R8MAT_SOLVE, singularity flag.
//    0, the matrix was not singular, the solutions were computed;
//    J, factorization failed on step J, and the solutions could not
//    be computed.
//
//    double A[N*(N+RHS_NUM)], the coefficient matrix
//    area has been destroyed, while the right hand sides have
//    been overwritten with the corresponding solutions.
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

void r8mat_transpose_print ( int m, int n, double a[], string title )

//****************************************************************************80
//
//  Purpose:
//
//    r8mat_transpose_print() prints an R8MAT, transposed.
//
//  Discussion:
//
//    An R8MAT is a doubly dimensioned array of R8 values, stored as a vector
//    in column-major order.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 September 2009
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int M, N, the number of rows and columns.
//
//    double A[M*N], an M by N matrix to be printed.
//
//    string TITLE, a title.
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
//    r8mat_transpose_print_some() prints some of an R8MAT, transposed.
//
//  Discussion:
//
//    An R8MAT is a doubly dimensioned array of R8 values, stored as a vector
//    in column-major order.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    07 April 2014
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int M, N, the number of rows and columns.
//
//    double A[M*N], an M by N matrix to be printed.
//
//    int ILO, JLO, the first row and column to print.
//
//    int IHI, JHI, the last row and column to print.
//
//    string TITLE, a title.
//
{
# define INCX 5

  int i;
  int i2;
  int i2hi;
  int i2lo;
  int i2lo_hi;
  int i2lo_lo;
  int inc;
  int j;
  int j2hi;
  int j2lo;

  cout << "\n";
  cout << title << "\n";

  if ( m <= 0 || n <= 0 )
  {
    cout << "\n";
    cout << "  (None)\n";
    return;
  }

  if ( ilo < 1 )
  {
    i2lo_lo = 1;
  }
  else
  {
    i2lo_lo = ilo;
  }

  if ( ihi < m )
  {
    i2lo_hi = m;
  }
  else
  {
    i2lo_hi = ihi;
  }

  for ( i2lo = i2lo_lo; i2lo <= i2lo_hi; i2lo = i2lo + INCX )
  {
    i2hi = i2lo + INCX - 1;

    if ( m < i2hi )
    {
      i2hi = m;
    }
    if ( ihi < i2hi )
    {
      i2hi = ihi;
    }

    inc = i2hi + 1 - i2lo;

    cout << "\n";
    cout << "  Row: ";
    for ( i = i2lo; i <= i2hi; i++ )
    {
      cout << setw(7) << i - 1 << "       ";
    }
    cout << "\n";
    cout << "  Col\n";
    cout << "\n";

    if ( jlo < 1 )
    {
      j2lo = 1;
    }
    else
    {
      j2lo = jlo;
    }
    if ( n < jhi )
    {
      j2hi = n;
    }
    else
    {
      j2hi = jhi;
    }

    for ( j = j2lo; j <= j2hi; j++ )
    {
      cout << setw(5) << j - 1 << ":";
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
//  Input:
//
//    int N, the number of entries in A.
//
//    double A[N], the vector whose L2 norm is desired.
//
//  Output:
//
//    double R8VEC_NORM, the L2 norm of A.
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

void r8vec_print ( int n, double a[], string title )

//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_PRINT prints an R8VEC.
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
//    16 August 2004
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of components of the vector.
//
//    double A[N], the vector to be printed.
//
//    string TITLE, a title.
//
{
  int i;

  cout << "\n";
  cout << title << "\n";
  cout << "\n";
  for ( i = 0; i < n; i++ )
  {
    cout << "  " << setw(8)  << i
         << ": " << setw(14) << a[i]  << "\n";
  }

  return;
}
//****************************************************************************80

double r8vec_sum ( int n, double a[] )

//****************************************************************************80
//
//  Purpose:
//
//    r8vec_sum() returns the sum of an R8VEC.
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
//    15 October 2004
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of entries in the vector.
//
//    double A[N], the vector.
//
//  Output:
//
//    double R8VEC_SUM, the sum of the vector.
//
{
  int i;
  double value;

  value = 0.0;
  for ( i = 0; i < n; i++ )
  {
    value = value + a[i];
  }
  return value;
}
//****************************************************************************80

double *r8vec_uniform_01_new ( int n, int &seed )

//****************************************************************************80
//
//  Purpose:
//
//    r8vec_uniform_01_new() returns a new unit pseudorandom R8VEC.
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
//    19 August 2004
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    int N, the number of entries in the vector.
//
//    int &SEED, a seed for the random number generator.
//
//  Output:
//
//    double R8VEC_UNIFORM_01_NEW[N], the vector of pseudorandom values.
//
//    int &SEED, an updated random number seed.
//
{
  int i;
  const int i4_huge = 2147483647;
  int k;
  double *r;

  if ( seed == 0 )
  {
    cerr << "\n";
    cerr << "R8VEC_UNIFORM_01_NEW(): Fatal error!\n";
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

    r[i] = ( double ) ( seed ) * 4.656612875E-10;
  }

  return r;
}
//****************************************************************************80

double segment_point_dist ( double p1[2], double p2[2], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    segment_point_dist(): distance ( line segment, point ).
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
//    30 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    double P1[2], P2[2], the endpoints of the line segment.
//
//    double P[2], the point whose nearest neighbor on the line
//    segment is to be determined.
//
//  Output:
//
//    double SEGMENT_POINT_DIST, the distance from the point 
//    to the line segment.
//
{
  double bot;
  double dist;
  int i;
  double t;
  double pn[2];
//
//  If the line segment is actually a point, then the answer is easy.
//
  if ( p1[0] == p2[0] && p1[1] == p2[1] )
  {
    t = 0.0;
  }
  else
  {
    bot = 0.0;
    for ( i = 0; i < 2; i++ )
    {
      bot = bot + pow ( p2[i] - p1[i], 2 );
    }

    t = 0.0;
    for ( i = 0; i < 2; i++ )
    {
      t = t + ( p[i] - p1[i] ) * ( p2[i] - p1[i] );
    }

    t = t / bot;
    if ( t < 0.0 )
    {
      t = 0.0;
    }
    if ( 1.0 < t )
    {
      t = 1.0;
    }
  }

  for ( i = 0; i < 2; i++ )
  {
    pn[i] = p1[i] + t * ( p2[i] - p1[i] );
  }

  dist = 0.0;
  for ( i = 0; i < 2; i++ )
  {
    dist = dist + pow ( p[i] - pn[i], 2 );
  }
  dist = sqrt ( dist );

  return dist;
}
//****************************************************************************80

double *segment_point_near ( double p1[2], double p2[2], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    segment_point_near() finds the point on a line segment nearest a point.
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
//    30 May 2010
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    double P1[2], P2[2], the two endpoints of the line segment.
//
//    double P[2], the point whose nearest neighbor
//    on the line segment is to be determined.
//
//  Output:
//
//    double SEGMENT_POINT_NEAR[2], the point on the line segment 
//    which is nearest P.
//
{
  double bot;
  int i;
  double *pn;
  double t;
//
//  If the line segment is actually a point, then the answer is easy.
//
  if ( p1[0] == p2[0] && p1[1] == p2[1] )
  {
    t = 0.0;
  }
  else
  {
    bot = 0.0;
    for ( i = 0; i < 2; i++ )
    {
      bot = bot + pow ( p2[i] - p1[i], 2 );
    }

    t = 0.0;
    for ( i = 0; i < 2; i++ )
    {
      t = t + ( p[i] - p1[i] ) * ( p2[i] - p1[i] );
    }

    t = t / bot;
    if ( t < 0.0 )
    {
      t = 0.0;
    }
    if ( 1.0 < t )
    {
      t = 1.0;
    }
  }

  pn = new double[2];

  for ( i = 0; i < 2; i++ )
  {
    pn[i] = p1[i] + t * ( p2[i] - p1[i] );
  }

  return pn;
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

double triangle_area ( double xa, double ya, double xb, double yb, double xc, 
  double yc )

//****************************************************************************80
//
//  Purpose:
//
//    triangle_area() computes the signed area of a triangle.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    05 May 2014
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    double XA, YA, XB, YB, XC, YC, the coordinates of
//    the vertices of the triangle, given in counterclockwise order.
//
//  Output:
//
//    double TRIANGLE_AREA, the signed area of the triangle.
//
{
  double value;

  value = 0.5 * ( 
      ( xb - xa ) * ( yc - ya ) 
    - ( xc - xa ) * ( yb - ya ) );

  return value;
}
//****************************************************************************80

double *triangle_barycentric ( double t[2*3], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    triangle_barycentric() finds the barycentric coordinates of a point.
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
//  Input:
//
//    double T[2*3], the vertices of the triangle.
//
//    double P[2], the point to be checked.
//
//  Output:
//
//    double C[3], the barycentric coordinates of the point with respect
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
    cerr << "triangle_barycentric(): Fatal error!\n";
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

bool triangle_contains_point_1 ( double t[2*3], double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    triangle_contains_point_1() finds if a point is inside a triangle.
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
//  Input:
//
//    double T[2*3], the triangle vertices.
//
//    double P[2], the point to be checked.
//
//  Output:
//
//    bool TRIANGLE_CONTAINS_POINT_1, is TRUE if the points 
//    is inside the triangle or on its boundary, and FALSE otherwise.
//
{
  double *c;
  int i;
  int value;

  c = triangle_barycentric ( t, p );

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
 
