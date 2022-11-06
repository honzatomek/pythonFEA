# include <cfloat>
# include <cmath>
# include <cstdlib>
# include <iomanip>
# include <iostream>

using namespace std;

# include "ellipse.hpp"

//****************************************************************************80

double ellipse_area1 ( double a[], double r )

//****************************************************************************80
//
//  Purpose:
//
//    ellipse_area1() returns the area of an ellipse defined by a matrix.
//
//  Discussion:
//
//    The points X in the ellipse are described by a 2 by 2
//    positive definite symmetric matrix A, and a "radius" R, such that
//      X' * A * X <= R * R
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    13 August 2014
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    double A[2*2], the matrix that describes
//    the ellipse.  A must be symmetric and positive definite.
//
//    double R, the "radius" of the ellipse.
//
//  Output:
//
//    double ellipse_area1, the area of the ellipse.
//
{
  const double r8_pi = 3.141592653589793;
  double value;

  value = r * r * r8_pi / sqrt ( a[0+0*2] * a[1+1*2] - a[1+0*2] * a[0+1*2] );

  return value;
}
//****************************************************************************80

double ellipse_area2 ( double a, double b, double c, double d )

//****************************************************************************80
//
//  Purpose:
//
//    ellipse_area2() returns the area of an ellipse defined by an equation.
//
//  Discussion:
//
//    The ellipse is described by the formula
//      a x^2 + b xy + c y^2 = d
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    08 November 2016
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    double A, B, C, coefficients on the left hand side.
//
//    double D, the right hand side.
//
//  Output:
//
//    double ellipse_area2, the area of the ellipse.
//
{
  const double r8_pi = 3.141592653589793;
  double value;

  value = 2.0 * d * d * r8_pi / sqrt ( 4.0 * a * c - b * b );

  return value;
}
//****************************************************************************80

double ellipse_area3 ( double r1, double r2 )

//****************************************************************************80
//
//  Purpose:
//
//    ellipse_area3() returns the area of an ellipse in 2D.
//
//  Discussion:
//
//    An ellipse in standard position has a center at the origin, and
//    axes aligned with the coordinate axes.  Any point P on the ellipse
//    satisfies
//
//      pow (  P[0] / R1, 2 ) + pow ( P[1] / R2, 2 ) == 1
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
//  Input:
//
//    double R1, R2, the "radius" of the ellipse in the major
//    and minor axis directions.  A circle has these values equal.
//
//  Output:
//
//    double ellipse_area3, the area of the ellipse.
//
{
  double area;
  double r8_pi = 3.141592653589793;

  area = r8_pi * r1 * r2;

  return area;
}
//****************************************************************************80

double ellipse_aspect_ratio ( double a, double b )

//****************************************************************************80
//
//  Purpose:
//
//    ellipse_aspect_ratio() computes the aspect ratio of an ellipse.
//
//  Discussion:
//
//    The ellipse has major and minor semi-axes a and b.  In particular, it
//    could have the form:
//
//      (x/a)^2 + (y/b)^2 = 1
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    14 October 2022
//
//  Reference:
//
//    John D Cook,
//    Eccentricity, Flattening, and Aspect Ratio,
//    https://www.johndcook.com/blog/2022/10/14/eccentricity-flatness-aspect/
//    Posted 14 October 2022.
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    double A, B, the major and minor semi-axes.
//
//  Output:
//
//    double ellipse_aspect_ratio: the aspect ratio of the ellipse.
//
{
  double t;
  double value;

  a = fabs ( a );
  b = fabs ( b );

  if ( a < b )
  {
    t = a;
    a = b;
    b = t;
  }

  value = b / a;

  return value;
}
//****************************************************************************80

double ellipse_eccentricity ( double a, double b )

//****************************************************************************80
//
//  Purpose:
//
//    ellipse_eccentricity() computes the eccentricity of an ellipse.
//
//  Discussion:
//
//    The ellipse has major and minor semi-axes a and b.  In particular, it
//    could have the form:
//
//      (x/a)^2 + (y/b)^2 = 1
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    14 October 2022
//
//  Reference:
//
//    John D Cook,
//    Eccentricity, Flattening, and Aspect Ratio,
//    https://www.johndcook.com/blog/2022/10/14/eccentricity-flatness-aspect/
//    Posted 14 October 2022.
//
//  Author:
//
//    John Burkardt
//
//  Input:
//
//    double A, B, the major and minor semi-axes.
//
//  Output:
//
//    double ellipse_eccentricity: the eccentricity of the ellipse.
//
{
  double ecc;
  double t;

  a = fabs ( a );
  b = fabs ( b );

  if ( a < b )
  {
    t = a;
    a = b;
    b = t;
  }

  ecc = sqrt ( 1.0 - pow ( b / a, 2 ) );

  return ecc;
}
//****************************************************************************80

double ellipse_flattening ( double a, double b )

//****************************************************************************80
//
//  Purpose:
//
//    ellipse_flattening() computes the flattening of an ellipse.
//
//  Discussion:
//
//    The ellipse has major and minor semi-axes a and b.  In particular, it
//    could have the form:
//
//      (x/a)^2 + (y/b)^2 = 1
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    14 October 2022
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    John D Cook,
//    Eccentricity, Flattening, and Aspect Ratio,
//    https://www.johndcook.com/blog/2022/10/14/eccentricity-flatness-aspect/
//    Posted 14 October 2022.
//
//  Input:
//
//    double A, B, the major and minor semi-axes.
//
//  Output:
//
//    double ellipse_flattening, the flattening of the ellipse.
//
{
  double t;
  double value;

  a = fabs ( a );
  b = fabs ( b );

  if ( a < b )
  {
    t = a;
    a = b;
    b = t;
  }

  value = ( a - b ) / a;

  return value;
}
//****************************************************************************80

double ellipse_point_dist_2d ( double r1, double r2, double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    ellipse_point_dist_2d() finds the distance from a point to an ellipse in 2D.
//
//  Discussion:
//
//    An ellipse in standard position has a center at the origin, and
//    axes aligned with the coordinate axes.  Any point P on the ellipse
//    satisfies
//
//      pow (  P[0] / R1, 2 ) + pow ( P[1] / R2, 2 ) == 1
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
//    Dianne O'Leary,
//    Elastoplastic Torsion: Twist and Stress,
//    Computing in Science and Engineering,
//    July/August 2004, pages 74-76.
//    September/October 2004, pages 63-65.
//
//  Input:
//
//    double R1, R2, the ellipse parameters.  Normally,
//    these are both positive quantities.  Generally, they are also
//    distinct.
//
//    double P[2], the point.
//
//  Output:
//
//    double ELLIPSE_POINT_DIST_2D, the distance to the ellipse.
//
{
# define DIM_NUM 2

  double dist;
  int i;
  double *pn;

  pn = ellipse_point_near_2d ( r1, r2, p );

  dist = 0.0;
  for ( i = 0; i < DIM_NUM; i++ )
  {
    dist = dist + pow ( p[i] - pn[i], 2 );
  }
  dist = sqrt ( dist );

  delete [] pn;

  return dist;
# undef DIM_NUM
}
//****************************************************************************80

double *ellipse_point_near_2d ( double r1, double r2, double p[2] )

//****************************************************************************80
//
//  Purpose:
//
//    ellipse_point_near_2d() finds the nearest point on an ellipse in 2D.
//
//  Discussion:
//
//    An ellipse in standard position has a center at the origin, and
//    axes aligned with the coordinate axes.  Any point P on the ellipse
//    satisfies
//
//      (  P(1) / R1 )^2 + ( P(2) / R2 )^2 == 1
//
//    The nearest point PN on the ellipse has the property that the
//    line from PN to P is normal to the ellipse.  Points on the ellipse
//    can be parameterized by T, to have the form
//
//      ( R1 * cos ( T ), R2 * sin ( T ) ).
//
//    The tangent vector to the ellipse has the form
//
//      ( -R1 * sin ( T ), R2 * cos ( T ) )
//
//    At PN, the dot product of this vector with  ( P - PN ) must be
//    zero:
//
//      - R1 * sin ( T ) * ( X - R1 * cos ( T ) )
//      + R2 * cos ( T ) * ( Y - R2 * sin ( T ) ) = 0
//
//    This nonlinear equation for T can be solved by Newton's method.
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
//  Input:
//
//    double R1, R2, the ellipse parameters.  Normally,
//    these are both positive quantities.  Generally, they are also
//    distinct.
//
//    double P[2], the point.
//
//  Output:
//
//    double ELLIPSE_POINT_NEAR_2D[2], the point on the ellipse which
//    is closest to P.
//
{
# define DIM_NUM 2

  double ct;
  double f;
  double fp;
  int iteration;
  int iteration_max = 100;
  double r8_pi = 3.141592653589793;
  double *pn;
  double st;
  double t;
  double x;
  double y;

  x = fabs ( p[0] );
  y = fabs ( p[1] );

  if ( y == 0.0 && r1 * r1 - r2 * r2 <= r1 * x )
  {
    t = 0.0;
  }
  else if ( x == 0.0 && r2 * r2 - r1 * r1 <= r2 * y )
  {
    t = r8_pi / 2.0;
  }
  else
  {
    if ( y == 0.0 )
    {
      y = sqrt ( DBL_EPSILON ) * fabs ( r2 );
    }

    if ( x == 0.0 )
    {
      x = sqrt ( DBL_EPSILON ) * fabs ( r1 );
    }
//
//  Initial parameter T:
//
    t = atan2 ( y, x );

    iteration = 0;

    for ( ; ; )
    {
      ct = cos ( t );
      st = sin ( t );

      f = ( x - fabs ( r1 ) * ct ) * fabs ( r1 ) * st
        - ( y - fabs ( r2 ) * st ) * fabs ( r2 ) * ct;

      if ( fabs ( f ) <= 100.0 * DBL_EPSILON )
      {
        break;
      }

      if ( iteration_max <= iteration )
      {
        cout << "\n";
        cout << "ELLIPSE_POINT_NEAR_2D - Warning!\n";
        cout << "  Reached iteration limit.\n";
        cout << "  T = " << t << "\n";
        cout << "  F = " << f << "\n";
        break;
      }

      iteration = iteration + 1;

      fp = r1 * r1 * st * st + r2 * r2 * ct * ct
         + ( x - fabs ( r1 ) * ct ) * fabs ( r1 ) * ct
         + ( y - fabs ( r2 ) * st ) * fabs ( r2 ) * st;

      t = t - f / fp;
    }
  }
//
//  From the T value, we get the nearest point.
//
  pn = new double[DIM_NUM];

  pn[0] = fabs ( r1 ) * cos ( t );
  pn[1] = fabs ( r2 ) * sin ( t );
//
//  Take care of case where the point was in another quadrant.
//
  pn[0] = r8_sign ( p[0] ) * pn[0];
  pn[1] = r8_sign ( p[1] ) * pn[1];

  return pn;
# undef DIM_NUM
}
//****************************************************************************80

void ellipse_points_2d ( double pc[2], double r1, double r2, double psi,
  int n, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    ellipse_points_2d() returns N points on an tilted ellipse in 2D.
//
//  Discussion:
//
//    An ellipse in standard position has a center at the origin, and
//    axes aligned with the coordinate axes.  Any point P on the ellipse
//    satisfies
//
//      pow (  P[0] / R1, 2 ) + pow ( P[1] / R2, 2 ) == 1
//
//    The points are "equally spaced" in the angular sense.  They are
//    not equally spaced along the perimeter of the ellipse.
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
//  Input:
//
//    double PC[2], the coordinates of the center of the ellipse.
//
//    double R1, R2, the "radius" of the ellipse in the major
//    and minor axis directions.  A circle has these values equal.
//
//    double PSI, the angle that the major axis of the ellipse
//    makes with the X axis.  A value of 0.0 means that the major and
//    minor axes of the ellipse will be the X and Y coordinate axes.
//
//    int N, the number of points desired.  N must be at least 1.
//
//  Output:
//
//    double P[2*N], the coordinates of points on the ellipse.
//
{
  int i;
  double r8_pi = 3.141592653589793;
  double theta;

  for ( i = 0; i < n; i++ )
  {
    theta = ( 2.0 * r8_pi * ( ( double ) i ) ) / ( ( double ) n );

    p[0+i*2] = pc[0] + r1 * cos ( psi ) * cos ( theta )
                     - r2 * sin ( psi ) * sin ( theta );

    p[1+i*2] = pc[1] + r1 * sin ( psi ) * cos ( theta )
                     + r2 * cos ( psi ) * sin ( theta );

  }

  return;
}
//****************************************************************************80

void ellipse_points_arc_2d ( double pc[2], double r1, double r2, double psi,
  double theta1, double theta2, int n, double p[] )

//****************************************************************************80
//
//  Purpose:
//
//    ellipse_points_arc_2d() returns N points on a tilted elliptical arc in 2D.
//
//  Discussion:
//
//    An ellipse in standard position has a center at the origin, and
//    axes aligned with the coordinate axes.  Any point P on the ellipse
//    satisfies
//
//      pow (  P[0] / R1, 2 ) + pow ( P[1] / R2, 2 ) == 1
//
//    The points are "equally spaced" in the angular sense.  They are
//    not equally spaced along the perimeter of the ellipse.
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
//  Input:
//
//    double PC[2], the coordinates of the center of the ellipse.
//
//    double R1, R2, the "radius" of the ellipse in the major
//    and minor axis directions.  A circle has these values equal.
//
//    double PSI, the angle that the major axis of the ellipse
//    makes with the X axis.  A value of 0.0 means that the major and
//    minor axes of the ellipse will be the X and Y coordinate axes.
//
//    double THETA1, THETA2, the angular coordinates of the first
//    and last points to be drawn, in radians.  This angle is measured
//    with respect to the (possibly tilted) major axis.
//
//    int N, the number of points desired.  N must be at least 1.
//
//  Output:
//
//    double P[2*N], the coordinates of points on the ellipse.
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

    p[0+i*2] = pc[0] + r1 * cos ( psi ) * cos ( theta )
                     - r2 * sin ( psi ) * sin ( theta );

    p[1+i*2] = pc[1] + r1 * sin ( psi ) * cos ( theta )
                     + r2 * cos ( psi ) * sin ( theta );

  }

  return;
}
//****************************************************************************80

double r8_modp ( double x, double y )

//****************************************************************************80
//
//  Purpose:
//
//    r8_modp() returns the nonnegative remainder of R8 division.
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
//  Input:
//
//    double X, the number to be divided.
//
//    double Y, the number that divides X.
//
//  Output:
//
//    double R8_MODP, the nonnegative remainder when X is divided by Y.
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

double r8_sign ( double x )

//****************************************************************************80
//
//  Purpose:
//
//    r8_sign() returns the "sign" of an R8.
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
//  Input:
//
//    double X, the argument.
//
//  Output:
//
//    double R8_SIGN, the sign of X.
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
