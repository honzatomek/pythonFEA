# include <float.h>
# include <math.h>
# include <stdbool.h>
# include <stdio.h>
# include <stdlib.h>

# include "ellipse.h"

/******************************************************************************/

double ellipse_area1 ( double a[], double r )

/******************************************************************************/
/*
  Purpose:

    ellipse_area1() returns the area of an ellipse in quadratic form.

  Discussion:

    The points X in the ellipse are described by a 2 by 2
    positive definite symmetric matrix A, and a "radius" R, such that
      X' * A * X <= R * R

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    13 August 2014

  Author:

    John Burkardt

  Input:

    double A[2*2], the matrix that describes
    the ellipse.  A must be symmetric positive definite.

    double R, the "radius" of the ellipse.

  Output:

    double ELLIPSE_AREA1, the area of the ellipse.
*/
{
  const double r8_pi = 3.141592653589793;
  double value;

  value = r * r * r8_pi / sqrt ( ( a[0+0*2] * a[1+1*2] - a[1+0*2] * a[0+1*2] ) );

  return value;
}
/******************************************************************************/

double ellipse_area2 ( double a, double b, double c, double d )

/******************************************************************************/
/*
  Purpose:

    ellipse_area2() returns the area of an ellipse defined by an equation.

  Discussion:

    The ellipse is described by the formula
      a x^2 + b xy + c y^2 = d

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    08 November 2016

  Author:

    John Burkardt

  Input:

    double A, B, C, coefficients on the left hand side.

    double D, the right hand side.

  Output:

    double ELLIPSE_AREA2, the area of the ellipse.
*/
{
  const double r8_pi = 3.141592653589793;
  double value;

  value = 2.0 * d * d * r8_pi / sqrt ( 4.0 * a * c - b * b );

  return value;
}
/******************************************************************************/

double ellipse_area3 ( double r1, double r2 )

/******************************************************************************/
/*
  Purpose:

    ellipse_area3() returns the area of an ellipse in standard form.

  Discussion:

    An ellipse in standard form has a center at the origin, and
    axes aligned with the coordinate axes.  Any point P on the ellipse
    satisfies

      (  X / A )^2 +  ( Y / B )^2 == 1

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    17 May 2010

  Author:

    John Burkardt

  Input:

    double A, B, the lengths of the major and minor semi-axes.

  Output:

    double ELLIPSE_AREA3, the area of the ellipse.
*/
{
  double area;
  const double r8_pi = 3.141592653589793;

  area = r8_pi * r1 * r2;

  return area;
}
/******************************************************************************/

double ellipse_aspect_ratio ( double a, double b )

/******************************************************************************/
/*
  Purpose:

    aspect_ratio() computes the aspect ratio of an ellipse.

  Discussion:

    An ellipse in standard form has a center at the origin, and
    axes aligned with the coordinate axes.  Any point P on the ellipse
    satisfies

      (  X / A )^2 +  ( Y / B )^2 == 1

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    14 October 2022

  Author:

    John Burkardt

  Reference:

    John D Cook,
    Eccentricity, Flattening, and Aspect Ratio,
    https://www.johndcook.com/blog/2022/10/14/eccentricity-flatness-aspect/
    Posted 14 October 2022.

  Input:

    double A, B: the major and minor semi-axes.

  Output:

    double ellipse_aspect_ratio: the aspect ratio of the ellipse.
*/
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
/******************************************************************************/

double ellipse_eccentricity ( double a, double b )

/******************************************************************************/
/*
  Purpose:

    ellipse_eccentricity() computes the eccentricity of an ellipse.

  Discussion:

    An ellipse in standard form has a center at the origin, and
    axes aligned with the coordinate axes.  Any point P on the ellipse
    satisfies

      (  X / A )^2 +  ( Y / B )^2 == 1

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    24 March 2021

  Author:

    John Burkardt

  Reference:

    John D Cook,
    Eccentricity, Flattening, and Aspect Ratio,
    https://www.johndcook.com/blog/2022/10/14/eccentricity-flatness-aspect/
    Posted 14 October 2022.

  Input:

    double A, B, the major and minor semi-axes.

  Output:

    double ellipse_eccentricity: the eccentricity of the ellipse.
*/
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
/******************************************************************************/

double ellipse_flattening ( double a, double b )

/******************************************************************************/
/*
  Purpose:

    ellipse_flattening() computes the flattening of an ellipse.

  Discussion:

    An ellipse in standard form has a center at the origin, and
    axes aligned with the coordinate axes.  Any point P on the ellipse
    satisfies

      (  X / A )^2 +  ( Y / B )^2 == 1

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    14 October 2022

  Author:

    John Burkardt

  Reference:

    John D Cook,
    Eccentricity, Flattening, and Aspect Ratio,
    https://www.johndcook.com/blog/2022/10/14/eccentricity-flatness-aspect/
    Posted 14 October 2022.

  Input:

    double A, B, the major and minor semi-axes.

  Output:

    double ellipse_flattening: the flattening of the ellipse.
*/
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
/******************************************************************************/

double ellipse_point_dist_2d ( double r1, double r2, double p[2] )

/******************************************************************************/
/*
  Purpose:

    ellipse_point_dist_2d() finds the distance from a point to an ellipse in 2D.

  Discussion:

    An ellipse in standard form has a center at the origin, and
    axes aligned with the coordinate axes.  Any point P on the ellipse
    satisfies

      (  X / A )^2 +  ( Y / B )^2 == 1

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    17 May 2010

  Author:

    John Burkardt

  Reference:

    Dianne O'Leary,
    Elastoplastic Torsion: Twist and Stress,
    Computing in Science and Engineering,
    July/August 2004, pages 74-76.
    September/October 2004, pages 63-65.

  Input:

    double R1, R2, the ellipse parameters.  Normally,
    these are both positive quantities.  Generally, they are also
    distinct.

    double P[2], the point.

  Output:

    double ELLIPSE_POINT_DIST_2D, the distance to the ellipse.
*/
{
  double dist;
  int i;
  double *pn;

  pn = ellipse_point_near_2d ( r1, r2, p );

  dist = 0.0;
  for ( i = 0; i < 2; i++ )
  {
    dist = dist + pow ( p[i] - pn[i], 2 );
  }
  dist = sqrt ( dist );

  free ( pn );

  return dist;
}
/******************************************************************************/

double *ellipse_point_near_2d ( double r1, double r2, double p[2] )

/******************************************************************************/
/*
  Purpose:

    ellipse_point_near_2d() finds the nearest point on an ellipse in 2D.

  Discussion:

    An ellipse in standard form has a center at the origin, and
    axes aligned with the coordinate axes.  Any point P on the ellipse
    satisfies

      (  X / A )^2 +  ( Y / B )^2 == 1

    The nearest point PN on the ellipse has the property that the
    line from PN to P is normal to the ellipse.  Points on the ellipse
    can be parameterized by T, to have the form

      ( R1 * cos ( T ), R2 * sin ( T ) ).

    The tangent vector to the ellipse has the form

      ( -R1 * sin ( T ), R2 * cos ( T ) ) 

    At PN, the dot product of this vector with  ( P - PN ) must be
    zero:

      - R1 * sin ( T ) * ( X - R1 * cos ( T ) )
      + R2 * cos ( T ) * ( Y - R2 * sin ( T ) ) = 0

    This nonlinear equation for T can be solved by Newton's method.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    17 May 2010

  Author:

    John Burkardt

  Input:

    double R1, R2, the ellipse parameters.  Normally,
    these are both positive quantities.  Generally, they are also
    distinct.

    Input, double P[2], the point.

  Output:

    double ELLIPSE_POINT_NEAR_2D[2], the point on the ellipse which
    is closest to P.
*/
{
  double ct;
  double f;
  double fp;
  int iteration;
  int iteration_max = 100;
  const double r8_pi = 3.141592653589793;
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
/*
  Initial parameter T:
*/
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
        fprintf ( stderr, "\n" );
        fprintf ( stderr, "ELLIPSE_POINT_NEAR_2D - Warning!\n" );
        fprintf ( stderr, "  Reached iteration limit.\n" );
        fprintf ( stderr, "  T = %f\n", t );
        fprintf ( stderr, "  F = %f\n", f );
        break;
      }

      iteration = iteration + 1;

      fp = r1 * r1 * st * st + r2 * r2 * ct * ct 
         + ( x - fabs ( r1 ) * ct ) * fabs ( r1 ) * ct 
         + ( y - fabs ( r2 ) * st ) * fabs ( r2 ) * st;

      t = t - f / fp;
    }
  }
/*
  From the T value, we get the nearest point.
*/
  pn = ( double * ) malloc ( 2 * sizeof ( double ) );

  pn[0] = fabs ( r1 ) * cos ( t );
  pn[1] = fabs ( r2 ) * sin ( t );
/*
  Take care of case where the point was in another quadrant.
*/
  pn[0] = r8_sign ( p[0] ) * pn[0];
  pn[1] = r8_sign ( p[1] ) * pn[1];

  return pn;
}
/******************************************************************************/

void ellipse_points_2d ( double pc[2], double r1, double r2, double theta, 
  int n, double p[] )

/******************************************************************************/
/*
  Purpose:

    ellipse_points_2d() returns N points on an rotated ellipse.

  Discussion:

    An ellipse in standard form has a center at the origin, and
    axes aligned with the coordinate axes.  Any point P on the ellipse
    satisfies

      (  X / A )^2 +  ( Y / B )^2 == 1

    The points are "equally spaced" in the angular sense.  They are
    not equally spaced along the perimeter of the ellipse.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    17 May 2010

  Author:

    John Burkardt

  Input:

    double PC[2], the coordinates of the center of the ellipse.

    double R1, R2, the "radius" of the ellipse in the major
    and minor axis directions.  A circle has these values equal.

    double THETA, the angle that the major axis of the ellipse
    makes with the X axis.  A value of 0.0 means that the major and
    minor axes of the ellipse will be the X and Y coordinate axes.

    int N, the number of points desired.  N must be at least 1.

  Output:

    double P[2*N], the coordinates of points on the ellipse.
*/
{
  int i;
  double psi;
  const double r8_pi = 3.141592653589793;

  for ( i = 0; i < n; i++ )
  {
    psi = ( 2.0 * r8_pi * ( ( double ) i ) ) / ( ( double ) n );

    p[0+i*2] = pc[0] + r1 * cos ( theta ) * cos ( psi ) 
                     - r2 * sin ( theta ) * sin ( psi );

    p[1+i*2] = pc[1] + r1 * sin ( theta ) * cos ( psi ) 
                     + r2 * cos ( theta ) * sin ( psi );
  }

  return;
}
/******************************************************************************/

void ellipse_points_arc_2d ( double pc[2], double r1, double r2, double theta,
  double psi1, double psi2, int n, double p[] )

/******************************************************************************/
/*
  Purpose:

    ellipse_points_arc_2d() returns N points on a tilted elliptical arc in 2D.

  Discussion:

    An ellipse in standard form has a center at the origin, and
    axes aligned with the coordinate axes.  Any point P on the ellipse
    satisfies

      (  X / A )^2 +  ( Y / B )^2 == 1

    The points are "equally spaced" in the angular sense.  They are
    not equally spaced along the perimeter of the ellipse.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    17 May 2010

  Author:

    John Burkardt

  Input:

    double PC[2], the coordinates of the center of the ellipse.

    double R1, R2, the "radius" of the ellipse in the major
    and minor axis directions.  A circle has these values equal.

    double THETA, the angle that the major axis of the ellipse
    makes with the X axis.  A value of 0.0 means that the major and
    minor axes of the ellipse will be the X and Y coordinate axes.

    double PSI1, PSI2, the angular coordinates of the first
    and last points to be drawn, in radians.  This angle is measured
    with respect to the (possibly tilted) major axis.

    int N, the number of points desired.  N must be at least 1.

  Output:

    double P[2*N], the coordinates of points on the ellipse.
*/
{
  int i;
  const double r8_pi = 3.141592653589793;
  double psi;
  double psi3;
/*
  PSI3 is the smallest angle, no less than PSI1, which
  coincides with PSI2.
*/
  psi3 = psi1 + r8_modp ( psi2 - psi1, 2.0 * r8_pi );

  for ( i = 0; i < n; i++ )
  {
    if ( 1 < n )
    {
      psi = ( ( double ) ( n - i - 1 ) * psi1 
            + ( double ) (     i     ) * psi3 ) 
            / ( double ) ( n     - 1 );
    }
    else
    {
      psi = 0.5 * ( psi1 + psi3 );
    }

    p[0+i*2] = pc[0] + r1 * cos ( theta ) * cos ( psi ) 
                     - r2 * sin ( theta ) * sin ( psi );

    p[1+i*2] = pc[1] + r1 * sin ( theta ) * cos ( psi ) 
                     + r2 * cos ( theta ) * sin ( psi );
  }

  return;
}
/******************************************************************************/

double r8_modp ( double x, double y )

/******************************************************************************/
/*
  Purpose:

    r8_modp() returns the nonnegative remainder of R8 division.

  Formula:

    If
      REM = R8_MODP ( X, Y )
      RMULT = ( X - REM ) / Y
    then
      X = Y * RMULT + REM
    where REM is always nonnegative.

  Discussion:

    The MOD function computes a result with the same sign as the
    quantity being divided.  Thus, suppose you had an angle A,
    and you wanted to ensure that it was between 0 and 360.
    Then mod(A,360.0) would do, if A was positive, but if A
    was negative, your result would be between -360 and 0.

    On the other hand, R8_MODP(A,360.0) is between 0 and 360, always.

  Example:

        I         J     MOD  R8_MODP   R8_MODP Factorization

      107        50       7       7    107 =  2 *  50 + 7
      107       -50       7       7    107 = -2 * -50 + 7
     -107        50      -7      43   -107 = -3 *  50 + 43
     -107       -50      -7      43   -107 =  3 * -50 + 43

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    05 May 2006

  Author:

    John Burkardt

  Input:

    double X, the number to be divided.

    double Y, the number that divides X.

  Output:

    double R8_MODP, the nonnegative remainder when X is divided by Y.
*/
{
  double value;

  if ( y == 0.0 )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "R8_MODP(): Fatal error!\n" );
    fprintf ( stderr, "  R8_MODP ( X, Y ) called with Y = %f\n", y );
    exit ( 1 );
  }

  value = x - ( ( double ) ( ( int ) ( x / y ) ) ) * y;

  if ( value < 0.0 )
  {
    value = value + fabs ( y );
  }

  return value;
}
/******************************************************************************/

double r8_sign ( double x )

/******************************************************************************/
/*
  Purpose:

    r8_sign() returns the sign of an R8.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    08 May 2006

  Author:

    John Burkardt

  Input:

    double X, the number whose sign is desired.

  Output:

    double R8_SIGN, the sign of X.
*/
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
