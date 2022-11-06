# include <math.h>
# include <stdio.h>
# include <stdlib.h>
# include <time.h>

# include "circle_monte_carlo.h"

/******************************************************************************/

double circle01_length ( )

/******************************************************************************/
/*
  Purpose:

    CIRCLE01_LENGTH: length of the circumference of the unit circle in 2D.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    11 January 2014

  Author:

    John Burkardt

  Parameters:

    Output, double CIRCLE01_LENGTH, the length.
*/
{
  double length;
  const double r = 1.0;
  const double r8_pi = 3.141592653589793;

  length = 2.0 * r8_pi * r;

  return length;
}
/******************************************************************************/

double circle01_monomial_integral ( int e[2] )

/******************************************************************************/
/*
  Purpose:

    CIRCLE01_MONOMIAL_INTEGRAL: integrals on the circumference of the unit circle in 2D.

  Discussion:

    The integration region is 

      X^2 + Y^2 = 1.

    The monomial is F(X,Y) = X^E(1) * Y^E(2).

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    11 January 2014

  Author:

    John Burkardt

  Reference:

    Philip Davis, Philip Rabinowitz,
    Methods of Numerical Integration,
    Second Edition,
    Academic Press, 1984, page 263.

  Parameters:

    Input, int E[2], the exponents of X and Y in the 
    monomial.  Each exponent must be nonnegative.

    Output, double CIRCLE01_MONOMIAL_INTEGRAL, the integral.
*/
{
  int i;
  double integral;

  if ( e[0] < 0 || e[1] < 0 )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "CIRCLE01_MONOMIAL_INTEGRAL - Fatal error!\n" );
    fprintf ( stderr, "  All exponents must be nonnegative.\n" );
    fprintf ( stderr, "  E[0] = %d\n", e[0] );
    fprintf ( stderr, "  E[1] = %d\n", e[1] );
    exit ( 1 );
  }

  if ( ( e[0] % 2 ) == 1 || ( e[1] % 2 ) == 1 )
  {
    integral = 0.0;
  }
  else
  {
    integral = 2.0;

    for ( i = 0; i < 2; i++ )
    {
      integral = integral * tgamma ( 0.5 * ( double ) ( e[i] + 1 ) );
    }

    integral = integral / tgamma ( 0.5 * ( double ) ( e[0] + e[1] + 2 ) );

  }
  return integral;
}
/******************************************************************************/

double *circle01_sample_ergodic ( int n, double *angle )

/******************************************************************************/
/*
  Purpose:

    CIRCLE01_SAMPLE_ERGODIC samples points on the unit circle in 2D.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    06 June 2017

  Author:

    John Burkardt

  Parameters:

    Input, int N, the number of points.

    Input/output, double *ANGLE, the base angle, which could be anything
    in the range [0,2 PI).

    Output, double X[2*N], the points.
*/
{
  const double c[2] = { 0.0, 0.0 };
  double golden_angle;
  double golden_ratio;
  int j;
  const double r = 1.0;
  const double r8_pi = 3.141592653589793;
  double *x;

  golden_ratio = ( 1.0 + sqrt ( 5.0 ) ) / 2.0;

  golden_angle = 2.0 * r8_pi / pow ( golden_ratio, 2 );

  x = ( double * ) malloc ( 2 * n * sizeof ( double ) );

  for ( j = 0; j < n; j++ )
  {
    x[0+j*2] = c[0] + r * cos ( *angle );
    x[1+j*2] = c[1] + r * sin ( *angle );
    *angle = fmod ( *angle + golden_angle, 2.0 * r8_pi ) ;
  }

  return x;
}
/******************************************************************************/

double *circle01_sample_random ( int n, int *seed )

/******************************************************************************/
/*
  Purpose:

    CIRCLE01_SAMPLE_RANDOM samples points on the unit circle in 2D.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    06 June 2017

  Author:

    John Burkardt

  Reference:

    Russell Cheng,
    Random Variate Generation,
    in Handbook of Simulation,
    edited by Jerry Banks,
    Wiley, 1998, pages 168.

    Reuven Rubinstein,
    Monte Carlo Optimization, Simulation, and Sensitivity 
    of Queueing Networks,
    Krieger, 1992,
    ISBN: 0894647644,
    LC: QA298.R79.

  Parameters:

    Input, int N, the number of points.

    Input/output, int *SEED, a seed for the random 
    number generator.

    Output, double X[2*N], the points.
*/
{
  const double c[2] = { 0.0, 0.0 };
  int j;
  const double r = 1.0;
  const double r8_pi = 3.141592653589793;
  double *theta;
  double *x;

  theta = r8vec_uniform_01_new ( n, seed );

  x = ( double * ) malloc ( 2 * n * sizeof ( double ) );

  for ( j = 0; j < n; j++ )
  {
    x[0+j*2] = c[0] + r * cos ( 2.0 * r8_pi * theta[j] );
    x[1+j*2] = c[1] + r * sin ( 2.0 * r8_pi * theta[j] );
  }

  free ( theta );

  return x;
}
/******************************************************************************/

double *monomial_value ( int m, int n, int e[], double x[] )

/******************************************************************************/
/*
  Purpose:

    MONOMIAL_VALUE evaluates a monomial.

  Discussion:

    This routine evaluates a monomial of the form

      product ( 1 <= i <= m ) x(i)^e(i)

    where the exponents are nonnegative integers.  Note that
    if the combination 0^0 is encountered, it should be treated
    as 1.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    08 May 2014

  Author:

    John Burkardt

  Parameters:

    Input, int M, the spatial dimension.

    Input, int N, the number of points at which the
    monomial is to be evaluated.

    Input, int E[M], the exponents.

    Input, double X[M*N], the point coordinates.

    Output, double MONOMIAL_VALUE[N], the value of the monomial.
*/
{
  int i;
  int j;
  double *v;

  v = ( double * ) malloc ( n * sizeof ( double ) );

  for ( j = 0; j < n; j++ )
  {
    v[j] = 1.0;
  }

  for ( i = 0; i < m; i++ )
  {
    if ( 0 != e[i] )
    {
      for ( j = 0; j < n; j++ )
      {
        v[j] = v[j] * pow ( x[i+j*m], e[i] );
      }
    }
  }

  return v;
}
/******************************************************************************/

double r8vec_sum ( int n, double a[] )

/******************************************************************************/
/*
  Purpose:

    R8VEC_SUM returns the sum of an R8VEC.

  Discussion:

    An R8VEC is a vector of R8's.

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    26 August 2008

  Author:

    John Burkardt

  Parameters:

    Input, int N, the number of entries in the vector.

    Input, double A[N], the vector.

    Output, double R8VEC_SUM, the sum of the vector.
*/
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
/******************************************************************************/

double *r8vec_uniform_01_new ( int n, int *seed )

/******************************************************************************/
/*
  Purpose:

    R8VEC_UNIFORM_01_NEW returns a unit pseudorandom R8VEC.

  Discussion:

    This routine implements the recursion

      seed = 16807 * seed mod ( 2^31 - 1 )
      unif = seed / ( 2^31 - 1 )

    The integer arithmetic never requires more than 32 bits,
    including a sign bit.

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    19 August 2004

  Author:

    John Burkardt

  Reference:

    Paul Bratley, Bennett Fox, Linus Schrage,
    A Guide to Simulation,
    Second Edition,
    Springer, 1987,
    ISBN: 0387964673,
    LC: QA76.9.C65.B73.

    Bennett Fox,
    Algorithm 647:
    Implementation and Relative Efficiency of Quasirandom
    Sequence Generators,
    ACM Transactions on Mathematical Software,
    Volume 12, Number 4, December 1986, pages 362-376.

    Pierre L'Ecuyer,
    Random Number Generation,
    in Handbook of Simulation,
    edited by Jerry Banks,
    Wiley, 1998,
    ISBN: 0471134031,
    LC: T57.62.H37.

    Peter Lewis, Allen Goodman, James Miller,
    A Pseudo-Random Number Generator for the System/360,
    IBM Systems Journal,
    Volume 8, Number 2, 1969, pages 136-143.

  Parameters:

    Input, int N, the number of entries in the vector.

    Input/output, int *SEED, a seed for the random number generator.

    Output, double R8VEC_UNIFORM_01_NEW[N], the vector of pseudorandom values.
*/
{
  int i;
  int i4_huge = 2147483647;
  int k;
  double *r;

  if ( *seed == 0 )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "R8VEC_UNIFORM_01_NEW - Fatal error!\n" );
    fprintf ( stderr, "  Input value of SEED = 0.\n" );
    exit ( 1 );
  }

  r = ( double * ) malloc ( n * sizeof ( double ) );

  for ( i = 0; i < n; i++ )
  {
    k = *seed / 127773;

    *seed = 16807 * ( *seed - k * 127773 ) - k * 2836;

    if ( *seed < 0 )
    {
      *seed = *seed + i4_huge;
    }

    r[i] = ( double ) ( *seed ) * 4.656612875E-10;
  }

  return r;
}
/******************************************************************************/

void timestamp ( )

/******************************************************************************/
/*
  Purpose:

    TIMESTAMP prints the current YMDHMS date as a time stamp.

  Example:

    31 May 2001 09:45:54 AM

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    24 September 2003

  Author:

    John Burkardt

  Parameters:

    None
*/
{
# define TIME_SIZE 40

  static char time_buffer[TIME_SIZE];
  const struct tm *tm;
  time_t now;

  now = time ( NULL );
  tm = localtime ( &now );

  strftime ( time_buffer, TIME_SIZE, "%d %B %Y %I:%M:%S %p", tm );

  fprintf ( stdout, "%s\n", time_buffer );

  return;
# undef TIME_SIZE
}

