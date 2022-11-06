# include <math.h>
# include <stdlib.h>
# include <stdio.h>
# include <time.h>

# include "circle_rule.h"

/******************************************************************************/

void circle_rule ( int nt, double w[], double t[] )

/******************************************************************************/
/*
  Purpose:

    CIRCLE_RULE computes a quadrature rule for the unit circle.

  Discussion:

    The unit circle is the region:

      x * x + y * y = 1.

    The integral I(f) is then approximated by

      Q(f) = 2 * pi * sum ( 1 <= i <= NT ) W(i) * F ( cos(T(i)), sin(T(i)) ).

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    06 April 2014

  Author:

    John Burkardt

  Parameters:

    Input, int NT, the number of angles to use.

    Output, double W[NT], the weights for the rule.

    Output, double T[NT], the angles for the rule.
*/
{
  int it;
  double r8_pi = 3.141592653589793;

  for ( it = 0; it < nt; it++ )
  {
    w[it] = 1.0 / ( double ) ( nt );
    t[it] = 2.0 * r8_pi * ( double ) ( it ) / ( double ) ( nt );
  }
  return;
}
/******************************************************************************/

double circle01_monomial_integral ( int e[2] )

/******************************************************************************/
/*
  Purpose:

    CIRCLE01_MONOMIAL_INTEGRAL: integrals on circumference of unit circle in 2D.

  Discussion:

    The integration region is 

      X^2 + Y^2 = 1.

    The monomial is F(X,Y,Z) = X^E(1) * Y^E(2).

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

