# include <cstdlib>
# include <cmath>
# include <ctime>
# include <iostream>
# include <iomanip>

using namespace std;

# include "circle_rule.hpp"

//****************************************************************************80

void circle_rule ( int nt, double w[], double t[] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE_RULE computes a quadrature rule for the unit circle.
//
//  Discussion:
//
//    The unit circle is the region:
//
//      x * x + y * y = 1.
//
//    The integral I(f) is then approximated by
//
//      Q(f) = 2 * pi * sum ( 1 <= i <= NT ) W(i) * F ( cos(T(i)), sin(T(i)) ).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    06 April 2014
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int NT, the number of angles to use.
//
//    Output, double W[NT], the weights for the rule.
//
//    Output, double T[NT], the angles for the rule.
//
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
//****************************************************************************80

double circle01_monomial_integral ( int e[2] )

//****************************************************************************80
//
//  Purpose:
//
//    CIRCLE01_MONOMIAL_INTEGRAL returns monomial integrals on the unit circle.
//
//  Discussion:
//
//    The integration region is 
//
//      X^2 + Y^2 = 1.
//
//    The monomial is F(X,Y) = X^E(1) * Y^E(2).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    11 January 2014
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Philip Davis, Philip Rabinowitz,
//    Methods of Numerical Integration,
//    Second Edition,
//    Academic Press, 1984, page 263.
//
//  Parameters:
//
//    Input, int E[2], the exponents of X and Y in the 
//    monomial.  Each exponent must be nonnegative.
//
//    Output, double CIRCLE01_MONOMIAL_INTEGRAL, the integral.
//
{
  double arg;
  int i;
  double integral;

  if ( e[0] < 0 || e[1] < 0 )
  {
    cout << "\n";
    cout << "CIRCLE01_MONOMIAL_INTEGRAL - Fatal error!\n";
    cout << "  All exponents must be nonnegative.\n";
    cout << "  E[0] = " << e[0] << "\n";
    cout << "  E[1] = " << e[1] << "\n";
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
      arg = 0.5 * ( double ) ( e[i] + 1 );
      integral = integral * tgamma ( arg );
    }

    arg = 0.5 * ( double ) ( e[0] + e[1] + 2 );
    integral = integral / tgamma ( arg );
  }
  return integral;
}
//****************************************************************************80

void timestamp ( )

//****************************************************************************80
//
//  Purpose:
//
//    TIMESTAMP prints the current YMDHMS date as a time stamp.
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
//  Parameters:
//
//    None
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
