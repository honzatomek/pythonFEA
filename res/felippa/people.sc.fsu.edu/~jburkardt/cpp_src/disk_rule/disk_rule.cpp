# include <cfloat>
# include <cmath>
# include <cstdlib>
# include <cstring>
# include <ctime>
# include <iomanip>
# include <iostream>

using namespace std;

# include "disk_rule.hpp"

//****************************************************************************80

void disk_rule ( int nr, int nt, double xc, double yc, double rc, double w[], 
  double x[], double y[] )

//****************************************************************************80
//
//  Purpose:
//
//    DISK_RULE computes a quadrature rule for the general disk.
//
//  Discussion:
//
//    The general disk is the region:
//
//      ( x - xc ) ^ 2 + ( y - yc ) ^ 2 <= rc ^ 2.
//
//    The integral I(f) is then approximated by
//
//      S(f) = sum ( 1 <= i <= NT * NR ) W(i) * F ( X(i), Y(i) ).
//
//      Area = pi * RC ^ 2
//
//      Q(f) = Area * S(f)
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 March 2014
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int NR, the number of points in the radial rule.
//
//    Input, int NT, the number of angles to use.
//
//    Input, double XC, YC, the center of the disk.
//
//    Input, double RC, the radius of the disk.
//
//    Output, double W[NR*NT], the weights for the rule.
//
//    Output, double X[NR*NT], Y[NR*NT], the points for the rule.
//
{
  int i;
  int j;
  double *r01;
  double *t01;
  double *w01;

  w01 = new double[nr];
  r01 = new double[nr];
  t01 = new double[nt];

  disk01_rule ( nr, nt, w01, r01, t01 );
//
//  Recompute the rule for the general circle in terms of X, Y.
//
  for ( j = 0; j < nt; j++ )
  {
    for ( i = 0; i < nr; i++ )
    {
      w[i+j*nr] = w01[i];
      x[i+j*nr] = xc + rc * r01[i] * cos ( t01[j] );
      y[i+j*nr] = yc + rc * r01[i] * sin ( t01[j] );
    }
  }
//
//  Free memory.
//
  delete [] r01;
  delete [] t01;
  delete [] w01;

  return;
}
//****************************************************************************80

double disk01_monomial_integral ( int e[2] )

//****************************************************************************80
//
//  Purpose:
//
//    DISK01_MONOMIAL_INTEGRAL returns monomial integrals in the unit disk in 2D.
//
//  Discussion:
//
//    The integration region is 
//
//      X^2 + Y^2 <= 1.
//
//    The monomial is F(X,Y) = X^E(1) * Y^E(2).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    03 January 2014
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
//    Output, double DISK01_MONOMIAL_INTEGRAL, the integral.
//
{
  double arg;
  int i;
  double integral;
  const double r = 1.0;
  double s;

  if ( e[0] < 0 || e[1] < 0 )
  {
    cerr << "\n";
    cerr << "DISK01_MONOMIAL_INTEGRAL - Fatal error!\n";
    cerr << "  All exponents must be nonnegative.\n";
    cerr << "  E[0] = " << e[0] << "\n";
    cerr << "  E[1] = " << e[1] << "\n";
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
//
//  Adjust the surface integral to get the volume integral.
//
  s = e[0] + e[1] + 2;
  integral = integral * pow ( r, s ) / ( double ) ( s );

  return integral;
}
//****************************************************************************80

void disk01_rule ( int nr, int nt, double w[], double r[], double t[] )

//****************************************************************************80
//
//  Purpose:
//
//    DISK01_RULE computes a quadrature rule for the unit disk.
//
//  Discussion:
//
//    The unit disk is the region:
//
//      x * x + y * y <= 1.
//
//    The integral I(f) is then approximated by
//
//      Q(f) = pi * sum ( 1 <= j <= NT ) sum ( 1 <= i <= NR ) 
//        W(i) * F ( R(i) * cos(T(j)), R(i) * sin(T(j)) ).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    17 March 2014
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int NR, the number of points in the radial rule.
//
//    Input, int NT, the number of angles to use.
//
//    Output, double W[NR], the weights for the disk rule.
//
//    Output, double R[NR], T[NT], the (R,Theta) points for the rule.
//
{
  int ir;
  int it;
  const double r8_pi = 3.141592653589793;
  double *wr;
  double *xr;
//
//  Request a Legendre rule for [-1,+1].
//
  xr = new double[nr];
  wr = new double[nr];

  legendre_ek_compute ( nr, xr, wr );
//
//  Shift the rule to [0,1].
//
  for ( ir = 0; ir < nr; ir++ )
  {
    xr[ir] = ( xr[ir] + 1.0 ) / 2.0;
    wr[ir] = wr[ir] / 2.0;
  }
//
//  Compute the disk rule.
//
  for ( it = 0; it < nt; it++ )
  {
    t[it] = 2.0 * r8_pi * ( double ) ( it  ) / ( double ) ( nt );
  }

  for ( ir = 0; ir < nr; ir++ )
  {
    w[ir] = wr[ir] / ( double ) ( nt );
    r[ir] = sqrt ( xr[ir] );
  }

  delete [] wr;
  delete [] xr;

  return;
}
//****************************************************************************80

void imtqlx ( int n, double d[], double e[], double z[] )

//****************************************************************************80
//
//  Purpose:
//
//    IMTQLX diagonalizes a symmetric tridiagonal matrix.
//
//  Discussion:
//
//    This routine is a slightly modified version of the EISPACK routine to 
//    perform the implicit QL algorithm on a symmetric tridiagonal matrix. 
//
//    The authors thank the authors of EISPACK for permission to use this
//    routine. 
//
//    It has been modified to produce the product Q' * Z, where Z is an input 
//    vector and Q is the orthogonal matrix diagonalizing the input matrix.  
//    The changes consist (essentially) of applying the orthogonal transformations
//    directly to Z as they are generated.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license. 
//
//  Modified:
//
//    08 January 2010
//
//  Author:
//
//    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Sylvan Elhay, Jaroslav Kautsky,
//    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of 
//    Interpolatory Quadrature,
//    ACM Transactions on Mathematical Software,
//    Volume 13, Number 4, December 1987, pages 399-415.
//
//    Roger Martin, James Wilkinson,
//    The Implicit QL Algorithm,
//    Numerische Mathematik,
//    Volume 12, Number 5, December 1968, pages 377-383.
//
//  Parameters:
//
//    Input, int N, the order of the matrix.
//
//    Input/output, double D(N), the diagonal entries of the matrix.
//    On output, the information in D has been overwritten.
//
//    Input/output, double E(N), the subdiagonal entries of the 
//    matrix, in entries E(1) through E(N-1).  On output, the information in
//    E has been overwritten.
//
//    Input/output, double Z(N).  On input, a vector.  On output,
//    the value of Q' * Z, where Q is the matrix that diagonalizes the
//    input symmetric tridiagonal matrix.
//
{
  double b;
  double c;
  double f;
  double g;
  int i;
  int ii;
  int itn = 30;
  int j;
  int k;
  int l;
  int m;
  int mml;
  double p;
  double prec;
  double r;
  double s;

  prec = DBL_EPSILON;

  if ( n == 1 )
  {
    return;
  }

  e[n-1] = 0.0;

  for ( l = 1; l <= n; l++ )
  {
    j = 0;
    for ( ; ; )
    {
      for ( m = l; m <= n; m++ )
      {
        if ( m == n )
        {
          break;
        }

        if ( fabs ( e[m-1] ) <= prec * ( fabs ( d[m-1] ) + fabs ( d[m] ) ) )
        {
          break;
        }
      }
      p = d[l-1];
      if ( m == l )
      {
        break;
      }
      if ( itn <= j )
      {
        cerr << "\n";
        cerr << "IMTQLX - Fatal error!\n";
        cerr << "  Iteration limit exceeded\n";
        exit ( 1 );
      }
      j = j + 1;
      g = ( d[l] - p ) / ( 2.0 * e[l-1] );
      r = sqrt ( g * g + 1.0 );
      g = d[m-1] - p + e[l-1] / ( g + fabs ( r ) * r8_sign ( g ) );
      s = 1.0;
      c = 1.0;
      p = 0.0;
      mml = m - l;

      for ( ii = 1; ii <= mml; ii++ )
      {
        i = m - ii;
        f = s * e[i-1];
        b = c * e[i-1];

        if ( fabs ( g ) <= fabs ( f ) )
        {
          c = g / f;
          r = sqrt ( c * c + 1.0 );
          e[i] = f * r;
          s = 1.0 / r;
          c = c * s;
        }
        else
        {
          s = f / g;
          r = sqrt ( s * s + 1.0 );
          e[i] = g * r;
          c = 1.0 / r;
          s = s * c;
        }
        g = d[i] - p;
        r = ( d[i-1] - g ) * s + 2.0 * c * b;
        p = s * r;
        d[i] = g + p;
        g = c * r - b;
        f = z[i];
        z[i] = s * z[i-1] + c * f;
        z[i-1] = c * z[i-1] - s * f;
      }
      d[l-1] = d[l-1] - p;
      e[l-1] = g;
      e[m-1] = 0.0;
    }
  }
//
//  Sorting.
//
  for ( ii = 2; ii <= m; ii++ )
  {
    i = ii - 1;
    k = i;
    p = d[i-1];

    for ( j = ii; j <= n; j++ )
    {
      if ( d[j-1] < p )
      {
         k = j;
         p = d[j-1];
      }
    }

    if ( k != i )
    {
      d[k-1] = d[i-1];
      d[i-1] = p;
      p = z[i-1];
      z[i-1] = z[k-1];
      z[k-1] = p;
    }
  }
  return;
}
//****************************************************************************80

void legendre_ek_compute ( int n, double x[], double w[] )

//****************************************************************************80
//
//  Purpose:
//
//    LEGENDRE_EK_COMPUTE: Legendre quadrature rule by the Elhay-Kautsky method.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    19 April 2011
//
//  Author:
//
//    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Sylvan Elhay, Jaroslav Kautsky,
//    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
//    Interpolatory Quadrature,
//    ACM Transactions on Mathematical Software,
//    Volume 13, Number 4, December 1987, pages 399-415.
//
//  Parameters:
//
//    Input, int N, the order.
//
//    Output, double X[N], the abscissas.
//
//    Output, double W[N], the weights.
//
{
  double *bj;
  int i;
  double zemu;
//
//  Define the zero-th moment.
//
  zemu = 2.0;
//
//  Define the Jacobi matrix.
//
  bj = new double[n];

  for ( i = 0; i < n; i++ )
  {
    bj[i] = ( double ) ( ( i + 1 ) * ( i + 1 ) ) 
          / ( double ) ( 4 * ( i + 1 ) * ( i + 1 ) - 1 );
    bj[i] = sqrt ( bj[i] );
  }

  for ( i = 0; i < n; i++ )
  {
    x[i] = 0.0;
  }

  w[0] = sqrt ( zemu );

  for ( i = 1; i < n; i++ )
  {
    w[i] = 0.0;
  }
//
//  Diagonalize the Jacobi matrix.
//
  imtqlx ( n, x, bj, w );

  for ( i = 0; i < n; i++ )
  {
    w[i] = w[i] * w[i];
  }

  delete [] bj;

  return;
}
//****************************************************************************80

double r8_sign ( double x )

//****************************************************************************80
//
//  Purpose:
//
//    R8_SIGN returns the sign of an R8.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    18 October 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X, the number whose sign is desired.
//
//    Output, double R8_SIGN, the sign of X.
//
{
  double value;

  if ( x < 0.0 )
  {
    value = -1.0;
  }
  else
  {
    value = 1.0;
  }
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
//  Parameters:
//
//    Input, int N, the number of components of the vector.
//
//    Input, double A[N], the vector to be printed.
//
//    Input, string TITLE, a title.
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
