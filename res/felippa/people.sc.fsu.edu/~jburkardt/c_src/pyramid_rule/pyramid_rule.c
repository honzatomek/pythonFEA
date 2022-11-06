# include <float.h>
# include <math.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <time.h>

int main ( int argc, char *argv[] );
void jacobi_compute ( int order, double alpha, double beta, double x[],
  double w[] );
void jacobi_recur ( double *p2, double *dp2, double *p1, double x, int order,
  double alpha, double beta, double b[], double c[] );
void jacobi_root ( double *x, int order, double alpha, double beta,
  double *dp2, double *p1, double b[], double c[] );
void legendre_compute ( int order, double x[], double w[] );
void pyramid_handle ( int legendre_order, int jacobi_order, char *filename );
void r8mat_write ( char *output_filename, int m, int n, double table[] );
void r8vec_reverse ( int n, double a[] );
void timestamp ( );

/******************************************************************************/

int main ( int argc, char *argv[] )

/******************************************************************************/
/*
  Purpose:

    MAIN is the main program for PYRAMID_RULE.

  Discussion:

    This program computes a quadrature rule for a pyramid
    and writes it to a file.

    The user specifies:
    * the LEGENDRE_ORDER (number of points in the X and Y dimensions)
    * the JACOBI_ORDER (number of points in the Z dimension)
    * FILENAME, the root name of the output files.

    The integration region is:

      - ( 1 - Z ) <= X <= 1 - Z
      - ( 1 - Z ) <= Y <= 1 - Z
                0 <= Z <= 1.

    When Z is zero, the integration region is a square lying in the (X,Y)
    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
    radius of the square diminishes, and when Z reaches 1, the square has
    contracted to the single point (0,0,1).

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    23 July 2009

  Author:

    John Burkardt
*/
{
  char filename[255];
  int jacobi_order;
  int legendre_order;

  timestamp ( );
  printf ( "\n" );
  printf ( "PYRAMID_RULE\n" );
  printf ( "  C version\n" );
  printf ( "\n" );
  printf ( "  Compute a quadrature rule for approximating\n" );
  printf ( "  the integral of a function over a pyramid.\n" );
  printf ( "\n" );
  printf ( "  The user specifies:\n" );
  printf ( "\n" );
  printf ( "  LEGENDRE_ORDER, the order of the Legendre rule for X and Y.\n" );
  printf ( "  JACOBI_ORDER, the order of the Jacobi rule for Z,\n" );
  printf ( "  FILENAME, the prefix of the three output files:\n" );
  printf ( "\n" );
  printf ( "    filename_w.txt - the weight file\n" );
  printf ( "    filename_x.txt - the abscissa file.\n" );
  printf ( "    filename_r.txt - the region file.\n" );
/*
  Get the Legendre order.
*/
  if ( 1 < argc )
  {
    legendre_order = atoi ( argv[1] );
  }
  else
  {
    printf ( "\n" );
    printf ( "  Enter the Legendre rule order:\n" );
    scanf ( "%d", &legendre_order );
  }

  printf ( "\n" );
  printf ( "  The requested Legendre order of the rule is %d\n", legendre_order );
/*
  Get the Jacobi order.
*/
  if ( 2 < argc )
  {
    jacobi_order = atoi ( argv[2] );
  }
  else
  {
    printf ( "\n" );
    printf ( "  Enter the Jacobi rule order:\n" );
    scanf ( "%d", &jacobi_order );
  }

  printf ( "\n" );
  printf ( "  The requested Jacobi order of the rule is %d\n", jacobi_order );
/*
  Get the output option or quadrature file root name:
*/
  if ( 3 < argc )
  {
    strcpy ( filename, argv[3] );
  }
  else
  {
    printf ( "\n" );
    printf ( "  Enter FILENAME, the \"root name\" of the quadrature files).\n" );
    scanf ( "%s", filename );
  }

  pyramid_handle ( legendre_order, jacobi_order, filename );

  printf ( "\n" );
  printf ( "PYRAMID_RULE:\n" );
  printf ( "  Normal end of execution.\n" );

  printf ( "\n" );
  timestamp ( );

  return 0;
}
/******************************************************************************/

void jacobi_compute ( int order, double alpha, double beta, double xtab[],
  double weight[] )

/******************************************************************************/
/*
  Purpose:
  
    JACOBI_COMPUTE computes a Gauss-Jacobi quadrature rule.
  
  Discussion:
    
    The integral:
  
      Integral ( -1 <= X <= 1 ) (1-X)^ALPHA * (1+X)^BETA * F(X) dX
  
    The quadrature rule:
  
      Sum ( 1 <= I <= ORDER ) WEIGHT(I) * F ( XTAB(I) )
  
    Thanks to Xu Xiang of Fudan University for pointing out that
    an earlier implementation of this routine was incorrect!
  
  Licensing:
  
    This code is distributed under the GNU LGPL license.
  
  Modified:
  
    14 May 2007
  
  Author:
  
    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
    C version by John Burkardt.
  
  Reference:
  
    Arthur Stroud, Don Secrest,
    Gaussian Quadrature Formulas,
    Prentice Hall, 1966,
    LC: QA299.4G3S7.
  
  Parameters:
  
    Input, int ORDER, the order.
  
    Input, double ALPHA, BETA, the exponents of (1-X) and
    (1+X) in the quadrature rule.  For simple Gauss-Legendre quadrature,
    set ALPHA = BETA = 0.0.  -1.0 < ALPHA and -1.0 < BETA are required.
  
    Output, double XTAB[ORDER], the abscissas.
  
    Output, double WEIGHT[ORDER], the weights.
*/
{
  double an;
  double *b;
  double bn;
  double *c;
  double cc;
  double delta;
  double dp2;
  int i;
  double p1;
  double prod;
  double r1;
  double r2;
  double r3;
  double x;
/*
  Check ALPHA and BETA.
*/
  if ( alpha <= -1.0 )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "JACOBI_COMPUTE - Fatal error!\n" );
    fprintf ( stderr, "  -1.0 < ALPHA is required.\n" );
    exit ( 1 );
  }

  if ( beta <= -1.0 )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "JACOBI_COMPUTE - Fatal error!\n" );
    fprintf ( stderr, "  -1.0 < BETA is required.\n" );
    exit ( 1 );
  }

  b = ( double * ) malloc ( order * sizeof ( double ) );
  c = ( double * ) malloc ( order * sizeof ( double ) );
/*
  Set the recursion coefficients.
*/
  for ( i = 1; i <= order; i++ )
  {
    if ( alpha + beta == 0.0 || beta - alpha == 0.0 )
    {
      b[i-1] = 0.0;
    }
    else
    {
      b[i-1] = ( alpha + beta ) * ( beta - alpha ) /
             ( ( alpha + beta + ( double ) ( 2 * i ) )
             * ( alpha + beta + ( double ) ( 2 * i - 2 ) ) );
    }

    if ( i == 1 )
    {
      c[i-1] = 0.0;
    }
    else
    {
      c[i-1] = 4.0 * ( double ) ( i - 1 )
         * ( alpha + ( double ) ( i - 1 ) )
          * ( beta + ( double ) ( i - 1 ) )
            * ( alpha + beta + ( double ) ( i - 1 ) ) /
            ( ( alpha + beta + ( double ) ( 2 * i - 1 ) )
            * pow ( alpha + beta + ( double ) ( 2 * i - 2 ), 2 )
            * ( alpha + beta + ( double ) ( 2 * i - 3 ) ) );
    }
  }

  delta = tgamma ( alpha        + 1.0 )
        * tgamma (         beta + 1.0 )
        / tgamma ( alpha + beta + 2.0 );

  prod = 1.0;
  for ( i = 2; i <= order; i++ )
  {
    prod = prod * c[i-1];
  }
  cc = delta * pow ( 2.0, alpha + beta + 1.0 ) * prod;

  for ( i = 1; i <= order; i++ )
  {
    if ( i == 1 )
    {
      an = alpha / ( double ) ( order );
      bn = beta / ( double ) ( order );

      r1 = ( 1.0 + alpha )
        * ( 2.78 / ( 4.0 + ( double ) ( order * order ) )
        + 0.768 * an / ( double ) ( order ) );

      r2 = 1.0 + 1.48 * an + 0.96 * bn
        + 0.452 * an * an + 0.83 * an * bn;

      x = ( r2 - r1 ) / r2;
    }
    else if ( i == 2 )
    {
      r1 = ( 4.1 + alpha ) /
        ( ( 1.0 + alpha ) * ( 1.0 + 0.156 * alpha ) );

      r2 = 1.0 + 0.06 * ( ( double ) ( order ) - 8.0 ) *
        ( 1.0 + 0.12 * alpha ) / ( double ) ( order );

      r3 = 1.0 + 0.012 * beta *
        ( 1.0 + 0.25 * fabs ( alpha ) ) / ( double ) ( order );

      x = x - r1 * r2 * r3 * ( 1.0 - x );
    }
    else if ( i == 3 )
    {
      r1 = ( 1.67 + 0.28 * alpha ) / ( 1.0 + 0.37 * alpha );

      r2 = 1.0 + 0.22 * ( ( double ) ( order ) - 8.0 )
        / ( double ) ( order );

      r3 = 1.0 + 8.0 * beta /
        ( ( 6.28 + beta ) * ( double ) ( order * order ) );

      x = x - r1 * r2 * r3 * ( xtab[0] - x );
    }
    else if ( i < order - 1 )
    {
      x = 3.0 * xtab[i-2] - 3.0 * xtab[i-3] + xtab[i-4];
    }
    else if ( i == order - 1 )
    {
      r1 = ( 1.0 + 0.235 * beta ) / ( 0.766 + 0.119 * beta );

      r2 = 1.0 / ( 1.0 + 0.639
        * ( ( double ) ( order ) - 4.0 )
        / ( 1.0 + 0.71 * ( ( double ) ( order ) - 4.0 ) ) );

      r3 = 1.0 / ( 1.0 + 20.0 * alpha / ( ( 7.5 + alpha ) *
        ( double ) ( order * order ) ) );

      x = x + r1 * r2 * r3 * ( x - xtab[i-3] );
    }
    else if ( i == order )
    {
      r1 = ( 1.0 + 0.37 * beta ) / ( 1.67 + 0.28 * beta );

      r2 = 1.0 /
        ( 1.0 + 0.22 * ( ( double ) ( order ) - 8.0 )
        / ( double ) ( order ) );

      r3 = 1.0 / ( 1.0 + 8.0 * alpha /
        ( ( 6.28 + alpha ) * ( double ) ( order * order ) ) );

      x = x + r1 * r2 * r3 * ( x - xtab[i-3] );
    }
    jacobi_root ( &x, order, alpha, beta, &dp2, &p1, b, c );

    xtab[i-1] = x;
    weight[i-1] = cc / ( dp2 * p1 );
  }
/*
  Reverse the order of the values.
*/
  r8vec_reverse ( order, xtab );
  r8vec_reverse ( order, weight );

  free ( b );
  free ( c );

  return;
}
/******************************************************************************/

void jacobi_recur ( double *p2, double *dp2, double *p1, double x, int order,
  double alpha, double beta, double b[], double c[] )

/******************************************************************************/
/*
  Purpose:
  
    JACOBI_RECUR finds the value and derivative of a Jacobi polynomial.
  
  Licensing:
  
    This code is distributed under the GNU LGPL license.
  
  Modified:
  
    04 May 2006
  
  Author:
  
    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
    C version by John Burkardt.
  
  Reference:
  
    Arthur Stroud, Don Secrest,
    Gaussian Quadrature Formulas,
    Prentice Hall, 1966,
    LC: QA299.4G3S7.
  
  Parameters:
  
    Output, double *P2, the value of J(ORDER)(X).
  
    Output, double *DP2, the value of J'(ORDER)(X).
  
    Output, double *P1, the value of J(ORDER-1)(X).
  
    Input, double X, the point at which polynomials are evaluated.
  
    Input, int ORDER, the order of the polynomial.
  
    Input, double ALPHA, BETA, the exponents of (1-X) and
    (1+X) in the quadrature rule.
  
    Input, double B[ORDER], C[ORDER], the recursion coefficients.
*/
{
  double dp0;
  double dp1;
  int i;
  double p0;

  *p1 = 1.0;
  dp1 = 0.0;

  *p2 = x + ( alpha - beta ) / ( alpha + beta + 2.0 );
  *dp2 = 1.0;

  for ( i = 2; i <= order; i++ )
  {
    p0 = *p1;
    dp0 = dp1;

    *p1 = *p2;
    dp1 = *dp2;

    *p2 = ( x - b[i-1] ) *  ( *p1 ) - c[i-1] * p0;
    *dp2 = ( x - b[i-1] ) * dp1 + ( *p1 ) - c[i-1] * dp0;
  }
  return;
}
/******************************************************************************/

void jacobi_root ( double *x, int order, double alpha, double beta,
  double *dp2, double *p1, double b[], double c[] )

/******************************************************************************/
/*
  Purpose:
  
    JACOBI_ROOT improves an approximate root of a Jacobi polynomial.
  
  Licensing:
  
    This code is distributed under the GNU LGPL license.
  
  Modified:
  
    04 May 2006
  
  Author:
  
    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
    C version by John Burkardt.
  
  Reference:
  
    Arthur Stroud, Don Secrest,
    Gaussian Quadrature Formulas,
    Prentice Hall, 1966,
    LC: QA299.4G3S7.
  
  Parameters:
  
    Input/output, double *X, the approximate root, which
    should be improved on output.
  
    Input, int ORDER, the order of the polynomial.
  
    Input, double ALPHA, BETA, the exponents of (1-X) and
    (1+X) in the quadrature rule.
  
    Output, double *DP2, the value of J'(ORDER)(X).
  
    Output, double *P1, the value of J(ORDER-1)(X).
  
    Input, double B[ORDER], C[ORDER], the recursion coefficients.
*/
{
  double d;
  double eps;
  double p2;
  int step;
  int step_max = 10;

  eps = DBL_EPSILON;

  for ( step = 1; step <= step_max; step++ )
  {
    jacobi_recur ( &p2, dp2, p1, *x, order, alpha, beta, b, c );

    d = p2 / ( *dp2 );
    *x = *x - d;

    if ( fabs ( d ) <= eps * ( fabs ( *x ) + 1.0 ) )
    {
      return;
    }
  }
  return;
}
/******************************************************************************/

void legendre_compute ( int order, double xtab[], double weight[] )

/******************************************************************************/
/*
  Purpose:
  
    LEGENDRE_COMPUTE: Gauss-Legendre quadrature by Davis-Rabinowitz method.
  
  Discussion:
    
    The integral:
  
      Integral ( -1 <= X <= 1 ) F(X) dX
  
    The quadrature rule:
  
      Sum ( 1 <= I <= ORDER ) WEIGHT(I) * F ( XTAB(I) )
  
  Licensing:
  
    This code is distributed under the GNU LGPL license.
  
  Modified:
  
    28 August 2007
  
  Author:
  
    Original FORTRAN77 version by Philip Davis, Philip Rabinowitz.
    C version by John Burkardt.
  
  Reference:
  
    Philip Davis, Philip Rabinowitz,
    Methods of Numerical Integration,
    Second Edition,
    Dover, 2007,
    ISBN: 0486453391,
    LC: QA299.3.D28.
  
  Parameters:
  
    Input, int ORDER, the order.
    ORDER must be greater than 0.
  
    Output, double XTAB[ORDER], the abscissas.
  
    Output, double WEIGHT[ORDER], the weights.
*/
{
  double d1;
  double d2pn;
  double d3pn;
  double d4pn;
  double dp;
  double dpn;
  double e1;
  double fx;
  double h;
  int i;
  int iback;
  int k;
  int m;
  int mp1mi;
  int ncopy;
  int nmove;
  double p;
  const double r8_pi = 3.141592653589793;
  double pk;
  double pkm1;
  double pkp1;
  double t;
  double u;
  double v;
  double x0;
  double xtemp;

  if ( order < 1 )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "LEGENDRE_DR_COMPUTE - Fatal error!\n" );
    fprintf ( stderr, "  Illegal value of ORDER = %d\n", order );
    exit ( 1 );
  }

  e1 = ( double ) ( order * ( order + 1 ) );

  m = ( order + 1 ) / 2;

  for ( i = 1; i <= m; i++ )
  {
    mp1mi = m + 1 - i;

    t = ( double ) ( 4 * i - 1 ) * r8_pi / ( double ) ( 4 * order + 2 );

    x0 = cos ( t ) * ( 1.0 - ( 1.0 - 1.0 / ( double ) ( order ) )
      / ( double ) ( 8 * order * order ) );

    pkm1 = 1.0;
    pk = x0;

    for ( k = 2; k <= order; k++ )
    {
      pkp1 = 2.0 * x0 * pk - pkm1 - ( x0 * pk - pkm1 ) / ( double ) ( k );
      pkm1 = pk;
      pk = pkp1;
    }

    d1 = ( double ) ( order ) * ( pkm1 - x0 * pk );

    dpn = d1 / ( 1.0 - x0 * x0 );

    d2pn = ( 2.0 * x0 * dpn - e1 * pk ) / ( 1.0 - x0 * x0 );

    d3pn = ( 4.0 * x0 * d2pn + ( 2.0 - e1 ) * dpn ) / ( 1.0 - x0 * x0 );

    d4pn = ( 6.0 * x0 * d3pn + ( 6.0 - e1 ) * d2pn ) / ( 1.0 - x0 * x0 );

    u = pk / dpn;
    v = d2pn / dpn;
/*
  Initial approximation H:
*/
    h = -u * ( 1.0 + 0.5 * u * ( v + u * ( v * v - d3pn / ( 3.0 * dpn ) ) ) );
/*
  Refine H using one step of Newton's method:
*/
    p = pk + h * ( dpn + 0.5 * h * ( d2pn + h / 3.0
      * ( d3pn + 0.25 * h * d4pn ) ) );

    dp = dpn + h * ( d2pn + 0.5 * h * ( d3pn + h * d4pn / 3.0 ) );

    h = h - p / dp;

    xtemp = x0 + h;

    xtab[mp1mi-1] = xtemp;

    fx = d1 - h * e1 * ( pk + 0.5 * h * ( dpn + h / 3.0
      * ( d2pn + 0.25 * h * ( d3pn + 0.2 * h * d4pn ) ) ) );

    weight[mp1mi-1] = 2.0 * ( 1.0 - xtemp * xtemp ) / ( fx * fx );
  }

  if ( ( order % 2 ) == 1 )
  {
    xtab[0] = 0.0;
  }
/*
  Shift the data up.
*/
  nmove = ( order + 1 ) / 2;
  ncopy = order - nmove;

  for ( i = 1; i <= nmove; i++ )
  {
    iback = order + 1 - i;
    xtab[iback-1] = xtab[iback-ncopy-1];
    weight[iback-1] = weight[iback-ncopy-1];
  }
/*
  Reflect values for the negative abscissas.
*/
  for ( i = 1; i <= order - nmove; i++ )
  {
    xtab[i-1] = - xtab[order-i];
    weight[i-1] = weight[order-i];
  }

  return;
}
/******************************************************************************/

void pyramid_handle ( int legendre_order, int jacobi_order, char *filename )

/******************************************************************************/
/*
  Purpose:

    PYRAMID_HANDLE computes the requested pyramid rule and outputs it.

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    24 July 2009

  Author:

    John Burkardt

  Parameters:

    Input, int LEGENDRE_ORDER, JACOBI_ORDER, the orders
    of the component Legendre and Jacobi rules.

    Input, char *FILENAME, the rootname for the files,
    write files 'file_w.txt' and 'file_x.txt', and 'file_r.txt', weights,
    abscissas, and region.
*/
{
# define DIM_NUM 3

  char filename_r[255];
  char filename_w[255];
  char filename_x[255];
  int i;
  int j;
  double jacobi_alpha;
  double jacobi_beta;
  double *jacobi_w;
  double *jacobi_x;
  int k;
  int l;
  double *legendre_w;
  double *legendre_x;
  int pyramid_order;
  double pyramid_r[DIM_NUM*5] = {
    -1.0, -1.0, 0.0,
    +1.0, -1.0, 0.0,
    -1.0, +1.0, 0.0,
    +1.0, +1.0, 0.0,
     0.0,  0.0, 1.0 };
  double *pyramid_w;
  double *pyramid_x;
  double volume;
  double wi;
  double wj;
  double wk;
  double xi;
  double xj;
  double xk;
/*
  Compute the factor rules.
*/
  legendre_w = ( double * ) malloc ( legendre_order * sizeof ( double ) );
  legendre_x = ( double * ) malloc ( legendre_order * sizeof ( double ) );

  legendre_compute ( legendre_order, legendre_x, legendre_w );

  jacobi_w = ( double * ) malloc ( jacobi_order * sizeof ( double ) );
  jacobi_x = ( double * ) malloc ( jacobi_order * sizeof ( double ) );

  jacobi_alpha = 2.0;
  jacobi_beta = 0.0;

  jacobi_compute ( jacobi_order, jacobi_alpha, jacobi_beta, jacobi_x, jacobi_w );
/*
  Compute the pyramid rule.
*/
  pyramid_order = legendre_order * legendre_order * jacobi_order;

  pyramid_w = ( double * ) malloc ( pyramid_order * sizeof ( double ) );
  pyramid_x = ( double * ) malloc ( DIM_NUM * pyramid_order * sizeof ( double ) );

  volume = 4.0 / 3.0;

  l = 0;
  for ( k = 0; k < jacobi_order; k++ )
  {
    xk = ( jacobi_x[k] + 1.0 ) / 2.0;
    wk = jacobi_w[k] / 2.0;
    for ( j = 0; j < legendre_order; j++ )
    {
      xj = legendre_x[j];
      wj = legendre_w[j];
      for ( i = 0; i < legendre_order; i++ )
      {
        xi = legendre_x[i];
        wi = legendre_w[i];
        pyramid_w[l] = wi * wj * wk / 4.0 / volume;
        pyramid_x[0+l*3] = xi * ( 1.0 - xk );
        pyramid_x[1+l*3] = xj * ( 1.0 - xk );
        pyramid_x[2+l*3] =              xk;
        l = l + 1;
      }
    }
  }

  free ( jacobi_w );
  free ( jacobi_x );
  free ( legendre_w );
  free ( legendre_x );
/*
  Write the rule to files.
*/
  strcpy ( filename_w, filename );
  strcat ( filename_w, "_w.txt" );

  strcpy ( filename_x, filename );
  strcat ( filename_x, "_x.txt" );

  strcpy ( filename_r, filename );
  strcat ( filename_r, "_r.txt" );

  printf ( "\n" );
  printf ( "  Creating quadrature files.\n" );
  printf ( "\n" );
  printf ( "  Root file name is     '%s'\n", filename );
  printf ( "\n" );
  printf ( "  Weight file will be   '%s'\n", filename_w );
  printf ( "  Abscissa file will be '%s'\n", filename_x );
  printf ( "  Region file will be   '%s'\n", filename_r );

  r8mat_write ( filename_w, 1,       pyramid_order, pyramid_w );
  r8mat_write ( filename_x, DIM_NUM, pyramid_order, pyramid_x );
  r8mat_write ( filename_r, DIM_NUM, 5,             pyramid_r );

  free ( pyramid_w );
  free ( pyramid_x );

  return;

# undef DIM_NUM
}
/******************************************************************************/

void r8mat_write ( char *output_filename, int m, int n, double table[] )

/******************************************************************************/
/*
  Purpose:

    R8MAT_WRITE writes an R8MAT file.

  Discussion:

    An R8MAT is an array of R8's.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    01 June 2009

  Author:

    John Burkardt

  Parameters:

    Input, char *OUTPUT_FILENAME, the output filename.

    Input, int M, the spatial dimension.

    Input, int N, the number of points.

    Input, double TABLE[M*N], the data.
*/
{
  int i;
  int j;
  FILE *output;
/*
  Open the file.
*/
  output = fopen ( output_filename, "wt" );

  if ( !output )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "R8MAT_WRITE - Fatal error!\n" );
    fprintf ( stderr, "  Could not open the output file.\n" );
    exit ( 1 );
  }
/*
  Write the data.
*/
  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < m; i++ )
    {
      fprintf ( output, "  %24.16g", table[i+j*m] );
    }
    fprintf ( output, "\n" );
  }
/*
  Close the file.
*/
  fclose ( output );

  return;
}
/******************************************************************************/

void r8vec_reverse ( int n, double a[] )

/******************************************************************************/
/*
  Purpose:

    R8VEC_REVERSE reverses the elements of an R8VEC.

  Discussion:

    An R8VEC is a vector of R8's.

  Example:

    Input:

      N = 5, A = ( 11.0, 12.0, 13.0, 14.0, 15.0 ).

    Output:

      A = ( 15.0, 14.0, 13.0, 12.0, 11.0 ).

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    23 August 2010

  Author:

    John Burkardt

  Parameters:

    Input, int N, the number of entries in the array.

    Input/output, double A[N], the array to be reversed.
*/
{
  int i;
  int i_hi;
  double temp;

  i_hi = n / 2;

  for ( i = 1; i <= i_hi; i++ )
  {
    temp   = a[i-1];
    a[i-1] = a[n-i];
    a[n-i] = temp;
  }

  return;
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

