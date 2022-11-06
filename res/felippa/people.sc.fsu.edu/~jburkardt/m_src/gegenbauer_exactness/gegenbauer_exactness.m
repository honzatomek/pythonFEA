function gegenbauer_exactness ( quad_filename, degree_max, alpha )

%*****************************************************************************80
%
%% gegenbauer_exactness() determines the exactness of a Gegenbauer quadrature rule.
%
%  Discussion:
%
%    This program investigates a standard Gauss-Gegenbauer quadrature rule
%    by using it to integrate monomials over [-1,+1], and comparing the
%    approximate result to the known exact value.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 August 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    character QUAD_FILENAME, the "root" name of the R, W and X files 
%    that specify the rule;
%
%    integer DEGREE_MAX, the maximum monomial degree to be checked;
%
%    integer ALPHA, the exponent of the (1-X^2) factor;
%
  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'gegenbauer_exactness():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Investigate the polynomial exactness of a Gauss-Gegenbauer\n' );
  fprintf ( 1, '  quadrature rule by integrating weighted\n' );
  fprintf ( 1, '  monomials up to a given degree over the [-1,+1] interval.\n' );
%
%  Get the quadrature file root name:
%
  if ( 1 <= nargin )

  else

    fprintf ( 1, '\n' );
    fprintf ( 1, 'gegenbauer_exactness:\n' );

    quad_filename = input ( '  Enter the "root" name of the quadrature files.' );

  end
%
%  Create the names of:
%    the quadrature X file;
%    the quadrature W file;
%    the quadrature R file;
%
  quad_x_filename = strcat ( quad_filename, '_x.txt' );
  quad_w_filename = strcat ( quad_filename, '_w.txt' );
  quad_r_filename = strcat ( quad_filename, '_r.txt' );
%
%  The second command line argument is the maximum degree.
%
  if ( 2 <= nargin )

  else

    fprintf ( 1, '\n' );
    fprintf ( 1, 'gegenbauer_exactness():\n' );

    degree_max = input ( '  Please enter the maximum degree to check.' );

  end
%
%  The third command line argument is ALPHA.
%
  if ( 3 <= nargin )

  else

    fprintf ( 1, '\n' );
    fprintf ( 1, 'gegenbauer_exactness():\n' );
    fprintf ( 1, '  ALPHA is the power of (1-X^2) in the weighting function.\n' );
    fprintf ( 1, '\n' );
    fprintf ( 1, '  ALPHA is a real number greater than -1.0.\n' );
    fprintf ( 1, '\n' );
    alpha = input ( '  Please enter ALPHA.' );

  end
%
%  Summarize the input.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'gegenbauer_exactness(): User input:\n' );
  fprintf ( 1, '  Quadrature rule X file = "%s".\n', quad_x_filename );
  fprintf ( 1, '  Quadrature rule W file = "%s".\n', quad_w_filename );
  fprintf ( 1, '  Quadrature rule R file = "%s".\n', quad_r_filename );
  fprintf ( 1, '  Maximum degree to check = %d\n', degree_max );
  fprintf ( 1, '  Exponent of (1-x^2), ALPHA = %f\n', alpha );
%
%  Read the X file.
%
  x = load ( quad_x_filename );
  x = x';
  [ dim_num, order ] = size ( x );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Spatial dimension = %d\n', dim_num );
  fprintf ( 1, '  Number of points  = %d\n', order );
%
%  Read the W file.
%
  w = load ( quad_w_filename );
  w = w';
  [ dim_num2, point_num ] = size ( w );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Spatial dimension = %d\n', dim_num2 );
  fprintf ( 1, '  Number of points  = %d\n', point_num );
%
%  Read the R file.
%
  r = load ( quad_r_filename );
  r = r';
  [ dim_num2, point_num ] = size ( r );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Spatial dimension = %d\n', dim_num2 );
  fprintf ( 1, '  Number of points  = %d\n', point_num );
%
%  Print the input quadrature rule.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Tested a Gauss-Gegenbauer rule\n' );
  fprintf ( 1, '  ORDER = %d\n', order );
  fprintf ( 1, '  ALPHA = %f\n', alpha );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Standard rule:\n' );
  fprintf ( 1, '    Integral ( -1 <= x <= +1 ) (1-x^2)^alpha f(x) dx\n' );
  fprintf ( 1, '    is to be approximated by\n' );
  fprintf ( 1, '    sum ( 1 <= I <= ORDER ) w(i) * f(x(i)).\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Weights W:\n' );
  fprintf ( 1, '\n' );
  for i = 1 : order
    fprintf ( 1, '  w(%d) = %24.16f\n', i, w(i) );
  end
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Abscissas X:\n' );
  fprintf ( 1, '\n' );
  for i = 1 : order
    fprintf ( 1, '  x(%d) = %24.16f\n', i, x(i) );
  end
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Region R:\n' );
  fprintf ( 1, '\n' );
  for i = 1 : 2
    fprintf ( 1, '  r(%d) = %e\n', i, r(i) );
  end
%
%  Explore the monomials.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  A Gauss-Gegenbauer rule would be able to exactly\n' );
  fprintf ( 1, '  integrate monomials up to and including \n' );
  fprintf ( 1, '  degree = %d\n', 2 * order - 1 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '      Error    Degree\n' );
  fprintf ( 1, '\n' );

  for degree = 0 : degree_max

    quad_error = monomial_quadrature_gegenbauer ( degree, alpha, order, w, x );

    fprintf ( 1, '  %24.16f   %2d\n', quad_error, degree );

  end
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'gegenbauer_exactness():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  return
end
function value = gegenbauer_integral ( expon, alpha )

%*****************************************************************************80
%
%% gegenbauer_integral() evaluates the integral of a monomial with Gegenbauer weight.
%
%  Discussion:
%
%    VALUE = Integral ( -1 <= X <= +1 ) x^EXPON (1-x^2)^ALPHA dx
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    03 March 2008
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer EXPON, the exponent.
%
%    real ALPHA, the exponent of (1-X^2)
%    in the weight factor.
%
%  Output:
%
%    real VALUE, the value of the integral.
%
  if ( mod ( expon, 2 ) == 1 )
    value = 0.0;
    return
  end

  c = expon;

  arg1 = - alpha;
  arg2 =   1.0 + c;
  arg3 =   2.0 + alpha + c;
  arg4 = - 1.0;

  value1 = r8_hyper_2f1 ( arg1, arg2, arg3, arg4 );

  value = 2.0 * gamma ( 1.0 + c ) * gamma ( 1.0 + alpha ) ...
    * value1 / gamma ( 2.0 + alpha  + c );

  return
end
function quad_error = monomial_quadrature_gegenbauer ( expon, alpha, ...
  order, w, x )

%*****************************************************************************80
%
%% monomial_quadrature_gegenbauer() applies a quadrature rule to a monomial.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 March 2008
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer EXPON, the exponent.
%
%    real ALPHA, the exponent of (1-X^2) in the weight factor.
%
%    intege ORDER, the number of points in the rule.
%
%    real W(ORDER), the quadrature weights.
%
%    real X(ORDER), the quadrature points.
%
%  Output:
%
%    real QUAD_ERROR, the quadrature error.
%

%
%  Get the exact value of the integral.
%
  exact = gegenbauer_integral ( expon, alpha );
%
%  Evaluate the monomial at the quadrature points.
%
  value(1:order) = x(1:order).^expon;
%
%  Compute the weighted sum.
%
  quad = w * value';
%
%  Error:
%
  if ( exact == 0.0 )
    quad_error = abs ( quad - exact );
  else
    quad_error = abs ( ( quad - exact ) / exact );
  end

  return
end
function hf = r8_hyper_2f1 ( a, b, c, x )

%*****************************************************************************80
%
%% r8_hyper_2f1() evaluates the hypergeometric function 2F1(A,B,C,X).
%
%  Discussion:
%
%    A minor bug was corrected.  The HW variable, used in several places as
%    the "old" value of a quantity being iteratively improved, was not
%    being initialized.  JVB, 11 February 2008.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    11 February 2008
%
%  Author:
%
%    F77 original by Shanjie Zhang, Jianming Jin.
%    MATLAB version by John Burkardt.
%
%    The F77 original version of this routine is copyrighted by
%    Shanjie Zhang and Jianming Jin.  However, they give permission to
%    incorporate this routine into a user program provided that the copyright
%    is acknowledged.
%
%  Reference:
%
%    Shanjie Zhang, Jianming Jin,
%    Computation of Special Functions,
%    Wiley, 1996,
%    ISBN: 0-471-11963-6,
%    LC: QA351.C45
%
%  Input:
%
%    real A, B, C, X, the arguments of the function.
%    C must not be equal to a nonpositive integer.
%    X < 1.
%
%  Output:
%
%    real HF, the value of the function.
%
  el = 0.5772156649015329;

  l0 = ( c == floor ( c ) ) & ( c < 0.0 );
  l1 = ( 1.0 - x < 1.0E-15 ) & ( c - a - b <= 0.0 );
  l2 = ( a == floor ( a ) ) & ( a < 0.0 );
  l3 = ( b == floor ( b ) ) & ( b < 0.0 );
  l4 = ( c - a == floor ( c - a ) ) & ( c - a <= 0.0 );
  l5 = ( c - b == floor ( c - b ) ) & ( c - b <= 0.0 );

  if ( l0 | l1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'r8_hyper_2f1(): Fatal error!\n' );
    fprintf ( 1, '  The hypergeometric series is divergent.\n' );
    return
  end

  if ( 0.95 < x )
    eps = 1.0E-08;
  else
    eps = 1.0E-15;
  end

  if ( x == 0.0 | a == 0.0 | b == 0.0 )

    hf = 1.0;
    return

  elseif ( 1.0 - x == eps & 0.0 < c - a - b )

    gc = gamma ( c );
    gcab = gamma ( c - a - b );
    gca = gamma ( c - a );
    gcb = gamma ( c - b );
    hf = gc * gcab / ( gca * gcb );
    return

  elseif ( 1.0 + x <= eps & abs ( c - a + b - 1.0 ) <= eps )

    g0 = sqrt ( pi ) * 2.0^( - a );
    g1 = gamma ( c );
    g2 = gamma ( 1.0 + a / 2.0 - b );
    g3 = gamma ( 0.5 + 0.5 * a );
    hf = g0 * g1 / ( g2 * g3 );
    return

  elseif ( l2 | l3 )

    if ( l2 )
      nm = floor ( abs ( a ) );
    end

    if ( l3 )
      nm = floor ( abs ( b ) );
    end

    hf = 1.0;
    r = 1.0;

    for k = 1 : nm
      r = r * ( a + k - 1.0 ) * ( b + k - 1.0 ) ...
        / ( k * ( c + k - 1.0 ) ) * x;
      hf = hf + r;
    end

    return

  elseif ( l4 | l5 )

    if ( l4 )
      nm = floor ( abs ( c - a ) );
    end

    if ( l5 )
      nm = floor ( abs ( c - b ) );
    end

    hf = 1.0;
    r  = 1.0;
    for k = 1 : nm
      r = r * ( c - a + k - 1.0 ) * ( c - b + k - 1.0 ) ...
        / ( k * ( c + k - 1.0 ) ) * x;
      hf = hf + r;
    end
    hf = ( 1.0 - x )^( c - a - b ) * hf;
    return

  end

  aa = a;
  bb = b;
  x1 = x;

  if ( x < 0.0 )
    x = x / ( x - 1.0 );
    if ( a < c & b < a & 0.0 < b )
      a = bb;
      b = aa;
    end
    b = c - b;
  end

  if ( 0.75 <= x )

    gm = 0.0;

    if ( abs ( c - a - b - floor ( c - a - b ) ) < 1.0E-15 )

      m = floor ( c - a - b );
      ga = gamma ( a );
      gb = gamma ( b );
      gc = gamma ( c );
      gam = gamma ( a + m );
      gbm = gamma ( b + m );

      pa = r8_psi ( a );
      pb = r8_psi ( b );

      if ( m ~= 0 )
        gm = 1.0;
      end

      for j = 1 : abs ( m ) - 1
        gm = gm * j;
      end

      rm = 1.0;
      for j = 1 : abs ( m )
        rm = rm * j;
      end

      f0 = 1.0;
      r0 = 1.0;
      r1 = 1.0;
      sp0 = 0.0;
      sp = 0.0;

      if ( 0 <= m )

        c0 = gm * gc / ( gam * gbm );
        c1 = - gc * ( x - 1.0 )^m / ( ga * gb * rm );

        for k = 1 : m - 1
          r0 = r0 * ( a + k - 1.0 ) * ( b + k - 1.0 ) ...
            / ( k * ( k - m ) ) * ( 1.0 - x );
          f0 = f0 + r0;
        end

        for k = 1 : m
          sp0 = sp0 + 1.0 / ( a + k - 1.0 ) ...
            + 1.0 / ( b + k - 1.0 ) - 1.0 / k;
        end

        f1 = pa + pb + sp0 + 2.0 * el + log ( 1.0 - x );
        hw = f1;

        for k = 1 : 250

          sp = sp + ( 1.0 - a ) / ( k * ( a + k - 1.0 ) ) ...
            + ( 1.0 - b ) / ( k * ( b + k - 1.0 ) );

          sm = 0.0;
          for j = 1 : m
            sm = sm + ( 1.0 - a ) ...
              / ( ( j + k ) * ( a + j + k - 1.0 ) ) ...
              + 1.0 / ( b + j + k - 1.0 );
          end

          rp = pa + pb + 2.0 * el + sp + sm + log ( 1.0 - x );

          r1 = r1 * ( a + m + k - 1.0 ) * ( b + m + k - 1.0 ) ...
            / ( k * ( m + k ) ) * ( 1.0 - x );

          f1 = f1 + r1 * rp;

          if ( abs ( f1 - hw ) < abs ( f1 ) * eps )
            break
          end

          hw = f1;

        end

        hf = f0 * c0 + f1 * c1;

      elseif ( m < 0 )

        m = - m;
        c0 = gm * gc / ( ga * gb * ( 1.0 - x )^m );
        c1 = - ( - 1 )^m * gc / ( gam * gbm * rm );

        for k = 1 : m - 1
          r0 = r0 * ( a - m + k - 1.0 ) * ( b - m + k - 1.0 ) ...
            / ( k * ( k - m ) ) * ( 1.0 - x );
          f0 = f0 + r0;
        end

        for k = 1 : m
          sp0 = sp0 + 1.0 / k;
        end

        f1 = pa + pb - sp0 + 2.0 * el + log ( 1.0 - x );
        hw = f1;

        for k = 1 : 250

          sp = sp + ( 1.0 - a ) ...
            / ( k * ( a + k - 1.0 ) ) ...
            + ( 1.0 - b ) / ( k * ( b + k - 1.0 ) );

          sm = 0.0;
          for j = 1 : m
            sm = sm + 1.0 / ( j + k );
          end

          rp = pa + pb + 2.0 * el + sp - sm + log ( 1.0 - x );

          r1 = r1 * ( a + k - 1.0 ) * ( b + k - 1.0 ) ...
            / ( k * ( m + k ) ) * ( 1.0 - x );

          f1 = f1 + r1 * rp;

          if ( abs ( f1 - hw ) < abs ( f1 ) * eps )
            break
          end

          hw = f1;

        end

        hf = f0 * c0 + f1 * c1;

      end

    else

      ga = gamma ( a );
      gb = gamma ( b );
      gc = gamma ( c );
      gca = gamma ( c - a );
      gcb = gamma ( c - b );
      gcab = gamma ( c - a - b );
      gabc = gamma ( a + b - c );
      c0 = gc * gcab / ( gca * gcb );
      c1 = gc * gabc / ( ga * gb ) * ( 1.0 - x )^( c - a - b );
      hf = 0.0;
      hw = hf;
      r0 = c0;
      r1 = c1;

      for k = 1 : 250

        r0 = r0 * ( a + k - 1.0 ) * ( b + k - 1.0 ) ...
          / ( k * ( a + b - c + k ) ) * ( 1.0 - x );

        r1 = r1 * ( c - a + k - 1.0 ) * ( c - b + k - 1.0 ) ...
          / ( k * ( c - a - b + k ) ) * ( 1.0 - x );

        hf = hf + r0 + r1;

        if ( abs ( hf - hw ) < abs ( hf ) * eps )
          break
        end

        hw = hf;

      end

      hf = hf + c0 + c1;

    end

  else

    a0 = 1.0;

    if ( a < c & c < 2.0 * a & b < c & c < 2.0 * b )

      a0 = ( 1.0 - x )^( c - a - b );
      a = c - a;
      b = c - b;

    end

    hf = 1.0;
    hw = hf;
    r = 1.0;

    for k = 1 : 250

      r = r * ( a + k - 1.0 ) * ( b + k - 1.0 ) ...
        / ( k * ( c + k - 1.0 ) ) * x;

      hf = hf + r;

      if ( abs ( hf - hw ) <= abs ( hf ) * eps )
        break
      end

      hw = hf;

    end

    hf = a0 * hf;

  end

  if ( x1 < 0.0 )
    x = x1;
    c0 = 1.0 / ( 1.0 - x )^aa;
    hf = c0 * hf;
  end

  if ( 120 < k )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'r8_hyper_2f1(): Warning!\n' );
    fprintf ( 1, '  A large number of iterations were needed.\n' );
    fprintf ( 1, '  The accuracy of the results should be checked.\n' );
  end

  return
end
function value = r8_psi ( xx )

%*****************************************************************************80
%
%% r8_psi() evaluates the function Psi(X).
%
%  Discussion:
%
%    This routine evaluates the logarithmic derivative of the
%    Gamma function,
%
%      PSI(X) = d/dX ( GAMMA(X) ) / GAMMA(X)
%             = d/dX LN ( GAMMA(X) )
%
%    for real X, where either
%
%      - XMAX1 < X < - XMIN, and X is not a negative integer,
%
%    or
%
%      XMIN < X.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    09 February 2008
%
%  Author:
%
%    Original FOTRAN77 version by William Cody.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    William Cody, Anthony Strecok, Henry Thacher,
%    Chebyshev Approximations for the Psi Function,
%    Mathematics of Computation,
%    Volume 27, Number 121, January 1973, pages 123-127.
%
%  Input:
%
%    real XX, the argument of the function.
%
%  Output:
%
%    real VALUE, the value of the function.
%
  four = 4.0;
  fourth = 0.25;
  half = 0.5;
  one = 1.0;
  p1(1:9) = [ ...
   4.5104681245762934160E-03, ...
   5.4932855833000385356, ...
   3.7646693175929276856E+02, ...
   7.9525490849151998065E+03, ...
   7.1451595818951933210E+04, ...
   3.0655976301987365674E+05, ...
   6.3606997788964458797E+05, ...
   5.8041312783537569993E+05, ...
   1.6585695029761022321E+05 ];
  p2(1:7) = [ ...
  -2.7103228277757834192, ...
  -1.5166271776896121383E+01, ...
  -1.9784554148719218667E+01, ...
  -8.8100958828312219821, ...
  -1.4479614616899842986, ...
  -7.3689600332394549911E-02, ...
  -6.5135387732718171306E-21 ];
  piov4 = 0.78539816339744830962;
  q1(1:8) = [ ...
   9.6141654774222358525E+01, ...
   2.6287715790581193330E+03, ...
   2.9862497022250277920E+04, ...
   1.6206566091533671639E+05, ...
   4.3487880712768329037E+05, ...
   5.4256384537269993733E+05, ...
   2.4242185002017985252E+05, ...
   6.4155223783576225996E-08 ];
  q2(1:6) = [ ...
   4.4992760373789365846E+01, ...
   2.0240955312679931159E+02, ...
   2.4736979003315290057E+02, ...
   1.0742543875702278326E+02, ...
   1.7463965060678569906E+01, ...
   8.8427520398873480342E-01 ];
  three = 3.0;
  x01 = 187.0;
  x01d = 128.0;
  x02 = 6.9464496836234126266E-04;
  xinf = 1.70E+38;
  xlarge = 2.04E+15;
  xmax1 = 3.60E+16;
  xmin1 = 5.89E-39;
  xsmall = 2.05E-09;
  zero = 0.0;

  x = xx;
  w = abs ( x );
  aug = zero;
%
%  Check for valid arguments, then branch to appropriate algorithm.
%
  if ( xmax1 <= - x | w < xmin1 )

    if ( zero < x )
      value = - xinf;
    else
      value = xinf;
    end

    return
  end

  if ( x < half )
%
%  X < 0.5, use reflection formula: psi(1-x) = psi(x) + pi * cot(pi*x)
%  Use 1/X for PI*COTAN(PI*X)  when  XMIN1 < |X| <= XSMALL.
%
    if ( w <= xsmall )

      aug = - one / x;
%
%  Argument reduction for cotangent.
%
    else

      if ( x < zero )
        sgn = piov4;
      else
        sgn = - piov4;
      end

      w = w - floor ( w );
      nq = floor ( w * four );
      w = four * ( w - nq * fourth );
%
%  W is now related to the fractional part of 4.0 * X.
%  Adjust argument to correspond to values in the first
%  quadrant and determine the sign.
%
      n = floor ( nq / 2 );

      if ( n + n ~= nq )
        w = one - w;
      end

      z = piov4 * w;

      if ( mod ( n, 2 ) ~= 0 )
        sgn = - sgn;
      end
%
%  Determine the final value for  -pi * cotan(pi*x).
%
      n = floor ( ( nq + 1 ) / 2 );
      if ( mod ( n, 2 ) == 0 )
%
%  Check for singularity.
%
        if ( z == zero )

          if ( zero < x )
            value = -xinf;
          else
            value = xinf;
          end

          return
        end

        aug = sgn * ( four / tan ( z ) );

      else

        aug = sgn * ( four * tan ( z ) );

      end

    end

    x = one - x;

  end
%
%  0.5 <= X <= 3.0.
%
  if ( x <= three )

    den = x;
    upper = p1(1) * x;
    for i = 1 : 7
      den = ( den + q1(i) ) * x;
      upper = ( upper + p1(i+1) ) * x;
    end
    den = ( upper + p1(9) ) / ( den + q1(8) );
    x = ( x - x01 / x01d ) - x02;
    value = den * x + aug;
    return

  end
%
%  3.0 < X.
%
  if ( x < xlarge )
    w = one / ( x * x );
    den = w;
    upper = p2(1) * w;
    for i = 1 : 5
      den = ( den + q2(i) ) * w;
      upper = ( upper + p2(i+1) ) * w;
    end
    aug = ( upper + p2(7) ) / ( den + q2(6) ) - half / x + aug;
  end

  value = aug + log ( x );

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp() prints the current YMDHMS date as a timestamp.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  t = now;
  c = datevec ( t );
  s = datestr ( c, 0 );
  fprintf ( 1, '%s\n', s );

  return
end
 
