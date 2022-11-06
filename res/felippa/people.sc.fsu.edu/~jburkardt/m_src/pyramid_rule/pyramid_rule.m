function pyramid_rule ( legendre_order, jacobi_order, filename )

%*****************************************************************************80
%
%% pyramid_rule() generates a quadrature rule for a pyramid.
%
%  Discussion:
%
%    This program computes a quadrature rule for a pyramid
%    and writes it to a file.
%
%    The integration region is:
% 
%      - ( 1 - Z ) <= X <= 1 - Z
%      - ( 1 - Z ) <= Y <= 1 - Z
%                0 <= Z <= 1.
%
%    When Z is zero, the integration region is a square lying in the (X,Y) 
%    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the 
%    radius of the square diminishes, and when Z reaches 1, the square has 
%    contracted to the single point (0,0,1).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    23 July 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer LEGENDRE_ORDER, the number of points in the X and Y dimensions;
%
%    integer the JACOBI_ORDER, the number of points in the Z dimension;
%
%    character FILENAME, the root name of the output files.
%
  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'PYRAMID_RULE\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Compute a quadrature rule for approximating\n' );
  fprintf ( 1, '  the integral of a function over a pyramid.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  The user specifies:\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  LEGENDRE_ORDER, the order of the Legendre rule for X and Y.\n' );
  fprintf ( 1, '  JACOBI_ORDER, the order of the Jacobi rule for Z,\n' );
  fprintf ( 1, '  FILENAME, the prefix for the three output files:\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '    filename_w.txt - the weight file\n' );
  fprintf ( 1, '    filename_x.txt - the abscissa file.\n' );
  fprintf ( 1, '    filename_r.txt - the region file.\n' );
%
%  Get the Legendre order.
%
  if ( nargin < 1 )
    fprintf ( 1, '\n' );
    legendre_order = input ( '  Enter the Legendre rule order:' );
  elseif ( ischar ( legendre_order ) )
    legendre_order = str2num ( legendre_order );
  end

  fprintf ( 1, '\n' );
  fprintf ( 1, '  The requested Legendre order of the rule is %d.\n', ...
    legendre_order );
%
%  Get the Jacobi order.
%
  if ( nargin < 2 )
    fprintf ( 1, '\n' );
    jacobi_order = input ( '  Enter the Jacobi rule order:' );
  elseif ( ischar ( jacobi_order ) )
    jacobi_order = str2num ( jacobi_order );       
  end

  fprintf ( 1, '\n' );
  fprintf ( 1, '  The requested Jacobi order of the rule is %d.\n', ...
    jacobi_order );
%
%  Get the output option or quadrature file root name:
%
  if ( nargin < 3 )
    fprintf ( 1, '\n' );
    filename = input ( '  Enter the "root name" of the quadrature files).' );
  end

  pyramid_handle ( legendre_order, jacobi_order, filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'PYRAMID_RULE:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  return
end
function [ x, w ] = jacobi_compute ( order, alpha, beta )

%*****************************************************************************80
%
%% jacobi_compute() computes the abscissa and weights for Jacobi quadrature.
%
%  Discussion:
%
%    The integration interval is [ -1, 1 ].
%
%    The weight function is w(x) = (1-X)^ALPHA * (1+X)^BETA.
%
%    The integral to approximate is:
%
%      Integral ( -1 <= X <= 1 ) (1-X)^ALPHA * (1+X)^BETA * F(X) dX
%
%    The quadrature formula is:
%
%      Sum ( 1 <= I <= ORDER ) W(I) * F ( X(I) )
%
%    Thanks to Xu Xiang of Fudan University for pointing out that
%    an earlier implementation of this routine was incorrect!
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
%    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Arthur Stroud, Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966,
%    LC: QA299.4G3S7.
%
%  Input:
%
%    integer ORDER, the order of the quadrature rule to be computed.
%
%    real ALPHA, BETA, the exponents of (1-X) and
%    (1+X) in the quadrature rule.  For simple Legendre quadrature,
%    set ALPHA = BETA = 0.0.  -1.0 < ALPHA and -1.0 < BETA are required.
%
%  Output:
%
%    real X(ORDER), the abscissas.
%
%    real W(ORDER), the weights.
%
  if ( order < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'JACOBI_COMPUTE - Fatal error!\n' );
    fprintf ( 1, '  Illegal value of ORDER = %d\n', order );
    error ( 'JACOBI_COMPUTE - Fatal error!' );
  end
%
%  Check ALPHA and BETA.
%
  if ( alpha <= -1.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'JACOBI_COMPUTE - Fatal error!\n' );
    fprintf ( 1, '  -1.0 < ALPHA is required.\n' );
    error ( 'JACOBI_COMPUTE - Fatal error!' );
  end

  if ( beta <= -1.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'JACOBI_COMPUTE - Fatal error!\n' );
    fprintf ( 1, '  -1.0 < BETA is required.\n' );
    error ( 'JACOBI_COMPUTE - Fatal error!' );
  end
%
%  Set the recursion coefficients.
%
  b = zeros ( order, 1 );
  c = zeros ( order, 1 );
  w = zeros ( order, 1 );
  x = zeros ( order, 1 );
  
  for i = 1 : order

    if ( alpha + beta == 0.0 || beta - alpha == 0.0 )

      b(i) = 0.0;

    else

      b(i) = ( alpha + beta ) * ( beta - alpha ) / ...
            ( ( alpha + beta + 2 * i ) ...
            * ( alpha + beta + 2 * i - 2 ) );

    end

    if ( i == 1 )

      c(i) = 0.0;

    else

      c(i) = 4.0 * ( i - 1 ) * ( alpha + i - 1 ) * ( beta + i - 1 ) ...
        * ( alpha + beta + i - 1 ) / ( ( alpha + beta + 2 * i - 1 ) ...
        * ( alpha + beta + 2 * i - 2 )^2 * ( alpha + beta + 2 * i - 3 ) );

    end

  end

  delta = gamma ( alpha        + 1.0 ) ...
        * gamma (         beta + 1.0 ) ...
        / gamma ( alpha + beta + 2.0 );

  cc = delta * 2.0^( alpha + beta + 1.0 ) * prod ( c(2:order) );

  for i = 1 : order

    if ( i == 1 )

      an = alpha / order;
      bn = beta / order;

      r1 = ( 1.0 + alpha ) * ( 2.78 / ( 4.0 + order * order ) ...
        + 0.768 * an / order );

      r2 = 1.0 + 1.48 * an + 0.96 * bn + 0.452 * an^2 + 0.83 * an * bn;

      x0 = ( r2 - r1 ) / r2;

    elseif ( i == 2 )

      r1 = ( 4.1 + alpha ) / ...
        ( ( 1.0 + alpha ) * ( 1.0 + 0.156 * alpha ) );

      r2 = 1.0 + 0.06 * ( order - 8.0 ) * ( 1.0 + 0.12 * alpha ) / order;

      r3 = 1.0 + 0.012 * beta * ...
        ( 1.0 + 0.25 * abs ( alpha ) ) / order;

      x0 = x0 - r1 * r2 * r3 * ( 1.0 - x0 );

    elseif ( i == 3 )

      r1 = ( 1.67 + 0.28 * alpha ) / ( 1.0 + 0.37 * alpha );

      r2 = 1.0 + 0.22 * ( order - 8.0 ) / order;

      r3 = 1.0 + 8.0 * beta / ( ( 6.28 + beta ) * order * order );

      x0 = x0 - r1 * r2 * r3 * ( x(1) - x0 );

    elseif ( i < order - 1 )

      x0 = 3.0 * x(i-1) - 3.0 * x(i-2) + x(i-3);

    elseif ( i == order - 1 )

      r1 = ( 1.0 + 0.235 * beta ) / ( 0.766 + 0.119 * beta );

      r2 = 1.0 / ( 1.0 + 0.639 * ( order - 4.0 ) ...
        / ( 1.0 + 0.71 * ( order - 4.0 ) ) );

      r3 = 1.0 / ( 1.0 + 20.0 * alpha / ( ( 7.5 + alpha ) * order * order ) );

      x0 = x0 + r1 * r2 * r3 * ( x0 - x(i-2) );

    elseif ( i == order )

      r1 = ( 1.0 + 0.37 * beta ) / ( 1.67 + 0.28 * beta );

      r2 = 1.0 / ( 1.0 + 0.22 * ( order - 8.0 ) / order );

      r3 = 1.0 / ( 1.0 + 8.0 * alpha / ( ( 6.28 + alpha ) * order * order ) );

      x0 = x0 + r1 * r2 * r3 * ( x0 - x(i-2) );

    end

    [ x0, dp2, p1 ] = jacobi_root ( x0, order, alpha, beta, b, c );

    x(i) = x0;
    w(i) = cc / ( dp2 * p1 );

  end
%
%  Reverse the order of the values.
%
  x(1:order) = x(order:-1:1);
  w(1:order) = w(order:-1:1);

  return
end
function [ p2, dp2, p1 ] = jacobi_recur ( x, order, alpha, beta, b, c )

%*****************************************************************************80
%
%% jacobi_recur() finds the value and derivative of a Jacobi polynomial.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 October 2005
%
%  Author:
%
%    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Arthur Stroud, Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966,
%    LC: QA299.4G3S7.
%
%  Input:
%
%    real X, the point at which polynomials are evaluated.
%
%    integer ORDER, the order of the polynomial to be computed.
%
%    real ALPHA, BETA, the exponents of (1+X) and
%    (1-X) in the quadrature rule.
%
%    real B(ORDER), C(ORDER), the recursion
%    coefficients.
%
%  Output:
%
%    real P2, the value of J(ORDER)(X).
%
%    real DP2, the value of J'(ORDER)(X).
%
%    real P1, the value of J(ORDER-1)(X).
%
  p1 = 1.0;
  dp1 = 0.0;

  p2 = x + ( alpha - beta ) / ( alpha + beta + 2.0 );
  dp2 = 1.0;

  for i = 2 : order

    p0 = p1;
    dp0 = dp1;

    p1 = p2;
    dp1 = dp2;

    p2 = ( x - b(i) ) * p1 - c(i) * p0;
    dp2 = ( x - b(i) ) * dp1 + p1 - c(i) * dp0;

  end

  return
end
function [ x, dp2, p1 ] = jacobi_root ( x, order, alpha, beta, b, c )

%*****************************************************************************80
%
%% jacobi_root() improves an approximate root of a Jacobi polynomial.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 October 2005
%
%  Author:
%
%    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Arthur Stroud, Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966,
%    LC: QA299.4G3S7.
%
%  Input:
%
%    real X, the approximate root.
%
%    integer ORDER, the order of the polynomial to be computed.
%
%    real ALPHA, BETA, the exponents of (1+X) and
%    (1-X) in the quadrature rule.
%
%    real B(ORDER), C(ORDER), the recursion coefficients.
%
%  Output:
%
%    real X, the improved approximate root.
%
%    real DP2, the value of J'(ORDER)(X).
%
%    real P1, the value of J(ORDER-1)(X).
%
  maxstep = 10;

  for i = 1 : maxstep

    [ p2, dp2, p1 ] = jacobi_recur ( x, order, alpha, beta, b, c );

    d = p2 / dp2;
    x = x - d;

    if ( abs ( d ) <= eps * ( abs ( x ) + 1.0 ) )
      return
    end

  end

  return
end
function [ x, w ] = legendre_compute ( order )

%*****************************************************************************80
%
%% legendre_compute() computes a Legendre quadrature rule.
%
%  Discussion:
%
%    The integration interval is [ -1, 1 ].
%
%    The weight function is w(x) = 1.0.
%
%    The integral to approximate:
%
%      Integral ( -1 <= X <= 1 ) F(X) dX
%
%    The quadrature rule:
%
%      Sum ( 1 <= I <= ORDER ) W(I) * F ( X(I) )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 January 2008
%
%  Author:
%
%    Original FORTRAN77 version by Philip Davis, Philip Rabinowitz.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Philip Davis, Philip Rabinowitz,
%    Methods of Numerical Integration,
%    Second Edition,
%    Dover, 2007,
%    ISBN: 0486453391,
%    LC: QA299.3.D28.
%
%  Input:
%
%    integer ORDER, the order of the rule.
%    ORDER must be greater than 0.
%
%  Output:
%
%    real X(ORDER), the abscissas of the rule.
%
%    real W(ORDER), the weights of the rule.
%    The weights are positive, symmetric, and should sum to 2.
%
  if ( order < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'LEGENDRE_COMPUTE - Fatal error!\n' );
    fprintf ( 1, '  Illegal value of ORDER = %d\n', order );
    error ( 'LEGENDRE_COMPUTE - Fatal error!' );
  end

  w = zeros ( order, 1 );
  x = zeros ( order, 1 );
  
  e1 = order * ( order + 1 );

  m = floor ( ( order + 1 ) / 2 );

  for i = 1 : floor ( ( order + 1 ) / 2 )

    mp1mi = m + 1 - i;

    t = ( 4 * i - 1 ) * pi / ( 4 * order + 2 );

    x0 = cos(t) * ( 1.0 - ( 1.0 - 1.0 / ( order ) ) / ( 8 * order * order ) );

    pkm1 = 1.0;
    pk = x0;

    for k = 2 : order
      pkp1 = 2.0 * x0 * pk - pkm1 - ( x0 * pk - pkm1 ) / k;
      pkm1 = pk;
      pk = pkp1;
    end

    d1 = order * ( pkm1 - x0 * pk );

    dpn = d1 / ( 1.0 - x0 * x0 );

    d2pn = ( 2.0 * x0 * dpn - e1 * pk ) / ( 1.0 - x0 * x0 );

    d3pn = ( 4.0 * x0 * d2pn + ( 2.0 - e1 ) * dpn ) / ( 1.0 - x0 * x0 );

    d4pn = ( 6.0 * x0 * d3pn + ( 6.0 - e1 ) * d2pn ) / ( 1.0 - x0 * x0 );

    u = pk / dpn;
    v = d2pn / dpn;
%
%  Initial approximation H:
%
    h = - u * ( 1.0 + 0.5 * u * ( v + u * ( v * v - d3pn / ( 3.0 * dpn ) ) ) );
%
%  Refine H using one step of Newton's method:
%
    p = pk + h * ( dpn + 0.5 * h * ( d2pn + h / 3.0 ...
      * ( d3pn + 0.25 * h * d4pn ) ) );

    dp = dpn + h * ( d2pn + 0.5 * h * ( d3pn + h * d4pn / 3.0 ) );

    h = h - p / dp;

    xtemp = x0 + h;

    x(mp1mi) = xtemp;

    fx = d1 - h * e1 * ( pk + 0.5 * h * ( dpn + h / 3.0 ...
      * ( d2pn + 0.25 * h * ( d3pn + 0.2 * h * d4pn ) ) ) );

    w(mp1mi) = 2.0 * ( 1.0 - xtemp * xtemp ) / fx / fx;

  end

  if ( mod ( order, 2 ) == 1 )
    x(1) = 0.0;
  end
%
%  Shift the data up.
%
  nmove = floor ( ( order + 1 ) / 2 );
  ncopy = order - nmove;

  for i = 1 : nmove
    iback = order + 1 - i;
    x(iback) = x(iback-ncopy);
    w(iback) = w(iback-ncopy);
  end
%
%  Reflect values for the negative abscissas.
%
  for i = 1 : order - nmove
    x(i) = - x(order+1-i);
    w(i) = w(order+1-i);
  end

  return
end
function pyramid_handle ( legendre_order, jacobi_order, filename )

%*****************************************************************************80
%
%% pyramid_handle() computes the requested pyramid rule and outputs it.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 July 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer LEGENDRE_ORDER, JACOBI_ORDER, the orders
%    of the component Legendre and Jacobi rules.
%
%    string FILENAME, the rootname for the files,
%    write files 'file_w.txt' and 'file_x.txt', and 'file_r.txt', weights,
%    abscissas, and region.
%
  dim_num = 3;
%
%  Compute the factor rules.
%
  [ legendre_x, legendre_w ] = legendre_compute ( legendre_order );

  jacobi_alpha = 2.0;
  jacobi_beta = 0.0;

  [ jacobi_x, jacobi_w ] = jacobi_compute ( jacobi_order, jacobi_alpha, ...
    jacobi_beta );
%
%  Compute the pyramid rule.
%
  pyramid_order = legendre_order * legendre_order * jacobi_order;

  volume = 4.0 / 3.0;

  pyramid_w = zeros ( pyramid_order, 1 );
  pyramid_x = zeros ( dim_num, pyramid_order );
  
  l = 0;
  for k = 1 : jacobi_order
    xk = ( jacobi_x(k) + 1.0 ) / 2.0;
    wk = jacobi_w(k) / 2.0;
    for j = 1 : legendre_order
      xj = legendre_x(j);
      wj = legendre_w(j);
      for i = 1 : legendre_order
        xi = legendre_x(i);
        wi = legendre_w(i);
        l = l + 1;
        pyramid_w(l) = wi * wj * wk / 4.0 / volume;
        pyramid_x(1,l) = xi * ( 1.0 - xk );
        pyramid_x(2,l) = xj * ( 1.0 - xk );
        pyramid_x(3,l) =              xk;
      end
    end
  end

  pyramid_r(1:dim_num,1) = [ -1.0, -1.0, 0.0 ]';
  pyramid_r(1:dim_num,2) = [ +1.0, -1.0, 0.0 ]';
  pyramid_r(1:dim_num,3) = [ -1.0, +1.0, 0.0 ]';
  pyramid_r(1:dim_num,4) = [ +1.0, +1.0, 0.0 ]';
  pyramid_r(1:dim_num,5) = [  0.0,  0.0, 1.0 ]';
%
%  Write the rule to files.
%
  filename_w = strcat ( filename, '_w.txt' );
  filename_x = strcat ( filename, '_x.txt' );
  filename_r = strcat ( filename, '_r.txt' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Creating quadrature files.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  "Root" file name is   "%s".\n', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Weight file will be   "%s".\n', filename_w );
  fprintf ( 1, '  Abscissa file will be "%s".\n', filename_x );
  fprintf ( 1, '  Region file will be   "%s".\n', filename_r );

  r8mat_write ( filename_w, 1,       pyramid_order, pyramid_w' );
  r8mat_write ( filename_x, dim_num, pyramid_order, pyramid_x );
  r8mat_write ( filename_r, dim_num, 5,             pyramid_r );

  return
end
function r8mat_write ( output_filename, m, n, table )

%*****************************************************************************80
%
%% r8mat_write() writes an R8MAT file.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 August 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string OUTPUT_FILENAME, the output filename.
%
%    integer M, the spatial dimension.
%
%    integer N, the number of points.
%
%    real TABLE(M,N), the points.
%

%
%  Open the file.
%
  output_unit = fopen ( output_filename, 'wt' );

  if ( output_unit < 0 ) 
    fprintf ( 1, '\n' );
    fprintf ( 1, 'R8MAT_WRITE - Error!\n' );
    fprintf ( 1, '  Could not open the output file.\n' );
    error ( 'R8MAT_WRITE - Error!' );
  end
%
%  Write the data.
%
%  For smaller data files, and less precision, try:
%
%     fprintf ( output_unit, '  %14.6f', table(i,j) );
%
  for j = 1 : n
    for i = 1 : m
      fprintf ( output_unit, '  %24.16f', table(i,j) );
    end
    fprintf ( output_unit, '\n' );
  end
%
%  Close the file.
%
  fclose ( output_unit );

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
%    19 April 2009
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

