function [ value, ierr ] = rf ( x, y, z, errtol )

%*****************************************************************************80
%
%% rf() computes an incomplete elliptic integral of the first kind, RF(X,Y,Z).
%
%  Discussion:
%
%    This function computes the incomplete elliptic integral of the first kind.
%
%    RF(X,Y,Z) = Integral ( 0 <= T < oo )
%
%                    -1/2     -1/2     -1/2
%          (1/2)(T+X)    (T+Y)    (T+Z)    DT,
%
%    where X, Y, and Z are nonnegative and at most one of them is zero.
%
%    If X or Y or Z is zero, the integral is complete.
%
%    The duplication theorem is iterated until the variables are
%    nearly equal, and the function is then expanded in Taylor
%    series to fifth order.
%
%    Check by addition theorem:
%
%      RF(X,X+Z,X+W) + RF(Y,Y+Z,Y+W) = RF(0,Z,W),
%      where X, Y, Z, W are positive and X * Y = Z * W.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 May 2018
%
%  Author:
%
%    Original FORTRAN77 version by Bille Carlson, Elaine Notis.
%    This MATLAB version by John Burkardt.
%
%  Reference:
%
%    Bille Carlson,
%    Computing Elliptic Integrals by Duplication,
%    Numerische Mathematik,
%    Volume 33, 1979, pages 1-16.
%
%    Bille Carlson, Elaine Notis,
%    Algorithm 577, Algorithms for Incomplete Elliptic Integrals,
%    ACM Transactions on Mathematical Software,
%    Volume 7, Number 3, pages 398-403, September 1981.
%
%  Input:
%
%    real X, Y, Z, the arguments in the integral.
%
%    real ERRTOL, the error tolerance.
%    Relative error due to truncation is less than
%      ERRTOL ^ 6 / (4 * (1 - ERRTOL)).
%    Sample choices:
%      ERRTOL   Relative truncation error less than
%      1.E-3    3.E-19
%      3.E-3    2.E-16
%      1.E-2    3.E-13
%      3.E-2    2.E-10
%      1.E-1    3.E-7
%
%  Output:
%
%    real VALUE, the value of the function.
%
%    integer IERR, the error flag.
%    0, no error occurred.
%    1, abnormal termination.
%
%  Local:
%
%    LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
%    LOLIM IS NOT LESS THAN THE MACHINE MINIMUM MULTIPLIED BY 5.
%    UPLIM IS NOT GREATER THAN THE MACHINE MAXIMUM DIVIDED BY 5.
%
  lolim = 3.0E-78;
  uplim = 1.0E+75;

  if ( ...
    x < 0.0E+00 | ...
    y < 0.0E+00 | ...
    z < 0.0E+00 | ...
    x + y < lolim | ...
    x + z < lolim | ...
    y + z < lolim | ...
    uplim <= x | ...
    uplim <= y | ...
    uplim <= z )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'RF - Error!\n' );
    fprintf ( 1, '  Invalid input arguments.\n');
    fprintf ( 1, '  X = %g\n', x );
    fprintf ( 1, '  Y = %g\n', y );
    fprintf ( 1, '  Z = %g\n', z );
    fprintf ( 1, '\n' );
    ierr = 1;
    value = 0.0;
    return
  end

  ierr = 0;
  xn = x;
  yn = y;
  zn = z;

  while ( true )

    mu = ( xn + yn + zn ) / 3.0;
    xndev = 2.0 - ( mu + xn ) / mu;
    yndev = 2.0 - ( mu + yn ) / mu;
    zndev = 2.0 - ( mu + zn ) / mu;
    epslon = max ( abs ( xndev ), ...
      max ( abs ( yndev ), abs ( zndev ) ) );

    if ( epslon < errtol )
      c1 = 1.0 / 24.0;
      c2 = 3.0 / 44.0;
      c3 = 1.0 / 14.0;
      e2 = xndev * yndev - zndev * zndev;
      e3 = xndev * yndev * zndev;
      s = 1.0 + ( c1 * e2 - 0.1 - c2 * e3 ) * e2 + c3 * e3;
      value = s / sqrt ( mu );
      return
    end

    xnroot = sqrt ( xn );
    ynroot = sqrt ( yn );
    znroot = sqrt ( zn );
    lamda = xnroot * ( ynroot + znroot ) + ynroot * znroot;
    xn = ( xn + lamda ) * 0.25;
    yn = ( yn + lamda ) * 0.25;
    zn = ( zn + lamda ) * 0.25;

  end

end
