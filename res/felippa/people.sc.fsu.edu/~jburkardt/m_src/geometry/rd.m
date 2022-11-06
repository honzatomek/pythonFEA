function [ value, ierr ] = rd ( x, y, z, errtol )

%*****************************************************************************80
%
%% rd() computes an incomplete elliptic integral of the second kind, RD(X,Y,Z).
%
%  Discussion:
%
%    This function computes an incomplete elliptic integral of the second kind.
%
%    RD(X,Y,Z) = Integral ( 0 <= T < oo )
%
%                    -1/2     -1/2     -3/2
%          (3/2)(T+X)    (T+Y)    (T+Z)    DT,
%
%    where X and Y are nonnegative, X + Y is positive, and Z is positive.
%
%    If X or Y is zero, the integral is complete.
%
%    The duplication theorem is iterated until the variables are
%    nearly equal, and the function is then expanded in Taylor
%    series to fifth order.
%
%    Check:
%
%      RD(X,Y,Z) + RD(Y,Z,X) + RD(Z,X,Y) = 3 / sqrt ( X * Y * Z ),
%      where X, Y, and Z are positive.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 May 2018
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
%    Input, real X, Y, Z, the arguments in the integral.
%
%    Input, real ERRTOL, the error tolerance.
%    The relative error due to truncation is less than
%      3 * ERRTOL ^ 6 / (1-ERRTOL) ^ 3/2.
%    Sample choices:
%      ERRTOL   Relative truncation error less than
%      1.E-3    4.E-18
%      3.E-3    3.E-15
%      1.E-2    4.E-12
%      3.E-2    3.E-9
%      1.E-1    4.E-6
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
%    LOLIM IS NOT LESS THAN 2 / (MACHINE MAXIMUM) ^ (2/3).
%    UPLIM IS NOT GREATER THAN (0.1 * ERRTOL / MACHINE
%    MINIMUM) ^ (2/3), WHERE ERRTOL IS DESCRIBED BELOW.
%    IN THE FOLLOWING TABLE IT IS ASSUMED THAT ERRTOL WILL
%    NEVER BE CHOSEN SMALLER THAN 1.E-5.
%
  lolim = 6.E-51;
  uplim = 1.0E+48;

  if ( ...
    x < 0.0 | ...
    y < 0.0 | ...
    x + y < lolim | ...
    z < lolim | ...
    uplim < x | ...
    uplim < y | ...
    uplim < z )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'RD - Error!\n' );
    fprintf ( 1, '  Invalid input arguments.\n' );
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
  sigma = 0.0;
  power4 = 1.0;

  while ( true )

    mu = ( xn + yn + 3.0 * zn ) * 0.2;
    xndev = ( mu - xn ) / mu;
    yndev = ( mu - yn ) / mu;
    zndev = ( mu - zn ) / mu;
    epslon = max ( abs ( xndev ), max ( abs ( yndev ), abs ( zndev ) ) );

    if ( epslon < errtol )
      c1 = 3.0 / 14.0;
      c2 = 1.0 / 6.0;
      c3 = 9.0 / 22.0;
      c4 = 3.0 / 26.0;
      ea = xndev * yndev;
      eb = zndev * zndev;
      ec = ea - eb;
      ed = ea - 6.0 * eb;
      ef = ed + ec + ec;
      s1 = ed * ( - c1 + 0.25 * c3 * ed - 1.5 * c4 * zndev * ef );
      s2 = zndev  * ( c2 * ef + zndev * ( - c3 * ec + zndev * c4 * ea ) );
      value = 3.0 * sigma  + power4 * ( 1.0 + s1 + s2 ) / ( mu * sqrt ( mu ) );

      return
    end

    xnroot = sqrt ( xn );
    ynroot = sqrt ( yn );
    znroot = sqrt ( zn );
    lamda = xnroot * ( ynroot + znroot ) + ynroot * znroot;
    sigma = sigma + power4 / ( znroot * ( zn + lamda ) );
    power4 = power4 * 0.25;
    xn = ( xn + lamda ) * 0.25;
    yn = ( yn + lamda ) * 0.25;
    zn = ( zn + lamda ) * 0.25;

  end

end
