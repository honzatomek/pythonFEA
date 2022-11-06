function value = zeta_m1 ( p, tol )

%*****************************************************************************80
%
%% zeta_m1() estimates the Riemann Zeta function minus 1.
%
%  Discussion:
%
%    This function includes the Euler-McLaurin correction.
%
%    ZETA_M1 ( P ) = ZETA ( P ) - 1
%
%    ZETA(P) has the form 1 + small terms.  Computing ZETA(P)-1
%    allows for greater accuracy in the small terms.
%
%  Definition:
%
%    For 1 < P, the Riemann Zeta function is defined as:
%
%      ZETA ( P ) = Sum ( 1 <= N < Infinity ) 1 / N^P
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 January 2016
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    William Thompson,
%    Atlas for Computing Mathematical Functions,
%    Wiley, 1997,
%    ISBN: 0471181714,
%    LC: QA331 T385
%
%  Input:
%
%    real P, the power to which the integers are raised.
%    P must be greater than 1.
%
%    real TOL, the requested relative tolerance.
%
%  Output:
%
%    real VALUE, an approximation to the Riemann
%    Zeta function minus 1.
%
  if ( p <= 1.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'ZETA_M1 - Fatal error!\n' );
    fprintf ( 1, '  Exponent P <= 1.0.\n' );
  end
  
  nsterm = p * ( p + 1.0 ) * ( p + 2.0 ) * ( p + 3.0 ) * ( p + 4.0 ) / 30240.0;
    
  base = nsterm * ( 2.0 ^ p ) / tol;
  power = 1.0 / ( p + 5.0 );
  n = floor ( base ^ power );
  
  if ( n < 10 )
    n = 10;
  end
  
  negp = - p;
  value = 0.0;
  
  for k = 2 : n - 1
    value = value + k ^ negp;
  end
%
%  Euler-McLaurin correction.
%  
  value = value + ( n ^ negp ) ...
    * ( 0.5 + n / ( p - 1.0 ) ...
    + p * ( 1.0 - ...
    ( p + 1.0 ) * ( p + 2.0 ) / ( 60 * n * n ) ) ...
    / ( 12 * n ) ...
    + nsterm / ( n ^ ( p + 5.0 ) ) );
       
  return
end
