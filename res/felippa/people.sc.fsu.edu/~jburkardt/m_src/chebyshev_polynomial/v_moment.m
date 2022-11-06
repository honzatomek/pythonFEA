function value = v_moment ( e )

%*****************************************************************************80
%
%% v_moment(): integral ( -1 <= x <= +1 ) x^e sqrt(1+x) / sqrt(1-x) dx.
%
%  Discussion:
%
%    This function returns the moments of the distribution associated
%    with the Chebyshev V polynomial.
%
%     E  V_MOMENT
%    --  -------------
%     0      pi
%     1      pi / 2
%     2      pi / 2
%     3    3 pi / 8
%     4    3 pi / 8
%     5    5 pi / 16
%     6    5 pi / 16
%     7   35 pi / 128
%     8   35 pi / 128
%     9   63 pi / 256
%    10   63 pi / 256
%    11  231 pi / 1024
%    12  231 pi / 1024
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer E, the exponent of X.
%    0 <= E.
%
%  Output:
%
%    real VALUE, the value of the integral.
%
  f1 = 1.0 / gamma ( 1.5 + e );
  f2 = r8_mop ( e );
  f3 = pi * gamma ( 1.5 + e );
  f4 = 2.0 * r8_hyper_2f1 ( 0.5, -e, 1.0, 2.0 );
  f5 = ( -1.0 + r8_mop ( e ) ) * r8_hyper_2f1 ( 0.5, -e, 2.0, 2.0 );
  f6 = sqrt ( pi ) * factorial ( e );
  f7 = ( -1.0 + r8_mop ( e ) ) * r8_hyper_2f1 ( -0.5, 1.0 + e, 1.5 + e, - 1.0 );
  f8 = 2.0 * r8_hyper_2f1 ( 0.5, 1.0 + e, 1.5 + e, -1.0 );

  value = f1 * f2 * ( f3 * ( f4 + f5 ) - f6 * ( f7 + f8 ) );

  return
end
