function [ n_data, x, fx ] = synch1_values ( n_data )

%*****************************************************************************80
%
%% synch1_values() returns some values of the synchrotron radiation function.
%
%  Discussion:
%
%    The function is defined by:
%
%      SYNCH1(x) = x * Integral ( x <= t < infinity ) K(5/3)(t) dt
%
%    where K(5/3) is a modified Bessel function of order 5/3.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Allan McLeod,
%    Algorithm 757, MISCFUN: A software package to compute uncommon
%    special functions,
%    ACM Transactions on Mathematical Software,
%    Volume 22, Number 3, September 1996, pages 288-301.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Input:
%
%    integer N_DATA.  The user sets N_DATA to 0 before the first call.  
%    Thereafter, it should simply be the value returned by the previous call.
%
%  Output:
%
%    integer N_DATA.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 20;

  fx_vec = [ ...
       0.26514864547487397044E+00, ...
       0.62050129979079045645E+00, ...
       0.85112572132368011206E+00, ...
       0.87081914687546885094E+00, ...
       0.65142281535536396975E+00, ...
       0.45064040920322354579E+00, ...
       0.30163590285073940285E+00, ...
       0.19814490804441305867E+00, ...
       0.12856571000906381300E+00, ...
       0.52827396697866818297E-01, ...
       0.42139298471720305542E-01, ...
       0.21248129774981984268E-01, ...
       0.13400258907505536491E-01, ...
       0.84260797314108699935E-02, ...
       0.12884516186754671469E-02, ...
       0.19223826430086897418E-03, ...
       0.28221070834007689394E-04, ...
       0.15548757973038189372E-05, ...
       0.11968634456097453636E-07, ...
       0.89564246772237127742E-10 ];

  x_vec = [ ...
       0.0019531250E+00, ...
       0.0312500000E+00, ...
       0.1250000000E+00, ...
       0.5000000000E+00, ...
       1.0000000000E+00, ...
       1.5000000000E+00, ...
       2.0000000000E+00, ...
       2.5000000000E+00, ...
       3.0000000000E+00, ...
       4.0000000000E+00, ...
       4.2500000000E+00, ...
       5.0000000000E+00, ...
       5.5000000000E+00, ...
       6.0000000000E+00, ...
       8.0000000000E+00, ...
      10.0000000000E+00, ...
      12.0000000000E+00, ...
      15.0000000000E+00, ...
      20.0000000000E+00, ...
      25.0000000000E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    x = 0.0;
    fx = 0.0;
  else
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end