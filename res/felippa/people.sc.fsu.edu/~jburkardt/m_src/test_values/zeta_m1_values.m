function [ n_data, p, zeta_m1 ] = zeta_m1_values ( n_data )

%*****************************************************************************80
%
%% zeta_m1_values() returns some values of the Riemann Zeta Minus One function.
%
%  Discussion:
%
%    ZETA_M1(N) = ZETA(N) - 1
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 January 2016
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz, Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
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
%    real P, the argument.
%
%    real ZETA_M1, the value.
%
  n_max = 17;

  p_vec = [ ...
     2.0, ...
     2.5, ...
     3.0, ...
     3.5, ...
     4.0, ...
     5.0, ...
     6.0, ...
     7.0, ...
     8.0, ...
     9.0, ...
    10.0, ...
    11.0, ...
    12.0, ...
    16.0, ...
    20.0, ...
    30.0, ...
    40.0 ];

  zeta_m1_vec = [ ...
     0.64493406684822643647E+00, ...
     0.3414872573E+00, ...
     0.20205690315959428540E+00, ...
     0.1267338673E+00, ...
     0.8232323371113819152E-01, ...
     0.3692775514336992633E-01, ...
     0.1734306198444913971E-01, ...
     0.834927738192282684E-02, ...
     0.407735619794433939E-02, ...
     0.200839292608221442E-02, ...
     0.99457512781808534E-03, ...
     0.49418860411946456E-03, ...
     0.24608655330804830E-03, ...
     0.1528225940865187E-04, ...
     0.95396203387280E-06, ...
     0.93132743242E-10, ...
     0.90949478E-12 ]; 

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    p = 0.0;
    zeta_m1 = 0.0;
  else
    p = p_vec(n_data);
    zeta_m1 = zeta_m1_vec(n_data);
  end

  return
end

