function [ n_data, u, a, k, m, fx ] = jacobi_sn_values ( n_data )

%*****************************************************************************80
%
%% jacobi_sn_values() returns some values of the Jacobi elliptic function SN(U,M).
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      JacobiSN[ u, m ]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 November 2020
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
%    real U, the argument of the function.
%
%    real A, K, M, the parameter of the function.
%    K = sin ( A ), M = K^2.
%
%    real FX, the value of the function.
%
  n_max = 20;

  m_vec = [ ...
     0.0E+00, ...
     0.0E+00, ...
     0.0E+00, ...
     0.0E+00, ...
     0.0E+00, ...
     0.5E+00, ...
     0.5E+00, ...
     0.5E+00, ...
     0.5E+00, ...
     0.5E+00, ...
     1.0E+00, ...
     1.0E+00, ...
     1.0E+00, ...
     1.0E+00, ...
     1.0E+00, ...
     1.0E+00, ...
     1.0E+00, ...
     1.0E+00, ...
     1.0E+00, ...
     1.0E+00 ];

  fx_vec = [ ...
      0.9983341664682815E-01, ...
      0.1986693307950612E+00, ...
      0.4794255386042030E+00, ...
      0.8414709848078965E+00, ...
      0.9092974268256817E+00, ...
      0.9975068547462484E-01, ...
      0.1980217429819704E+00, ...
      0.4707504736556573E+00, ...
      0.8030018248956439E+00, ...
      0.9946623253580177E+00, ...
      0.9966799462495582E-01, ...
      0.1973753202249040E+00, ...
      0.4621171572600098E+00, ...
      0.7615941559557649E+00, ...
      0.9640275800758169E+00, ...
      0.9993292997390670E+00, ...
     -0.1973753202249040E+00, ...
     -0.4621171572600098E+00, ...
     -0.7615941559557649E+00, ...
     -0.9640275800758169E+00  ];

  u_vec = [ ...
      0.1E+00, ...
      0.2E+00, ... 
      0.5E+00, ...
      1.0E+00, ...
      2.0E+00, ...
      0.1E+00, ...
      0.2E+00, ...
      0.5E+00, ...
      1.0E+00, ...
      2.0E+00, ...
      0.1E+00, ...
      0.2E+00, ...
      0.5E+00, ...
      1.0E+00, ...
      2.0E+00, ...
      4.0E+00, ...
     -0.2E+00, ...
     -0.5E+00, ...
     -1.0E+00, ...
     -2.0E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    a = 0.0;
    k = 0.0;
    m = 0.0;
    u = 0.0;
    fx = 0.0;
  else
    m = m_vec(n_data);
    k = sqrt ( m );
    a = asin ( k );
    u = u_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
 
