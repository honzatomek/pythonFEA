function [ n_data, x, fx ] = ei_values ( n_data )

%*****************************************************************************80
%
%% ei_values() returns some values of the exponential integral function EI(X).
%
%  Definition:
%
%    The exponential integral EI(X) has the formula:
%
%      EI(X) = - integral ( -X <= T <= Infinity ) exp ( -T ) / T dT
%
%    In Mathematica, the function can be evaluated by:
%
%      ExpIntegralEi[x]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 September 2004
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
  n_max = 16;

  fx_vec = [ ...
     0.4542199048631736E+00, ...
     0.7698812899373594E+00, ...
     0.1064907194624291E+01, ...
     0.1347396548212326E+01, ...
     0.1622811713696867E+01, ...
     0.1895117816355937E+01, ...
     0.2167378279563403E+01, ...
     0.2442092285192652E+01, ...
     0.2721398880232024E+01, ...
     0.3007207464150646E+01, ...
     0.3301285449129798E+01, ...
     0.3605319949019469E+01, ...
     0.3920963201354904E+01, ...
     0.4249867557487934E+01, ...
     0.4593713686953585E+01, ...
     0.4954234356001890E+01 ];

  x_vec = [ ...
     0.5E+00, ...
     0.6E+00, ...
     0.7E+00, ...
     0.8E+00, ...
     0.9E+00, ...
     1.0E+00, ...
     1.1E+00, ...
     1.2E+00, ...
     1.3E+00, ...
     1.4E+00, ...
     1.5E+00, ...
     1.6E+00, ...
     1.7E+00, ...
     1.8E+00, ...
     1.9E+00, ...
     2.0E+00 ];

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
