function [ n_data, n, m, x, fx ] = legendre_associated_normalized_sphere_values ...
  ( n_data )

%*****************************************************************************80
%
%% legendre_associated_normalized_sphere_values(): normalized associated Legendre.
%
%  Discussion:
%
%    The function considered is the associated Legendre polynomial P^M_N(X).
%
%    In Mathematica, the function can be evaluated by:
%
%      LegendreP [ n, m, x ]
%
%    The function is normalized for the unit sphere by dividing by
%
%      sqrt ( 4 * pi * ( n + m )! / ( 2 * n + 1 ) / ( n - m )! )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 March 2012
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz, Irene Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964,
%    ISBN: 0-486-61272-4,
%    LC: QA47.A34.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Cambridge University Press, 1999,
%    ISBN: 0-521-64314-7,
%    LC: QA76.95.W65.
%
%  Input:
%
%    integer N_DATA.  The user sets N_DATA to 0 before the first call.
%
%  Output:
%
%    integer N_DATA.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    integer N, integer M, real X,
%    the arguments of the function.
%
%    real FX, the value of the function.
%
  n_max = 21;

  fx_vec = [ ...
     0.2820947917738781, ...
     0.2443012559514600, ...
    -0.2992067103010745, ...
    -0.07884789131313000, ...
    -0.3345232717786446, ...
     0.2897056515173922, ...
    -0.3265292910163510, ...
    -0.06997056236064664, ...
     0.3832445536624809, ...
    -0.2709948227475519, ...
    -0.2446290772414100, ...
     0.2560660384200185, ...
     0.1881693403754876, ...
    -0.4064922341213279, ...
     0.2489246395003027, ...
     0.08405804426339821, ...
     0.3293793022891428, ...
    -0.1588847984307093, ...
    -0.2808712959945307, ...
     0.4127948151484925, ...
    -0.2260970318780046 ]';
  m_vec = [ ...
    0, 0, 1, 0, ...
    1, 2, 0, 1, ...
    2, 3, 0, 1, ...
    2, 3, 4, 0, ...
    1, 2, 3, 4, ...
    5 ]';
  n_vec = [ ...
    0,  1,  1,  2, ...
    2,  2,  3,  3, ...
    3,  3,  4,  4, ...
    4,  4,  4,  5, ...
    5,  5,  5,  5, ...
    5 ]';
  x_vec = [ ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50, ...
    0.50 ]';

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    n = 0;
    m = 0;
    x = 0.0;
    fx = 0.0;
  else
    n = n_vec(n_data);
    m = m_vec(n_data);
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end