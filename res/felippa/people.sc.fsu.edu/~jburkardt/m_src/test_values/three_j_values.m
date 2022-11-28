function [ n_data, j1, j2, j3, m1, m2, m3, fx ] = three_j_values ( n_data )

%*****************************************************************************80
%
%% three_j_values() returns some values of the Wigner 3J function.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      ThreeJSymbol[{j1,m1},{j2,m2},{j3,m3}]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 February 2007
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
%    Thereafter, it should simply be the value returned by the previous call.
%
%  Output:
%
%    integer N_DATA.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    integer N_DATA.  The routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    real  J1, J2, J3, M1, M2, M3, the arguments 
%    of the function.
%
%    real FX, the value of the function.
%
  n_max = 8;

  fx_vec(1:n_max) = [ ...
     0.2788866755113585, ...
    -0.09534625892455923, ...
    -0.06741998624632421, ...
     0.1533110351679666, ...
    -0.1564465546936860, ...
     0.1099450412156551, ...
    -0.05536235693131719, ...
     0.01799835451137786 ];
  j1_vec(1:n_max) = [ ...
    1.0, ...
    2.0, ...
    3.0, ...
    4.0, ...
    5.0, ...
    6.0, ...
    7.0, ...
    8.0 ];
  j2_vec(1:n_max) = [ ...
    4.5, ...
    4.5, ...
    4.5, ...
    4.5, ...
    4.5, ...
    4.5, ...
    4.5, ...
    4.5 ];
  j3_vec(1:n_max) = [ ...
    3.5, ...
    3.5, ...
    3.5, ...
    3.5, ...
    3.5, ...
    3.5, ...
    3.5, ...
    3.5 ];
  m1_vec(1:n_max) = [ ...
    1.0, ...
    1.0, ...
    1.0, ...
    1.0, ...
    1.0, ...
    1.0, ...
    1.0, ...
    1.0 ];
  m2_vec(1:n_max) = [ ...
    -3.5, ...
    -3.5, ...
    -3.5, ...
    -3.5, ...
    -3.5, ...
    -3.5, ...
    -3.5, ...
    -3.5 ];
  m3_vec(1:n_max) = [ ...
    2.5, ...
    2.5, ...
    2.5, ...
    2.5, ...
    2.5, ...
    2.5, ...
    2.5, ...
    2.5 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    j1 = 0.0;
    j2 = 0.0;
    j3 = 0.0;
    m1 = 0.0;
    m2 = 0.0;
    m3 = 0.0;
    fx = 0.0;
  else
    j1 = j1_vec(n_data);
    j2 = j2_vec(n_data);
    j3 = j3_vec(n_data);
    m1 = m1_vec(n_data);
    m2 = m2_vec(n_data);
    m3 = m3_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end