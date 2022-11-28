function [ n_data, a, x, fx ] = mittag_leffler_ea_values ( n_data )

%*****************************************************************************80
%
%% mittag_leffler_ea_values() returns values of one-parameter Mittag Leffler function.
%
%  Discussion:
%
%    E(alpha;z) = sum ( 0 <= k < oo ) z^k / Gamma ( alpha * k + 1 )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 February 2017
%
%  Author:
%
%    John Burkardt
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
%    real A, the parameter of the function.
%
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 25;

  a_vec = [ ...
     0.25E+00, ...
     0.25E+00, ...
     0.25E+00, ...
     0.25E+00, ...
     0.25E+00, ...
     1.75E+00, ...
     1.75E+00, ...
     1.75E+00, ...
     1.75E+00, ...
     1.75E+00, ...
     2.25E+00, ...
     2.25E+00, ...
     2.25E+00, ...
     2.25E+00, ...
     2.25E+00, ...
     1.0E+00, ...
     2.0E+00, ...
     3.0E+00, ...
     4.0E+00, ...
     5.0E+00, ...
     6.0E+00, ...
     7.0E+00, ...
     8.0E+00, ...
     9.0E+00, ...
     10.0E+00 ];

  fx_vec = [ ......
      1.0, ...
      0.2525646348870172, ...
      0.1427989464258737, ...
      0.994231091392936E-01, ...
      0.762370352397216E-01, ...
      1.0, ...
     -0.2579027070618285, ...
      0.2716853059670252, ...
      0.1846579916604009E-01, ...
     -0.139707389642194, ...
      1.0, ...
     -0.1022121843823497E+01, ...
      0.1860844522589611E+01, ...
      0.2644615445996891E+01, ...
      0.7762512036620307, ...
      0.6737946999045729E-02, ...
     -0.6172728764571668, ...
      0.2010457248089053, ...
      0.7922864454196143, ...
      0.958340222567225, ...
      0.993055607747429, ...
      0.999007936794713, ...
      0.999875992064687, ...
      0.999986221340384, ...
      0.999998622134038 ];

  x_vec = [ ......
       0.0, ...
      -2.5, ...
      -5.0, ...
      -7.5, ...
     -10.0, ...
       0.0, ... 
     -12.5, ...
     -25.0, ...
     -37.5, ...
     -50.0, ...
       0.0, ... 
     -25.0, ...
     -50.0, ...
     -75.0, ...
    -100.0, ...
      -5.0, ...
      -5.0, ...
      -5.0, ...
      -5.0, ...
      -5.0, ...
      -5.0, ...
      -5.0, ...
      -5.0, ...
      -5.0, ...
      -5.0 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    a = 0.0;
    x = 0.0;
    fx = 0.0;
  else
    a = a_vec(n_data);
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end