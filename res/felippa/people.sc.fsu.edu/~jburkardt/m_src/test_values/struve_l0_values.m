function [ n_data, x, fx ] = struve_l0_values ( n_data )

%*****************************************************************************80
%
%% struve_l0_values() returns some values of the Struve L0 function.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      StruveL[0,x]
%
%    The data was reported by McLeod.
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
%      special functions,
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
      0.12433985199262820188E-02, ...
     -0.19896526647882937004E-01, ...
      0.79715713253115014945E-01, ...
     -0.32724069939418078025E+00, ...
      0.71024318593789088874E+00, ...
      0.19374337579914456612E+01, ...
     -0.11131050203248583431E+02, ...
      0.16850062034703267148E+03, ...
     -0.28156522493745948555E+04, ...
      0.89344618796978400815E+06, ...
      0.11382025002851451057E+07, ...
     -0.23549701855860190304E+07, ...
      0.43558282527641046718E+08, ...
      0.49993516476037957165E+09, ...
     -0.57745606064408041689E+10, ...
      0.78167229782395624524E+12, ...
     -0.14894774793419899908E+17, ...
      0.29325537838493363267E+21, ...
      0.58940770556098011683E+25, ...
     -0.12015889579125463605E+30 ];

  x_vec = [ ...
       0.0019531250E+00, ...
      -0.0312500000E+00, ...
       0.1250000000E+00, ...
      -0.5000000000E+00, ...
       1.0000000000E+00, ...
       2.0000000000E+00, ...
      -4.0000000000E+00, ...
       7.0000000000E+00, ...
     -10.0000000000E+00, ...
      16.0000000000E+00, ...
      16.2500000000E+00, ...
     -17.0000000000E+00, ...
      20.0000000000E+00, ...
      22.5000000000E+00, ...
     -25.0000000000E+00, ...
      30.0000000000E+00, ...
     -40.0000000000E+00, ...
      50.0000000000E+00, ...
      60.0000000000E+00, ...
     -70.0000000000E+00 ];

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
