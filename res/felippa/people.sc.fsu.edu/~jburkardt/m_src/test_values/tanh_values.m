function [ n_data, x, fx ] = tanh_values ( n_data )

%*****************************************************************************80
%
%% tanh_values() returns some values of the hyperbolic tangent function.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      Tanh[x]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 June 2007
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
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 18;

  fx_vec = [ ...
   -0.99990920426259513121, ...
   -0.76159415595576488812, ...
    0.00000000000000000000, ...
    0.099667994624955817118, ...
    0.19737532022490400074, ...
    0.29131261245159090582, ...
    0.37994896225522488527, ...
    0.46211715726000975850, ...
    0.53704956699803528586, ...
    0.60436777711716349631, ...
    0.66403677026784896368, ...
    0.71629787019902442081, ...
    0.76159415595576488812, ...
    0.96402758007581688395, ...
    0.99505475368673045133, ...
    0.99932929973906704379, ...
    0.99990920426259513121, ...
    0.99999999587769276362 ];

  x_vec = [ ...
   -5.0, ...
   -1.0, ...
    0.0, ...
    0.1, ...
    0.2, ...
    0.3, ...
    0.4, ...
    0.5, ...
    0.6, ...
    0.7, ...
    0.8, ...
    0.9, ...
    1.0, ...
    2.0, ...
    3.0, ...
    4.0, ...
    5.0, ...
   10.0 ];

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