function [ n_data, x, fx ] = fresnel_sin_values ( n_data )

%*****************************************************************************80
%
%% fresnel_sin_values() returns some values of the Fresnel sine integral function.
%
%  Discussion:
%
%    The Fresnel sine integral is defined by
%
%      S(X) = integral ( 0 <= T <= X ) sin ( pi * T^2 / 2 ) dT
%
%    In Mathematica, the function can be evaluated by:
%
%      FresnelS[x]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 November 2015
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
     0.0000000000000000E+00, ...
     0.4187609161656762E-02, ...
     0.3335943266061318E-01, ...
     0.1105402073593870E+00, ...
     0.2493413930539178E+00, ...
     0.4382591473903548E+00, ...
     0.6234009185462497E+00, ...
     0.7135250773634121E+00, ...
     0.6388876835093809E+00, ...
     0.4509387692675831E+00, ...
     0.3434156783636982E+00, ...
     0.4557046121246569E+00, ...
     0.6196899649456836E+00, ...
     0.5499893231527195E+00, ...
     0.3915284435431718E+00, ...
     0.4963129989673750E+00 ];

  x_vec = [ ...
     0.0E+00, ...
     0.2E+00, ...
     0.4E+00, ...
     0.6E+00, ...
     0.8E+00, ...
     1.0E+00, ...
     1.2E+00, ...
     1.4E+00, ...
     1.6E+00, ...
     1.8E+00, ...
     2.0E+00, ...
     2.2E+00, ...
     2.4E+00, ...
     2.6E+00, ...
     2.8E+00, ...
     3.0E+00 ];

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
