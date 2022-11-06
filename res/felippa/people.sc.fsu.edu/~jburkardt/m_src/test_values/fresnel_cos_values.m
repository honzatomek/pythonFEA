function [ n_data, x, fx ] = fresnel_cos_values ( n_data )

%*****************************************************************************80
%
%% fresnel_cos_values() returns values of the Fresnel cosine integral function.
%
%  Discussion:
%
%    The Fresnel cosine integral is defined by:
%
%      C(X) = integral ( 0 <= T <= X ) cos ( PI * T^2 / 2 ) dT
%
%    In Mathematica, the function can be evaluated by:
%
%      FresnelC[x]
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
     0.1999210575944531E+00, ...
     0.3974807591723594E+00, ...
     0.5810954469916523E+00, ...
     0.7228441718963561E+00, ...
     0.7798934003768228E+00, ...
     0.7154377229230734E+00, ...
     0.5430957835462564E+00, ...
     0.3654616834404877E+00, ...
     0.3336329272215571E+00, ...
     0.4882534060753408E+00, ...
     0.6362860449033195E+00, ...
     0.5549614058564281E+00, ...
     0.3889374961919690E+00, ...
     0.4674916516989059E+00, ...
     0.6057207892976856E+00 ];

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
