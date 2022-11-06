function [ n_data, x, fx ] = ci_values ( n_data )

%*****************************************************************************80
%
%% ci_values() returns some values of the cosine integral function.
%
%  Discussion:
%
%    The cosine integral is defined by
%
%      CI(X) = - integral ( X <= T < Infinity ) ( cos ( T ) ) / T  dT
%
%    In Mathematica, the function can be evaluated by:
%
%      CosIntegral[x]
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
     -0.1777840788066129E+00, ...
     -0.2227070695927976E-01, ...
      0.1005147070088978E+00, ...
      0.1982786159524672E+00, ...
      0.2760678304677729E+00, ...
      0.3374039229009681E+00, ...
      0.4204591828942405E+00, ...
      0.4620065850946773E+00, ...
      0.4717325169318778E+00, ...
      0.4568111294183369E+00, ...
      0.4229808287748650E+00, ...
      0.2858711963653835E+00, ...
      0.1196297860080003E+00, ...
     -0.3212854851248112E-01, ...
     -0.1409816978869304E+00, ...
     -0.1934911221017388E+00 ];

  x_vec = [ ...
      0.5E+00, ...
      0.6E+00, ...
      0.7E+00, ...
      0.8E+00, ...
      0.9E+00, ...
      1.0E+00, ...
      1.2E+00, ...
      1.4E+00, ...
      1.6E+00, ...
      1.8E+00, ...
      2.0E+00, ...
      2.5E+00, ...
      3.0E+00, ...
      3.5E+00, ...
      4.0E+00, ...
      4.5E+00 ];

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
