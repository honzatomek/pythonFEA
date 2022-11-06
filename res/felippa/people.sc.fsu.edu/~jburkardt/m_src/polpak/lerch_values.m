function [ n_data, z, s, a, fx ] = lerch_values ( n_data )

%*****************************************************************************80
%
%% lerch_values() returns some values of the Lerch transcendent function.
%
%  Discussion:
%
%    The Lerch function is defined as
%
%      Phi(z,s,a) = Sum ( 0 <= k < Infinity ) z^k / ( a + k )^s
%
%    omitting any terms with ( a + k ) = 0.
%
%    In Mathematica, the function can be evaluated by:
%
%      LerchPhi[z,s,a]
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
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
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
%    real Z, the parameters of the function.
%
%    integer S, the parameters of the function.
%
%    real A, the parameters of the function.
%
%    real FX, the value of the function.
%
  n_max = 12;

  a_vec = [ ...
     0.0, ...
     0.0, ...
     0.0, ...
     1.0, ...
     1.0, ...
     1.0, ...
     2.0, ...
     2.0, ...
     2.0, ...
     3.0, ...
     3.0, ...
     3.0 ];

  fx_vec = [ ...
     0.1644934066848226E+01, ...
     0.1202056903159594E+01, ...
     0.1000994575127818E+01, ...
     0.1164481052930025E+01, ...
     0.1074426387216080E+01, ...
     0.1000492641212014E+01, ...
     0.2959190697935714E+00, ...
     0.1394507503935608E+00, ...
     0.9823175058446061E-03, ...
     0.1177910993911311E+00, ...
     0.3868447922298962E-01, ...
     0.1703149614186634E-04 ];

  s_vec = [ ...
     2, 3, 10, ...
     2, 3, 10, ...
     2, 3, 10, ...
     2, 3, 10 ];

  z_vec = [ ...
     0.1000000000000000E+01, ...
     0.1000000000000000E+01, ...
     0.1000000000000000E+01, ...
     0.5000000000000000E+00, ...
     0.5000000000000000E+00, ...
     0.5000000000000000E+00, ...
     0.3333333333333333E+00, ...
     0.3333333333333333E+00, ...
     0.3333333333333333E+00, ...
     0.1000000000000000E+00, ...
     0.1000000000000000E+00, ...
     0.1000000000000000E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end
 
  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    z = 0.0;
    s = 0;
    a = 0.0;
    fx = 0.0;
  else
    z = z_vec(n_data);
    s = s_vec(n_data);
    a = a_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
