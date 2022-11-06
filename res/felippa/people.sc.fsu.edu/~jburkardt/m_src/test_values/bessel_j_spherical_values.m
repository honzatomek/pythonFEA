function [ n_data, n, x, fx ] = bessel_j_spherical_values ( n_data )

%*****************************************************************************80
%
%% bessel_j_spherical_values() returns values of the Spherical Bessel function j.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      Sqrt[Pi/(2*x)] * BesselJ[n+1/2,x]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 January 2016
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz, Irene Stegun,
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
%    integer N, the index of the function.
%
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 22;

  fx_vec = [ ...
     0.8689780717709105E+00, ...
     0.2776712616989048E+00, ...
     0.05147914933043151E+00, ...
     0.006743927971987495E+00, ...
     0.0006838294584220406E+00, ...
     0.00005658597917091951E+00, ...
     3.955923765931341E-06, ...
     2.394450910776484E-07, ...
     1.277940110150618E-08, ...
     6.099572379372921E-10, ...
     2.633096568558721E-11, ...
    -0.05440211108893698E+00, ...
     0.07846694179875155E+00, ...
     0.07794219362856245E+00, ...
    -0.03949584498447032E+00, ...
    -0.1055892851176917E+00, ...
    -0.05553451162145218E+00, ...
     0.04450132233409427E+00, ...
     0.1133862306557747E+00, ...
     0.1255780236495678E+00, ...
     0.1000964095484906E+00, ...
     0.06460515449256426E+00 ];

  n_vec = [ ...
     0, ...
     1, ...
     2, ...
     3, ...
     4, ...
     5, ...
     6, ...
     7, ...
     8, ...
     9, ...
    10, ...
     0, ...
     1, ...
     2, ...
     3, ...
     4, ...
     5, ...
     6, ...
     7, ...
     8, ...
     9, ...
    10 ];

  x_vec = [ ...
     0.9050000000000000E+00, ...
     0.9050000000000000E+00, ...
     0.9050000000000000E+00, ...
     0.9050000000000000E+00, ...
     0.9050000000000000E+00, ...
     0.9050000000000000E+00, ...
     0.9050000000000000E+00, ...
     0.9050000000000000E+00, ...
     0.9050000000000000E+00, ...
     0.9050000000000000E+00, ...
     0.9050000000000000E+00, ...
    10.00000000000000E+00, ...
    10.00000000000000E+00, ...
    10.00000000000000E+00, ...
    10.00000000000000E+00, ...
    10.00000000000000E+00, ...
    10.00000000000000E+00, ...
    10.00000000000000E+00, ...
    10.00000000000000E+00, ...
    10.00000000000000E+00, ...
    10.00000000000000E+00, ...
    10.00000000000000E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    n = 0;
    x = 0.0;
    fx = 0.0;
  else
    n = n_vec(n_data);
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
