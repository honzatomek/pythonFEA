function [ n_data, nu, x, fx ] = bessel_kx_values ( n_data )

%*****************************************************************************80
%
%% bessel_kx_values() returns some values of the Kx Bessel function.
%
%  Discussion:
%
%    This set of data considers the less common case in which the
%    index of the Bessel function Kn is actually not an integer.
%    We may suggest this case by occasionally replacing the symbol
%    "Kn" by "Kx".
%
%    The modified Bessel functions In(Z) and Kn(Z) are solutions of
%    the differential equation
%
%      Z^2 W'' + Z * W' - ( Z^2 + N^2 ) * W = 0.
%
%    In Mathematica, the function can be evaluated by:
%
%      BesselK[n,x]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 April 2007
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
%    real NU, the order of the function.
%
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 28;

  fx_vec = [ ...
       2.294489339798475E+00, ...
       0.4610685044478946E+00, ...
       0.1199377719680614E+00, ...
       0.06506594315400999E+00, ...
       0.03602598513176459E+00, ...
       0.003776613374642883E+00, ...
       0.00001799347809370518E+00, ...
       5.776373974707445E-10, ...
       0.9221370088957891E+00, ...
       0.1799066579520922E+00, ...
       0.004531936049571459E+00, ...
       0.00001979282590307570E+00, ...
       3.486992497366216E-23, ...
       3.227479531135262E+00, ...
       0.3897977588961997E+00, ...
       0.006495775004385758E+00, ...
       0.00002393132586462789E+00, ...
       3.627839645299048E-23, ...
       0.7311451879202114E+00, ...
       0.1567475478393932E+00, ...
       0.004257389528177461E+00, ...
       0.00001915541065869563E+00, ...
       3.463337593569306E-23, ...
       4.731184839919541E+00, ...
       0.4976876225514758E+00, ...
       0.007300864610941163E+00, ...
       0.00002546421294106458E+00, ...
       3.675275677913656E-23 ];

  nu_vec = [ ...
    0.50E+00, ...
    0.50E+00, ...
    0.50E+00, ...
    0.50E+00, ...
    0.50E+00, ...
    0.50E+00, ...
    0.50E+00, ...
    0.50E+00, ...
    1.50E+00, ...
    1.50E+00, ...
    1.50E+00, ...
    1.50E+00, ...
    1.50E+00, ...
    2.50E+00, ...
    2.50E+00, ...
    2.50E+00, ...
    2.50E+00, ...
    2.50E+00, ...
    1.25E+00, ...
    1.25E+00, ...
    1.25E+00, ...
    1.25E+00, ...
    1.25E+00, ...
    2.75E+00, ...
    2.75E+00, ...
    2.75E+00, ...
    2.75E+00, ...
    2.75E+00 ];

  x_vec = [ ...
      0.2E+00, ...
      1.0E+00, ...
      2.0E+00, ...
      2.5E+00, ...
      3.0E+00, ...
      5.0E+00, ...
     10.0E+00, ...
     20.0E+00, ...
      1.0E+00, ...
      2.0E+00, ...
      5.0E+00, ...
     10.0E+00, ...
     50.0E+00, ...
      1.0E+00, ...
      2.0E+00, ...
      5.0E+00, ...
     10.0E+00, ...
     50.0E+00, ...
      1.0E+00, ...
      2.0E+00, ...
      5.0E+00, ...
     10.0E+00, ...
     50.0E+00, ...
      1.0E+00, ...
      2.0E+00, ...
      5.0E+00, ...
     10.0E+00, ...
     50.0E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    nu = 0.0;
    x = 0.0;
    fx = 0.0;
  else
    nu = nu_vec(n_data);
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
