function [ n_data, x, fx ] = cinh_values ( n_data )

%*****************************************************************************80
%
%% cinh_values() returns some values of the alternate hyperbolic cosine integral function.
%
%  Discussion:
%
%    The alternate hyperbolic cosine integral is defined by
%
%      CINH(X) =integral ( 0 <= T < X ) ( cosh ( T ) - 1 ) / T  dT
%
%    In Mathematica, the function can be evaluated by:
%
%      Integrate [ ( Cosh[t] - 1 ) / t, { t, 0, x } ]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 March 2010
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
  n_max = 17;

  fx_vec = [ ...
     0.00000000000000000, ...
     0.06315467070191883, ...
     0.09136085223843649, ...
     0.1250284547325902, ...
     0.1643278712460683, ...
     0.2094587379417273, ...
     0.2606512760786754, ...
     0.3823047024751071, ...
     0.5318061742668980, ...
     0.7122865135136963, ...
     0.9275748842583805, ...
     1.182304077185436, ...
     2.030919091578478, ...
     3.284564141195967, ...
     5.129213294250493, ...
     7.850037532801762, ...
    11.88451858691463 ];

  x_vec = [ ...
     0.0, ...
     0.5, ...
     0.6, ...
     0.7, ...
     0.8, ...
     0.9, ...
     1.0, ...
     1.2, ...
     1.4, ...
     1.6, ...
     1.8, ...
     2.0, ...
     2.5, ...
     3.0, ...
     3.5, ...
     4.0, ...  
     4.5 ];

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
