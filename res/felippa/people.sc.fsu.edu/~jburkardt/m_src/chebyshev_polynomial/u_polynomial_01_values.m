function [ n_data, n, x, fx ] = u_polynomial_01_values ( n_data )

%*****************************************************************************80
%
%% u_polynomial_01_values(): values of shifted Chebyshev polynomials U01(n,x).
%
%  Discussion:
%
%    U01(n,x) = U(n,2*x-1)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 July 2015
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
%
%  Output:
%
%    integer N_DATA.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    integer N, the order of the function.
%
%    real X, the point where the function is evaluated.
%
%    real FX, the value of the function.
%
  n_max = 25;

  fx_vec = [ ...
     0.000000000000000, ...
     1.000000000000000, ...
     1.400000000000000, ...
     0.9600000000000000, ...
    -0.05600000000000000, ...
    -1.038400000000000, ...
    -1.397760000000000, ...
    -0.9184640000000000, ...
     0.1119104000000000, ...
     1.075138560000000, ...
     1.393283584000000, ...
     0.8754584576000000, ...
    -0.1676417433600000, ...
    -1.110156898304000, ...
    -8.000000000000000, ...
     1.511014400000000, ...
    -1.133260800000000, ...
    -0.1636352000000000, ...
     1.019801600000000, ...
     0.000000000000000, ...
    -1.019801600000000, ...
     0.1636352000000000, ...
     1.133260800000000, ...
    -1.511014400000000, ...
     8.000000000000000  ];

  n_vec = [ ...
    -1, ...
     0,  1,  2, ...
     3,  4,  5, ...
     6,  7,  8, ...
     9, 10, 11, ...
    12,  7,  7, ...
     7,  7,  7, ...
     7,  7,  7, ...
     7,  7,  7 ];

  x_vec = [ ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.85, ...
    0.00, ...
    0.10, ...
    0.20, ...
    0.30, ...
    0.40, ...
    0.50, ...
    0.60, ...
    0.70, ...
    0.80, ...
    0.90, ...
    1.00 ];

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
