function [ n_data, n, x, fx ] = hermite_poly_phys_values ( n_data )

%*****************************************************************************80
%
%% hermite_poly_phys_values() returns some values of the Hermite physicist polynomial.
%
%  Differential equation:
%
%    Y'' - 2 X Y' + 2 N Y = 0
%
%  First terms:
%
%      1
%      2 X
%      4 X^2     -  2
%      8 X^3     - 12 X
%     16 X^4     - 48 X^2     + 12
%     32 X^5    - 160 X^3    + 120 X
%     64 X^6    - 480 X^4    + 720 X^2    - 120
%    128 X^7   - 1344 X^5   + 3360 X^3   - 1680 X
%    256 X^8   - 3584 X^6  + 13440 X^4  - 13440 X^2   + 1680
%    512 X^9   - 9216 X^7  + 48384 X^5  - 80640 X^3  + 30240 X
%   1024 X^10 - 23040 X^8 + 161280 X^6 - 403200 X^4 + 302400 X^2 - 30240
%
%  Recursion:
%
%    H(0,X) = 1,
%    H(1,X) = 2*X,
%    H(N,X) = 2*X * H(N-1,X) - 2*(N-1) * H(N-2,X)
%
%  Norm:
%
%    Integral ( -Infinity < X < Infinity ) exp ( - X^2 ) * H(N,X)^2 dX
%    = sqrt ( PI ) * 2^N * N!
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 May 2004
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
%    integer N_DATA, the index of the test data.
%
%    integer N, the order of the polynomial.
%
%    real X, the point where the polynomial is evaluated.
%
%    real FX, the value of the function.
%
  n_max = 17;

  fx_vec = [ ...
     1.0E+00,            10.0E+00,           98.0E+00, ...
     940.0E+00,          8812.0E+00,         80600.0E+00, ...
     717880.0E+00,       6211600.0E+00,      520656800.0E+00, ...
     421271200E+00,      3275529760.0E+00,   24329873600.0E+00, ...
     171237081280.0E+00, 41.0E+00,          -8.0E+00, ...
     3816.0E+00,         3041200.0E+00 ];

  n_vec = [ ...
     0,  1,  2, ...
     3,  4,  5, ...
     6,  7,  8, ...
     9, 10, 11, ...
    12,  5,  5, ...
     5,  5 ];

  x_vec = [ ...
    5.0E+00,  5.0E+00,  5.0E+00, ...
    5.0E+00,  5.0E+00,  5.0E+00, ...
    5.0E+00,  5.0E+00,  5.0E+00, ...
    5.0E+00,  5.0E+00,  5.0E+00, ...
    5.0E+00,  0.5E+00,  1.0E+00, ...
    3.0E+00, 10.0E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    n = 0;
    x = 0.0E+00;
    fx = 0.0E+00;
  else
    n = n_vec(n_data);
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
