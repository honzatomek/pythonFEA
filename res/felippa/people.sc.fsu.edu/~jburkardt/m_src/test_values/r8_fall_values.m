function [ n_data, x, n, f ] = r8_fall_values ( n_data )

%*****************************************************************************80
%
%% r8_fall_values() returns some values of the falling factorial function.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      FactorialPower[X,Y]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 December 2014
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
%    real X, integer N, the arguments of the function.
%
%    real F, the value of the function.
%
  n_max = 15;

  f_vec = [ ...
       120.0000000000000,  ...
        163.1601562500000, ...
        216.5625000000000, ...
        281.6601562500000, ...
        360.0000000000000, ...
        1.000000000000000, ...
        7.500000000000000, ...
        48.75000000000000, ...
        268.1250000000000, ...
        1206.562500000000, ...
        4222.968750000000, ...
        10557.42187500000, ...
        15836.13281250000, ...
        7918.066406250000, ...
        -3959.03320312500 ];

  n_vec = [ ...
        4, ...
        4, ...
        4, ...
        4, ...
        4, ...
        0, ...
        1, ...
        2, ...
        3, ...
        4, ...
        5, ...
        6, ...
        7, ...
        8, ...
        9 ]; 

  x_vec = [ ...
        5.00, ...
        5.25, ...
        5.50, ...
        5.75, ...
        6.00, ...
        7.50, ...
        7.50, ...
        7.50, ...
        7.50, ...
        7.50, ...
        7.50, ...
        7.50, ...
        7.50, ...
        7.50, ...
        7.50  ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    x = 0.0;
    n = 0;
    f = 0.0;
  else
    x = x_vec(n_data);
    n = n_vec(n_data);
    f = f_vec(n_data);
  end

  return
end