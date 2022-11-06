function [ n_data, x, fx ] = gamma_log_values ( n_data )

%*****************************************************************************80
%
%% gamma_log_values() returns some values of the Log Gamma function.
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
%    integer N_DATA, indicates the index of the previous test data
%    returned, or is 0 if this is the first call.  For repeated calls,
%    set the input value of N_DATA to the output value of N_DATA
%    from the previous call.
%
%  Output:
%
%    integer N_DATA, the index of the test data.
%
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 18;
  fx_vec = [ ...
     1.524064183E+00,    0.7966780066E+00,   0.3982337117E+00,  ...
     0.1520599127E+00,   0.000000000E+00,   -0.04987246543E+00, ...
    -0.08537410945E+00, -0.1081747934E+00,  -0.1196128950E+00,  ...
    -0.1207822040E+00,  -0.1125917658E+00,  -0.09580771625E+00, ...
    -0.07108385116E+00, -0.03898428380E+00,  0.000000000E+00,   ...
    12.80182743E+00,    39.33988571E+00,    71.25704193E+00 ];
  x_vec = [ ...
    0.2E+00,  0.4E+00,  0.6E+00,  0.8E+00, ...
    1.0E+00,  1.1E+00,  1.2E+00,  1.3E+00, ...
    1.4E+00,  1.5E+00,  1.6E+00,  1.7E+00, ...
    1.8E+00,  1.9E+00,  2.0E+00, 10.0E+00, ...
   20.0E+00, 30.0E+00 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    x = 0.0E+00;
    fx = 0.0E+00;
  else
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
