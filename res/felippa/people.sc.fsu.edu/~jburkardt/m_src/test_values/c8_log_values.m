function [ n_data, x, fx ] = c8_log_values ( n_data )

%*****************************************************************************80
%
%% c8_log_values() returns some values of natural logarithm function for complex values.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    03 January 2018
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    David Collens,
%    Algorithm 243: Logarithm of a Complex Number,
%    Communications of the Association for Computing Machinery,
%    Volume 7, Number 11, November 1964, page 660.
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
%    complex X, the argument of the function.
%
%    complex FX, the value of the function.
%
  n_max = 12;

  fx_vec = [ ...
  1.039720770839918 - 2.356194490192345i;
  0.804718956217050 + 2.677945044588987i;
  0.346573590279973 - 2.356194490192345i;
  0.000000000000000 + 3.141592653589793i;
  0.693147180559945 - 1.570796326794897i;
  0.000000000000000 - 1.570796326794897i;
  0.000000000000000 + 1.570796326794897i;
  0.693147180559945 + 1.570796326794897i;
  0.346573590279973 - 0.785398163397448i;
  0.000000000000000 + 0.000000000000000i;
  1.039720770839918 - 0.785398163397448i;
  0.804718956217050 + 0.463647609000806i ];

  x_vec = [ ...
    -2.0 - 2.0i;
    -2.0 + 1.0i;
    -1.0 - 1.0i;
    -1.0 + 0.0i;
     0.0 - 2.0i;
     0.0 - 1.0i;
     0.0 + 1.0i;
     0.0 + 2.0i;
     1.0 - 1.0i;
     1.0 + 0.0i;
     2.0 - 2.0i;
     2.0 + 1.0i ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    x = 0.0 + 0.0i;
    fx = 0.0 + 0.0i;
  else
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
