function [ n_data, x, fx ] = int_values ( n_data )

%*****************************************************************************80
%
%% int_values() returns some values of the "integer part" function.
%
%  Discussion:
%
%    INT(X) = returns the integer part of a real number.
%
%    The result is returned as a real number.
%
%    The result is computed by rounding the absolute value of the
%    input towards 0, and then restoring the sign.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 October 2011
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
%    Output real FX, the value of the function.
%
  n_max = 25;

  fx_vec = [ ...
     -2.00E+00, ...
     -1.00E+00, ...
     -1.00E+00, ...
     -1.00E+00, ...    
     -1.00E+00, ...    
     -1.00E+00, ...      
      0.00E+00, ...
      0.00E+00, ...    
      0.00E+00, ...
      0.00E+00, ...
      0.00E+00, ...
      0.00E+00, ...
      0.00E+00, ...
      0.00E+00, ...
      0.00E+00, ...
      0.00E+00, ...
      0.00E+00, ...
      0.00E+00, ...
      0.00E+00, ...
      1.00E+00, ...
      1.00E+00, ...
      1.00E+00, ...
      1.00E+00, ...
      1.00E+00, ...
      2.00E+00 ]';

  x_vec = [ ...
     -2.01E+00, ...
     -1.99E+00, ...
     -1.50E+00, ...
     -1.10E+00, ...    
     -1.01E+00, ...    
     -1.00E+00, ...      
     -0.99E+00, ...
     -0.90E+00, ...    
     -0.51E+00, ...
     -0.50E+00, ...
     -0.49E+00, ...
     -0.01E+00, ...
      0.00E+00, ...
      0.01E+00, ...
      0.49E+00, ...
      0.50E+00, ...
      0.51E+00, ...
      0.90E+00, ...
      0.99E+00, ...
      1.00E+00, ...
      1.01E+00, ...
      1.10E+00, ...
      1.50E+00, ...
      1.99E+00, ...
      2.01E+00 ]';

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