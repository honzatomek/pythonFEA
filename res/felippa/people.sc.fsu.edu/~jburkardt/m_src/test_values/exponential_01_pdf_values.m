function [ n_data, x, fx ] = exponential_01_pdf_values ( n_data )

%*****************************************************************************80
%
%% exponential_01_pdf_values() returns some values of the standard exponential PDF.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 April 2016
%
%  Author:
%
%    John Burkardt
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
  n_max = 10;

  fx_vec = [ ...
    0.4959398481993681, ...
    0.00856777959135697, ...
    0.01720937842266235, ...
    0.07507070056996956, ...
    0.1679332083261492, ...
    0.0, ...
    0.399845179478639, ...
    0.9005384971416223, ...
    0.0, ...
    0.05044803826563792 ];

  x_vec = [ ...
        0.7013006334030669, ...
         4.759746670799113, ...
         4.062300786629853, ...
         2.589324935217918, ...
         1.784188948117787, ...
       -0.1363469579618277, ...
        0.9166778581012469, ...
        0.1047623644285883, ...
       -0.2589405122149109, ...
         2.986811417663269 ];

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
