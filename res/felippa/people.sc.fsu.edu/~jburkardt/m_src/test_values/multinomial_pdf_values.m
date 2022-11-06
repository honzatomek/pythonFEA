function [ n_data, n, p, x, pdf ] = multinomial_pdf_values ( n_data, m )

%*****************************************************************************80
%
%% multinomial_pdf_values() returns some values of the multinomial PDF.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 July 2015
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
%    Input, integer M, the number of outcomes.
%
%    integer N, the number of trials.
%
%    real P(M), the probability of each outcome on 
%    one trial.
%
%    integer X(M), the number of times each outcome 
%    occurred in N trials.
%
%    real PDF, the probability of X.
%
  n_max = 10;

  n_vec = [ ...
     3, 4, 3, 3, 3, ...
     3, 3, 3, 3, 3 ];
  offset = [ ...
     0,   2,  4,  6,  9, ...
     14, 19, 24, 29, 34 ];
  p_vec = [ ...
    0.7, 0.3, ...
    0.7, 0.3, ...
    0.5, 0.5, ...
    0.6, 0.0, 0.4, ...
    0.6, 0.1, 0.1, 0.1, 0.1, ...
    0.6, 0.1, 0.1, 0.1, 0.1, ...
    0.6, 0.1, 0.1, 0.1, 0.1, ...
    0.6, 0.1, 0.1, 0.1, 0.1, ...
    0.6, 0.1, 0.1, 0.1, 0.1, ...
    0.6, 0.1, 0.1, 0.1, 0.1 ];
  pdf_vec = [ ...
    0.441, ...
    0.2646, ...
    0.375, ...
    0.0, ...
    0.216, ...
    0.108, ...
    0.018, ...
    0.036, ...
    0.001, ...
    0.006 ];
  x_vec = [ ...
    2, 1, ...
    2, 2, ...
    2, 1, ...
    1, 1, 1, ...
    3, 0, 0, 0, 0, ...
    2, 1, 0, 0, 0, ...
    1, 0, 2, 0, 0, ...
    1, 0, 0, 1, 1, ...
    0, 0, 0, 3, 0, ...
    0, 1, 1, 1, 0 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    n = 0;
    p = [];
    x = [];
    pdf = 0.0;
  else
    n = n_vec(n_data);
    p = zeros ( m, 1 );
    for i = 1 : m
      p(i) = p_vec(i+offset(n_data));
    end
    x = zeros ( m, 1 );
    for i = 1 : m
      x(i) = x_vec(i+offset(n_data));
    end
    pdf = pdf_vec(n_data);
  end

  return
end
