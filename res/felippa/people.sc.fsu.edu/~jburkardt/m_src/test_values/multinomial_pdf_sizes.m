function [ n_data, m ] = multinomial_pdf_sizes ( n_data )

%*****************************************************************************80
%
%% multinomial_pdf_sizes() returns sizes of some multinomial PDF data.
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
%    integer M, the size of the given problem.
%
  n_max = 10;

  m_vec = [ ...
     2, 2, 2, 3, 5, ...
     5, 5, 5, 5, 5 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    m = 0;
  else
    m = m_vec(n_data);
  end

  return
end

