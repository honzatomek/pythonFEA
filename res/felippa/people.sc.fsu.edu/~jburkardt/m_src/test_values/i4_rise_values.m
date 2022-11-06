function [ n_data, m, n, fmn ] = i4_rise_values ( n_data )

%*****************************************************************************80
%
%% i4_rise_values() returns values of the integer rising factorial function.
%
%  Discussion:
%
%    The integer rising factorial function is sometimes symbolized by (m)_n.
%
%    The definition is
%
%      (m)_n = (m-1+n)! / (m-1)!
%            = ( m ) * ( m + 1 ) * ( m + 2 ) ... * ( m - 1 + n )
%            = Gamma ( m + n ) / Gamma ( m )
%
%    We assume 0 <= N <= M.
%
%    In Mathematica, the function can be evaluated by:
%
%      Pochhammer[m,n]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 December 2014
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
%    integer M, N, the arguments of the function.
%
%    integer FMN, the value of the function.
%
  n_max = 8;

  fmn_vec = [ ...
     1, 5, 30, 210, 1680, ...
     15120, 151200, 1, 10, 4000, ...
     110, 6840, 840, 970200, 5040 ];

  m_vec = [ ...
    5, 5, 5, 5, 5, ...
    5, 5, 50, 10, 4000, ...
    10, 18, 4, 98, 1 ];

  n_vec = [ ...
    0, 1, 2, 3, 4, ...
    5, 6, 0, 1, 1, ...
    2, 3, 4, 3, 7 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    m = 0;
    n = 0;
    fmn = 0;
  else
    m = m_vec(n_data);
    n = n_vec(n_data);
    fmn = fmn_vec(n_data);
  end

  return
end
