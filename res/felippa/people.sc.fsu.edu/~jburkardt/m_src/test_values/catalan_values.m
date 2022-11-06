function [ n_data, n, c ] = catalan_values ( n_data )

%*****************************************************************************80
%
%% catalan_values() returns some values of the Catalan numbers.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      Binomial[2*n,n] / ( n + 1 )
%
%  First values:
%
%     C(0)     1
%     C(1)     1
%     C(2)     2
%     C(3)     5
%     C(4)    14
%     C(5)    42
%     C(6)   132
%     C(7)   429
%     C(8)  1430
%     C(9)  4862
%    C(10) 16796
%
%  Formula:
%
%    C(N) = (2*N)! / ( (N+1) * (N!) * (N!) ) 
%         = 1 / (N+1) * COMB ( 2N, N )
%         = 1 / (2N+1) * COMB ( 2N+1, N+1).
%
%  Recursion:
%
%    C(N) = 2 * (2*N-1) * C(N-1) / (N+1)
%    C(N) = sum ( 1 <= I <= N-1 ) C(I) * C(N-I)
%
%  Discussion:
%
%    The Catalan number C(N) counts:
%
%    1) the number of binary trees on N vertices;
%    2) the number of ordered trees on N+1 vertices;
%    3) the number of full binary trees on 2N+1 vertices;
%    4) the number of well formed sequences of 2N parentheses;
%    5) the number of ways 2N ballots can be counted, in order,
%       with N positive and N negative, so that the running sum
%       is never negative;
%    6) the number of standard tableaus in a 2 by N rectangular Ferrers diagram;
%    7) the number of monotone functions from [1..N} to [1..N} which 
%       satisfy f(i) <= i for all i;
%    8) the number of ways to triangulate a polygon with N+2 vertices.
%
%  Example:
%
%    N = 3
%
%    ()()()
%    ()(())
%    (()())
%    (())()
%    ((()))
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 September 2004
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
%    integer N, the order of the Catalan number.
%
%    integer C, the value of the Catalan number.
%
  n_max = 11;

  c_vec = [ ...
    1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796 ];

  n_vec = [ ...
     0,  1,  2,  3,  4, 5,  6,  7,  8,  9,  10 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    n = 0;
    c = 0;
  else
    n = n_vec(n_data);
    c = c_vec(n_data);
  end

  return
end
