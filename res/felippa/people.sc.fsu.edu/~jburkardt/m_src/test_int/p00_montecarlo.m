function result = p00_montecarlo ( prob, int_num )

%*****************************************************************************80
%
%% p00_montecarlo() applies the Monte Carlo rule to integrate a function.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 December 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer PROB, the problem index.
%
%    integer INT_NUM, the number of sample points.
%
%  Output:
%
%    real RESULT, the approximate integral.
%
  [ a, b ] = p00_lim ( prob );

  x = a + ( b - a ) * rand ( int_num, 1 );

  result = sum ( p00_fun ( prob, int_num, x ) );

  result = result * ( b - a ) / int_num;

  return
end
