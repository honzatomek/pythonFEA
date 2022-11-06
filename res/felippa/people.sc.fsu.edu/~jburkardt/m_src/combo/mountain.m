function value = mountain ( n, x, y )

%*****************************************************************************80
%
%% mountain() enumerates the mountains.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Donald Kreher, Douglas Simpson,
%    Combinatorial Algorithms,
%    CRC Press, 1998,
%    ISBN: 0-8493-3988-X,
%    LC: QA164.K73.
%
%  Input:
%
%    integer N, ...
%    N must be positive.
%
%    integer X, Y, ...
%    0 <= X <= 2 * N,
%
%  Output:
%
%    integer VALUE, the value of the "mountain function"
%    M ( N, X, Y ), which is the number of all mountain ranges from
%    (X,Y) to (2*N,0) which do not drop below sea level.
%

%
%  Check.
%
  if ( n <= 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'MOUNTAIN - Fatal error!\n' );
    fprintf ( 1, '  N <= 0.\n' );
    fprintf ( 1, '  N = %d\n', n );
    error ( 'MOUNTAIN - Fatal error!' );
  elseif ( x < 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'MOUNTAIN - Fatal error!\n' );
    fprintf ( 1, '  X < 0.\n' );
    fprintf ( 1, '  X = %d\n', x );
    error ( 'MOUNTAIN - Fatal error!' );
  elseif ( 2 * n < x )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'MOUNTAIN - Fatal error!\n' );
    fprintf ( 1, '  2 * N < X.\n' );
    fprintf ( 1, '  X = %d\n', x );
    fprintf ( 1, '  N = %d\n', n );
    error ( 'MOUNTAIN - Fatal error!' );
  end
%
%  Special cases.
%
  if ( y < 0 )
    value = 0;
    return
  end

  if ( 2 * n < x + y )
    value = 0;
    return
  end

  if ( mod ( x + y, 2 ) == 1 )
    value = 0;
    return
  end

  xy = floor ( ( x + y ) / 2 );
  a = 2 * n - x;
  b = n - xy;
  c = n - 1 - xy;

  value = i4_choose ( a, b ) - i4_choose ( a, c );

  return
end
