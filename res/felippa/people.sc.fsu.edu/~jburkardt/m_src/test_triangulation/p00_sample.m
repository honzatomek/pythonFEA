function point = p00_sample ( test, m, n )

%*****************************************************************************80
%
%% p00_sample() samples points from the region in a problem.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer TEST, the index of the test problem
%
%    integer M, the spatial dimension.
%
%    integer N, the number of points.
%
%  Output:
%
%    real POINT(M,N), the coordinates of the points.
%
  if ( test == 1 )
    point = p01_sample ( m, n );
  elseif ( test == 2 )
    point = p02_sample ( m, n );
  elseif ( test == 3 )
    point = p03_sample ( m, n );
  elseif ( test == 4 )
    point = p04_sample ( m, n );
  elseif ( test == 5 )
    point = p05_sample ( m, n );
  elseif ( test == 6 )
    point = p06_sample ( m, n );
  elseif ( test == 7 )
    point = p07_sample ( m, n );
  elseif ( test == 8 )
    point = p08_sample ( m, n );
  elseif ( test == 9 )
    point = p09_sample ( m, n );
  elseif ( test == 10 )
    point = p10_sample ( m, n );
  elseif ( test == 11 )
    point = p11_sample ( m, n );
  elseif ( test == 12 )
    point = p12_sample ( m, n );
  elseif ( test == 13 )
    point = p13_sample ( m, n );
  elseif ( test == 14 )
    point = p14_sample ( m, n );
  elseif ( test == 15 )
    point = p15_sample ( m, n );
  else
    fprintf ( 1, '\n' );
    fprintf ( 1, 'P00_sample - Fatal error!\n' );
    fprintf ( 1, '  Input value of TEST is out of range.\n' );
    error ( 'P00_sample - Fatal error!' );
  end

  return
end
