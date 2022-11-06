function hole_point = p00_hole_point ( test, hole_index, m )

%*****************************************************************************80
%
%% p00_hole_point() returns a point inside a given hole.
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
%    integer HOLE_INDEX, the index of the hole.
%
%    integer M, the spatial dimension.
%
%  Output:
%
%    real HOLE_point(M), a point in the hole
%
  if ( test == 1 )
    hole_point = p01_hole_point ( hole_index, m );
  elseif ( test == 2 ) 
    hole_point = p02_hole_point ( hole_index, m );
  elseif ( test == 3 )
    hole_point = p03_hole_point ( hole_index, m );
  elseif ( test == 4 )
    hole_point = p04_hole_point ( hole_index, m );
  elseif ( test == 5 )
    hole_point = p05_hole_point ( hole_index, m );
  elseif ( test == 6 )
    hole_point = p06_hole_point ( hole_index, m );
  elseif ( test == 7 )
    hole_point = p07_hole_point ( hole_index, m );
  elseif ( test == 8 )
    hole_point = p08_hole_point ( hole_index, m );
  elseif ( test == 9 )
    hole_point = p09_hole_point ( hole_index, m );
  elseif ( test == 10 )
    hole_point = p10_hole_point ( hole_index, m );
  elseif ( test == 11 )
    hole_point = p11_hole_point ( hole_index, m );
  elseif ( test == 12 )
    hole_point = p12_hole_point ( hole_index, m );
  elseif ( test == 13 )
    hole_point = p13_hole_point ( hole_index, m );
  elseif ( test == 14 )
    hole_point = p14_hole_point ( hole_index, m );
  elseif ( test == 15 )
    hole_point = p15_hole_point ( hole_index, m );
  else
    fprintf ( 1, '\n' );
    fprintf ( 1, 'P00_hole_point - Fatal error!\n' );
    fprintf ( 1, '  Input value of TEST is out of range.\n' );
    error ( 'P00_hole_point - Fatal error!' );
  end

  return
end
