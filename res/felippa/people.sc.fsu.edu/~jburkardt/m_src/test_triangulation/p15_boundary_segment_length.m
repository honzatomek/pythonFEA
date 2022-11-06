function segment_length = p15_boundary_segment_length ( segment_index, h )

%*****************************************************************************80
%
%% p15_boundary_segment_length returns boundary segment lengths in problem 15.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer SEGMENT_INDEX, the index of one of the boundary segments.
%
%    real H, the suggested spacing between points.
%
%  Output:
%
%    integer SEGMENT_length, the number of points in the segment.
%
  if ( h <= 0.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'P15_boundary_segment_length - Fatal error!' );
    fprintf ( 1, '  Nonpositive H = %f\n', h );
    error ( 'P15_boundary_segment_length - Fatal error!' );
  end

  if ( segment_index == 1 )

    n = round ( 36.0 / h );
    n = max ( n, 17 );
    segment_length = n + mod ( 36 - mod ( n - 1, 36 ), 36 );

  else

    fprintf ( 1, '\n' );
    fprintf ( 1, 'P15_boundary_segment_length - Fatal error!' );
    fprintf ( 1, '  Illegal SEGMENT_INDEX = %d\n', segment_index );
    error ( 'P15_boundary_segment_length - Fatal error!' );

  end

  return
end
