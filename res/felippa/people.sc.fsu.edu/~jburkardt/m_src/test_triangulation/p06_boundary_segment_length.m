function segment_length = p06_boundary_segment_length ( segment_index, h )

%*****************************************************************************80
%
%% p06_boundary_segment_length returns boundary segment lengths in problem 06.
%
%  Discussion:
%
%    For this region, the boundary points will not be evenly spaced,
%    and the value of SEGMENT_length returned will only approximately
%    guarantee that the maximal spacing is H.
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
%  Reference:
%
%    Per-Olof Persson and Gilbert Strang,
%    A Simple Mesh Generator in MATLAB,
%    SIAM Review,
%    Volume 46, Number 2, June 2004, pages 329-345.
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
  r1 = 1.0;
  r2 = 0.5;

  if ( h <= 0.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'P06_boundary_segment_length - Fatal error!' );
    fprintf ( 1, '  Nonpositive H = %f\n', h );
    error ( 'P06_boundary_segment_length - Fatal error!' );
  end
  
  if ( segment_index == 1 )

    n = round ( 2.0 * pi * r1 / h );
    n = max ( n, 5 );
    segment_length = n;

  elseif ( segment_index == 2 )

    n = round ( 2.0 * pi * r2 / h );
    n = max ( n, 5 );
    segment_length = n;

  else

    fprintf ( 1, '\n' );
    fprintf ( 1, 'P06_boundary_segment_length - Fatal error!' );
    fprintf ( 1, '  Illegal SEGMENT_INDEX = %d\n', segment_index );
    error ( 'P06_boundary_segment_length - Fatal error!' );

  end

  return
end
