function point = p14_sample ( m, n )

%*****************************************************************************80
%
%% p14_sample() samples points from the region in problem 14.
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
%    integer M, the spatial dimension.
%
%    integer N, the number of points.
%
%  Output:
%
%    real POINT(M,N), the coordinates of the points.
%
  box = [ ...
    100.0,  145.0; ...
   +634.0, +799.0 ]';

  reject = 0;

  for j = 1 : n

    while ( true )

      x = rand ( m, 1 );
      x(1:m,1) = ( 1.0 - x(1:m,1) ) .* box(1:m,1) + x(1:m,1) .* box(1:m,2);

      inside = p14_inside ( m, 1, x );

      if ( inside )
        break
      end

      reject = reject + 1;

      if ( 30 * n + 10 <= reject )
        fprintf ( 1, '\n' );
        fprintf ( 1, 'P14_sample - Fatal error!\n' );
        fprintf ( 1, '  Trying to generate point J = %d\n', j );
        fprintf ( 1, '  Number of rejections = %d\n', reject );
        fprintf ( 1, '  Rejection percentage = %f\n', ...
          ( 100 * reject ) / ( reject + j - 1 ) );
        r8vec_print ( m, x, '  Most recent rejected point: ' );
        error ( 'P14_sample - Fatal error!' );
      end

    end

    point(1:m,j) = x(1:m,1);

  end

  return
end
