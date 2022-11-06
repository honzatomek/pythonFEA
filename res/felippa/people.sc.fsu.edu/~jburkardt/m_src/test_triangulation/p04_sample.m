function point = p04_sample ( m, n )

%*****************************************************************************80
%
%% p04_sample() samples points from the region in problem 04.
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
  r1 = 1.0;
  r2 = 0.5;

  have = 0;
%
%  We are going to generate batches of sample points.
%
  sample_num = min ( 1000, n );

  x1 = -r1;
  x2 = +r1;
  y1 = -r1;
  y2 = +r1;

  while ( true )
%
%  Generate a batch of points in [0,1]x[0,1].
%
    sample = rand ( m, sample_num );
%
%  Remap the points to [x1,x2] x [y1,y2].
%
    sample(1,1:sample_num) = x1 + sample(1,1:sample_num) * ( x2 - x1 );
    sample(2,1:sample_num) = x1 + sample(2,1:sample_num) * ( y2 - y1 );
%
%  Accept each point that is inside the region.
%
    inside = p04_inside ( m, sample_num, sample );

    for j = 1 : sample_num

      if ( inside(j) )

        have = have + 1;
        point(1:m,have) = sample(1:m,j);

        if ( have == n )
          return
        end

      end
   
    end

  end

  return
end
