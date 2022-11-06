function x = uniform_on_simplex ( n, d )

%*****************************************************************************80
%
%% uniform_on_simplex() maps uniform points onto the unit simplex.
%
%  Discussion:
%
%    The surface of the unit D-dimensional simplex is the set of points
%    X(1:D) such that each X(I) is nonnegative,
%    every X(I) is no greater than 1, and
%
%    ( X(I) = 0 for some I, or sum ( X(1:D) ) = 1. )
%
%    In D dimensions, there are D sides plus one main face.
%    This code picks a point uniformly with respect to "area".
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    11 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Reuven Rubinstein,
%    Monte Carlo Optimization, Simulation, and Sensitivity
%    of Queueing Networks,
%    Wiley, 1986.
%
%  Input:
%
%    integer N, the number of points.
%
%    integer D, the dimension of the space.
%
%  Output:
%
%    real X(N,D), the points.
%
  x = zeros ( d, n );
%
%  The construction begins by sampling D points from the
%  exponential distribution with parameter 1.
%
  for i = 1 : n

    e = rand ( 1, d );

    e(1,1:d ) = - log ( e(1,1:d) );

    e_sum = sum ( e(1,1:d) );

    x(i,1:d) = e(1,1:d) / e_sum;
%
%  The point X is now on the "main" face of the unit simplex.
%
%  Based on their relative areas, choose a side of the simplex,
%  or the main face.
%
    area1 = sqrt ( d );

    area2 = d;

    r = rand ( 1, 1 );
%
%  If we choose to move the point from the main face,
%  set a random coordinate to 0.
%
    if ( area1 / ( area1 + area2 ) < r )
      j = randi ( [ 1, d ], 1 );
      x(i,j) = 0.0;
    end

  end

  return
end
