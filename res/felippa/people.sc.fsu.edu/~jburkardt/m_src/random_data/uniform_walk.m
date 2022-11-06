function x = uniform_walk ( n, d )

%*****************************************************************************80
%
%% uniform_walk() generates points on a uniform random walk.
%
%  Discussion:
%
%    The first point is at the origin.  Uniform random numbers are
%    generated to determine the direction of the next step, which
%    is always of length 1, and in coordinate direction.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    13 April 2022
%
%  Author:
%
%    John Burkardt
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
  x = zeros ( n, d );

  x(1,:) = 0.0;

  dir = rand ( n - 1, 1 );

  dir(1:n-1) = ( 2 * d ) * ( dir(1:n-1) - 0.5 );

  for i = 2 : n

    x(i,1:d) = x(i-1,1:d);

    j = round ( abs ( dir(i-1) ) + 0.5 );
    j = min ( j, d );
    j = max ( j, 1 );

    if ( dir(i-1) < 0.0 )
      x(i,j) = x(i,j) - 1.0;
    else
      x(i,j) = x(i,j) + 1.0;
    end

  end

  return
end
