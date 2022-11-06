function x = brownian ( n, d )

%*****************************************************************************80
%
%% brownian() creates Brownian motion points.
%
%  Discussion:
%
%    A starting point is generated at the origin.  The next point
%    is generated at a uniformly random angle and a (0,1) normally
%    distributed distance from the previous point.
%
%    It is up to the user to rescale the data, if desired.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 April 2022
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

%
%  Initial point.
%
  x = zeros ( n, d );
%
%  Generate angles and steps.
%
  for i = 2 : n

    r = abs ( randn ( 1, 1 ) );

    direction = direction_uniform_nd ( d );

    x(i,1:d) = x(i-1,1:d) + r * direction(1,1:d);

  end

  return
end
