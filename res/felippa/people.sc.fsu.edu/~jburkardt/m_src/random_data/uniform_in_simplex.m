function x = uniform_in_simplex ( n, d )

%*****************************************************************************80
%
%% uniform_in_simplex() maps uniform points into the unit simplex.
%
%  Discussion:
%
%    The interior of the unit D-dimensional simplex is the set of
%    points X(1:D) such that each X(I) is nonnegative, and
%    sum(X(1:D)) <= 1.
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
  x = zeros ( n, d );
%
%  The construction begins by sampling D+1 points from the
%  exponential distribution with parameter 1.
%
  for i = 1 : n

    e = rand ( 1, d + 1 );

    e(1,1:d+1) = - log ( e(1,1:d+1) );

    e_sum = sum ( e(1,1:d+1) );

    x(i,1:d) = e(1,1:d) / e_sum;

  end

  return
end
