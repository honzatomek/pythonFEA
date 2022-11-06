function x = simplex_unit_sample ( m, n )

%*****************************************************************************80
%
%% simplex_unit_sample() samples the unit simplex.
%
%  Discussion:
%
%    The interior of the unit M-dimensional simplex is the set of
%    points X(1:M) such that each X(I) is nonnegative, and
%    sum(X(1:M)) <= 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    03 August 2005
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
%    integer M, the dimension of the space.
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(M,N), the points.
%

%
%  The construction begins by sampling M+1 points from the
%  exponential distribution with parameter 1.
%
  for j = 1 : n

    e = rand ( m + 1, 1 );

    e(1:m+1) = - log ( e(1:m+1) );

    x(1:m,j) = e(1:m) / sum ( e(1:m+1) );

  end

  return
end
