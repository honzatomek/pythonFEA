function x = uniform_in_ellipse ( n, A, r )

%*****************************************************************************80
%
%% uniform_in_ellipse() maps uniform points into an ellipse.
%
%  Discussion:
%
%    The points X in the ellipse are described by a 2 by 2
%    symmetric positive definite matrix A, and a "radius" R, such that
%
%      X' * A * X <= R^2
%
%    The upper triangular Cholesky factor of A is computed:
%
%      A = U' * U
%
%    Points Y are selected uniformly at random from the circle of radius R.
%
%      || Y || <= R
%
%    The appropriate points in the ellipse are found by solving
%
%      U * X = Y
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    09 April 2022
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
%    real A(2,2), the matrix that describes the ellipse.
%
%    real R, the right hand side of the ellipse equation.
%
%  Output:
%
%    real X(N,2), the points.
%

%
%  Get the Cholesky factor U.
%
  [ U, info ] = dpo_fa ( 2, A );

  if ( info ~= 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'uniform_in_ellipse(): Fatal error!\n' );
    fprintf ( 1, '  dpo_fa() reports that the matrix A \n' );
    fprintf ( 1, '  is not symmetric positive definite.\n' );
    error ( 'uniform_in_ellipse(): Fatal error!' );
  end
%
%  Get the points Y that satisfy Y' * Y <= R * R.
%
  x = uniform_in_circle ( n );

  x = r * x;
%
%  Solve U * X = Y.
%
  for j = 1 : n
    x(j,1:2) = dpo_sl ( 2, U, x(j,1:2) );
  end

  return
end
