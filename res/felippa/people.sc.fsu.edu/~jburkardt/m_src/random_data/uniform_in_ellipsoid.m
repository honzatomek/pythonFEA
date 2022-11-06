function x = uniform_in_ellipsoid ( n, d, a, r )

%*****************************************************************************80
%
%% uniform_in_ellipsoid() maps uniform points into an ellipsoid.
%
%  Discussion:
%
%    The points X in the ellipsoid are described by a D by D
%    symmetric positive definite matrix A, and a "radius" R, such that
%
%      X' * A * X <= R * R
%
%    The algorithm computes the Cholesky factorization of A:
%
%      A = U' * U.
%
%    A set of uniformly random points Y is generated, satisfying:
%
%      Y' * Y <= R * R.
%
%    The appropriate points in the ellipsoid are found by solving
%
%      U * X = Y
%
%    Thanks to Dr Karl-Heinz Keil for pointing out that the original
%    coding was actually correct only if A was replaced by its inverse.
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
%    real A(D,D), the matrix that describes the ellipsoid.
%
%    real R, the right hand side of the ellipsoid equation.
%
%  Output:
%
%    real X(N,D), the points.
%

%
%  Get the Cholesky factor U.
%
  [ a_fa, info ] = dpo_fa ( d, a );

  if ( info ~= 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'uniform_in_ellipsoid(): Fatal error!\n' );
    fprintf ( 1, '  dpo_fa() reports that the matrix A \n' );
    fprintf ( 1, '  is not symmetric positive definite.\n' );
    error ( 'uniform_in_ellipsoid(): Fatal error!' );
  end
%
%  Get the points Y that satisfy Y' * Y <= R * R.
%
  x = uniform_in_hypersphere ( n, d );

  x = r * x;
%
%  Solve U * X = Y.
%
  for i = 1 : n
    x(i,:) = dpo_sl ( d, a_fa, x(i,:) );
  end

  return
end
