function x = normal_square ( n, d )

%*****************************************************************************80
%
%% normal_square() creates normally distributed points in D dimensions.
%
%  Discussion:
%
%    The multivariate normal distribution has the form:
%
%      f(x) = (2*pi*det(V))^(-D/2) * exp(-0.5*(x-mu)'*inverse(V)*(x-mu))
%
%    where mu is the mean vector, and V is a positive definite symmetric
%    matrix called the variance-covariance matrix.
%
%    This routine implements the simplest version of a multivariate
%    normal distribution.  The variance-covariance matrix is the identity,
%    and the mean vector is entirely zero.  Thus, a sample on N points
%    is simply N*D scalar values generated under the univariate
%    normal distribution with zero mean and unit variance.
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
  x = randn ( n, d );

  return
end
