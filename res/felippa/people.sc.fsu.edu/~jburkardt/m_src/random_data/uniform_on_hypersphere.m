function x = uniform_on_hypersphere ( n, d )

%*****************************************************************************80
%
%% uniform_on_hypersphere() maps uniform points onto the unit hypersphere.
%
%  Discussion:
%
%    The sphere has center 0 and radius 1.
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
%    Russell Cheng,
%    Random Variate Generation,
%    in Handbook of Simulation,
%    edited by Jerry Banks,
%    Wiley, 1998, pages 168.
%
%    George Marsaglia,
%    Choosing a point from the surface of a sphere,
%    Annals of Mathematical Statistics,
%    Volume 43, Number 2, April 1972, pages 645-646.
%
%    Reuven Rubinstein,
%    Monte Carlo Optimization, Simulation, and Sensitivity
%    of Queueing Networks,
%    Wiley, 1986, page 234.
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
  for i = 1 : n
    xnorm = norm ( x(i,:) );
    x(i,:) = x(i,:) / xnorm;
  end

  return
end
