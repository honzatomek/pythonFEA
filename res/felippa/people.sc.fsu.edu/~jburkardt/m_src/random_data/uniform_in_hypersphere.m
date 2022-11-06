function x = uniform_in_hypersphere ( n, d )

%*****************************************************************************80
%
%% uniform_in_hypersphere() maps uniform points into the unit hypersphere.
%
%  Discussion:
%
%    The sphere has center 0 and radius 1.
%
%    We first generate a point ON the sphere, and then distribute it
%    IN the sphere.
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
%    Russell Cheng,
%    Random Variate Generation,
%    in Handbook of Simulation,
%    edited by Jerry Banks,
%    Wiley, 1998, pages 168.
%
%    Reuven Rubinstein,
%    Monte Carlo Optimization, Simulation, and Sensitivity
%    of Queueing Networks,
%    Wiley, 1986, page 232.
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

  exponent = 1.0 / d;

  for i = 1 : n
%
%  Fill a vector with normally distributed values.
%
    v = randn ( 1, d );
%
%  Compute the length of the vector.
%
    v_norm = norm ( v );
%
%  Normalize the vector.
%
    v = v / v_norm;
%
%  Now compute a value to map the point ON the sphere INTO the sphere.
%
    r = rand ( 1, 1 );

    x(i,1:d) = r^exponent * v;

  end

  return
end
