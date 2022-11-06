function x = hypersphere_01_interior_uniform ( m, n )

%*****************************************************************************80
%
%% hypersphere_01_interior_uniform(): uniform points inside unit hypersphere.
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
%    integer M, the dimension of the space.
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(M,N), the points.
%
  exponent = 1.0 / m;

  for j = 1 : n
%
%  Fill a vector with normally distributed values.
%
    v = randn ( m, 1 );
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

    x(1:m,j) = r^exponent * v;

  end

  return
end
