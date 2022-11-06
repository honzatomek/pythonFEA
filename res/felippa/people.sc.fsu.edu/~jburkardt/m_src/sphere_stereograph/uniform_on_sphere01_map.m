function x = uniform_on_sphere01_map ( dim_num, n )

%*****************************************************************************80
%
%% uniform_on_sphere01_map() maps uniform points onto the unit sphere.
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
%    10 November 2010
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
%    Wiley, 1986, page 234.
%
%  Input:
%
%    integer DIM_NUM, the dimension of the space.
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(DIM_NUM,N), the points.
%
  x = randn ( dim_num, n );
  v(1,1:n) = sqrt ( sum ( x.^2, 1 ) );
  vv = repmat ( v, dim_num, 1 );
  x = x ./ vv;

  return
end
