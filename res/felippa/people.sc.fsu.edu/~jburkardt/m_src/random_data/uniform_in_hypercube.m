function x = uniform_in_hypercube ( n, d )

%*****************************************************************************80
%
%% uniform_in_hypercube() creates uniform points in the unit hypercube.
%
%  Discussion:
%
%    The unit hypercube is defined as points whose components are between
%    -1 and 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    10 April 2022
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
  x = 2.0 * rand ( n, d ) - 1.0;

  return
end
