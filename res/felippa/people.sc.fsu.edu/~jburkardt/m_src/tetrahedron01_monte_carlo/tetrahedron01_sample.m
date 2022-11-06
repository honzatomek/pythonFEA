function x = tetrahedron01_sample ( n )

%*****************************************************************************80
%
%% tetrahedron01_sample() samples the unit tetrahedron in 3D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 January 2014
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
%    Krieger, 1992,
%    ISBN: 0894647644,
%    LC: QA298.R79.
%
%  Input:
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(3,N), the points.
%
  m = 3;

  for j = 1 : n

    e = rand ( m + 1, 1 );

    e(1:m+1) = - log ( e(1:m+1) );

    x(1:m,j) = e(1:m) / sum ( e(1:m+1) );

  end

  return
end
