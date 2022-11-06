function x = simplex01_sample ( m, n )

%*****************************************************************************80
%
%% simplex01_sample() samples the unit simplex in M dimensions.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the spatial dimension.
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(M,N), the points.
%
  for j = 1 : n

    e = rand ( m + 1, 1 );

    e(1:m+1) = - log ( e(1:m+1) );

    x(1:m,j) = e(1:m) / sum ( e(1:m+1) );

  end

  return
end
