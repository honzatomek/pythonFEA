function value = parallelipiped_volume_nd ( n, v )

%*****************************************************************************80
%
%% parallelipiped_volume_nd() returns the volume of a parallelipiped in ND.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    13 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the dimension of the space.
%
%    real V(N+1,N); each row contains the coordinates of
%    one of the vertices of the parallelipiped.
%
%  Output:
%
%    real VALUE, the volume of the parallelipiped.
%
  v2(1:n+1,1:n) = v(1:n+1,1:n);
%
%  Compute the volume of the N-dimensional parallelipiped.
%
  v2(1:n+1,n+1) = 1.0;

  value = abs ( det ( v2 ) );

  return
end
