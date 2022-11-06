function value = simplex_unit_volume_nd ( n )

%*****************************************************************************80
%
%% simplex_unit_volume_nd() returns the volume of the unit simplex in ND.
%
%  Integration region:
%
%    The unit simplex in N dimensions,
%      0 <= X(1:N),
%      Sum ( X(1:N) ) <= 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    11 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the dimension of the space.
%
%  Output:
%
%    real VALUE, the volume of the unit simplex.
%
  value = 1.0 / factorial ( n );

  return
end
