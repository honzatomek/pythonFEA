function value = octahedron_unit_volume_nd ( n )

%*****************************************************************************80
%
%% octahedron_unit_volume_nd() returns the volume of the unit octahedron in ND.
%
%  Integration region:
%
%    Points X(1:N) such that:
%
%      Sum ( Abs ( X(1:N) ) ) <= 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    27 November 2004
%
%  Author:
%
%    John Burkardt
%
%  Inputs:
%
%    integer N, the dimension of the space.
%
%  Output:
%
%    real VALUE, the volume of the unit octahedron.
%
  value = 2^n / prod ( 1 : n );

  return
end
