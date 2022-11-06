function value = tetra_unit_volume ( )

%*****************************************************************************80
%
%% tetra_unit_volume() returns the volume of the unit tetrahedron.
%
%  Discussion:
%
%    The integration region is:
%
%      0 <= X,
%      0 <= Y,
%      0 <= Z,
%      X + Y + Z <= 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    26 May 2004
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real VALUE, the volume.
%
  value = 1.0 / 6.0;

  return
end
