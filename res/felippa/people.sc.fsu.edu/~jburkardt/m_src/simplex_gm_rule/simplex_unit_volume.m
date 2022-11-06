function volume = simplex_unit_volume ( m )

%*****************************************************************************80
%
%% simplex_unit_volume() computes the volume of the unit simplex.
%
%  Discussion:
%
%    The formula is simple: volume = 1/M!.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 February 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer M, the dimension of the space.
%
%  Output:
%
%    real VOLUME, the volume of the cone.
%
  volume = 1.0;
  for i = 1 : m
    volume = volume / i;
  end

  return
end
