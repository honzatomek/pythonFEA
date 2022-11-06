function vran = direction_uniform_2d ( )

%*****************************************************************************80
%
%% direction_uniform_2d() picks a random direction vector in 2D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 February 2005
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real VRAN(2), the random direction vector, with unit norm.
%
  dim_num = 2;

  theta = 2.0 * pi * rand ( 1, 1 );

  vran(1) = cos ( theta );
  vran(2) = sin ( theta );

  return
end
