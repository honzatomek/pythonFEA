function value = triangle_volume ( x, y )

%*****************************************************************************80
%
%% triangle_volume() returns the "volume" of a triangle in 2D.
%
%  Integration region:
%
%    Points (X,Y) such that
%
%      0 <= X,
%      0 <= Y, and
%      X + Y <= 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    20 May 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real X(3), Y(3), the vertices of the triangle.
%
%  Output:
%
%    real VALUE, the volume of the triangle.
%
  value = 0.5 * abs ( ...
    x(1) * ( y(2) - y(3) ) + ...
    x(2) * ( y(3) - y(1) ) + ...
    x(3) * ( y(1) - y(2) ) );

  return
end
