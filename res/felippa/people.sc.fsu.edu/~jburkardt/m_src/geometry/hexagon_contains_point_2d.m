function value = hexagon_contains_point_2d ( v, p )

%*****************************************************************************80
%
%% hexagon_contains_point_2d() finds if a point is inside a hexagon in 2D.
%
%  Discussion:
%
%    This test is only valid if the hexagon is convex.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 June 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real V(2,6), the vertics, in counterclockwise order.
%
%    real P(2,1), the point to be tested.
%
%  Output:
%
%    logical VALUE, is TRUE if X is in the hexagon.
%
  p = p(:);

  n = 6;
%
%  A point is inside a convex hexagon if and only if it is "inside"
%  each of the 6 halfplanes defined by lines through consecutive
%  vertices.
%
  for i = 1 : n

    j = mod ( i, n ) + 1;

    if (  v(1,i) * ( v(2,j) - p(2,1) ) ...
        + v(1,j) * ( p(2,1) - v(2,i) ) ...
        + p(1,1) * ( v(2,i) - v(2,j) ) < 0.0 )

      value = false;
      return

    end

  end

  value = true;

  return
end
