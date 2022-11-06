function [ r, s, area ] = node_reference_t3 ( )

%*****************************************************************************80
%
%% node_reference_t3() returns the basis nodes for the 3 node triangle.
%
%  Reference Element T3:
%
%    |
%    1  3
%    |  |\
%    |  | \
%    S  |  \
%    |  |   \
%    |  |    \
%    0  1-----2
%    |
%    +--0--R--1-->
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real R(3), S(3), the coordinates of the basis nodes.
%
%    real AREA, the area of the element.
%
  r(1:3) = [ 0.0, 1.0, 0.0 ];
  s(1:3) = [ 0.0, 0.0, 1.0 ];

  area = 0.5;

  return
end
