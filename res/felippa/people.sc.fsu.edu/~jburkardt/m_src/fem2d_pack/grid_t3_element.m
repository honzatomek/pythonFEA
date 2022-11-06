function [ element_node ] = grid_t3_element ( nelemx, nelemy )

%*****************************************************************************80
%
%% grid_t3_element() produces a grid of pairs of 3 node triangles.
%
%  Discussion:
%
%    A rectangular grid is assumed, which can be thought of as a
%    grid of (nelemx+1) x (nelemy+1) evenly spaced points, or as
%    a grid of nelemx x nelemy squares, each square being subdivided
%    into a pair of triangles.
%
%    Node numbering starts at the lower left, moves right first, and
%    then upwards.
%
%    Element numbering goes similarly, square by square, with the
%    lower triangle of each pair numbered first.
%
%    The element_node array records, for each element, the indices of
%    the three nodes that form the vertices of the triangle, in counter
%    clockwise order, starting with the node associated with the 90 degree
%    angle.
%
%  Example:
%
%    Input:
%
%      NELEMX = 3, NELEMY = 2
%
%    Output:
%
%      ELEMENT_NODE =
%         1,  2,  5;
%         6,  5,  2;
%         2,  3,  6;
%         7,  6,  3;
%         3,  4,  7;
%         8,  7,  4;
%         5,  6,  9;
%        10,  9,  6;
%         6,  7, 10;
%        11, 10,  7;
%         7,  8, 11;
%        12, 11,  8.
%
%  Grid:
%
%    9---10---11---12     +----+----+----+
%    |\   |\   |\   |     |\ 8 |\10 |\12 |
%    | \  | \  | \  |     | \  | \  | \  |
%    |  \ |  \ |  \ |     |  \ |  \ |  \ |
%    |   \|   \|   \|     |  7\|  9\| 11\|
%    5----6----7----8     +----+----+----+
%    |\   |\   |\   |     |\ 2 |\ 4 |\ 6 |
%    | \  | \  | \  |     | \  | \  | \  |
%    |  \ |  \ |  \ |     |  \ |  \ |  \ |
%    |   \|   \|   \|     |  1\|  3\|  5\|
%    1----2----3----4     +----+----+----+
%      Node indexing      Element indexing
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
%    03 April 2019
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NELEMX, NELEMY, the number of elements along the
%    X and Y directions.  The number of elements generated will be
%    2 * NELEMX * NELEMY.
%
%  Output:
%
%    integer ELEMENT_NODE(3,2*NELEMX*NELEMY), the nodes that form
%    each element.
%

%
%  Node labeling:
%
%    NW--NE
%     |\ |
%     | \|
%    SW--SE
%
  element = 0;

  for j = 1 : nelemy
    for i = 1 : nelemx

      sw = i     + ( j - 1 ) * ( nelemx + 1 );
      se = i + 1 + ( j - 1 ) * ( nelemx + 1 );
      nw = i     +   j       * ( nelemx + 1 );
      ne = i + 1 +   j       * ( nelemx + 1 );

      element = element + 1;

      element_node(1,element) = sw;
      element_node(2,element) = se;
      element_node(3,element) = nw;

      element = element + 1;

      element_node(1,element) = ne;
      element_node(2,element) = nw;
      element_node(3,element) = se;

    end
  end

  return
end
