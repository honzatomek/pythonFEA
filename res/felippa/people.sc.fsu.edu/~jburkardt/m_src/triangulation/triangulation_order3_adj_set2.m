function [ ia, ja ] = triangulation_order3_adj_set2 ( node_num, ...
  triangle_num, triangle_node, triangle_neighbor, adj_num, adj_col )

%*****************************************************************************80
%
%% triangulation_order3_adj_set2() sets adjacencies in a triangulation.
%
%  Discussion:
%
%    This routine is called to set up the arrays IA and JA that
%    record which nodes are adjacent in a triangulation.
%
%    The triangulation is assumed to involve 3-node triangles.
%
%    Two nodes are "adjacent" if they are both nodes in some triangle.
%    Also, a node is considered to be adjacent to itself.
%
%    This routine can be used to set up the sparse triplet storage
%    for a linear triangle finite element discretization of Poisson's
%    equation in two dimensions.
%
%  Diagram:
%
%       3
%    s  |\
%    i  | \
%    d  |  \
%    e  |   \  side 2
%       |    \
%    3  |     \
%       |      \
%       1-------2
%
%         side 1
%
%    The local node numbering
%
%
%   21-22-23-24-25
%    |\ |\ |\ |\ |
%    | \| \| \| \|
%   16-17-18-19-20
%    |\ |\ |\ |\ |
%    | \| \| \| \|
%   11-12-13-14-15
%    |\ |\ |\ |\ |
%    | \| \| \| \|
%    6--7--8--9-10
%    |\ |\ |\ |\ |
%    | \| \| \| \|
%    1--2--3--4--5
%
%    A sample grid
%
%
%    Below, we have a chart that summarizes the adjacency relationships
%    in the sample grid.  On the left, we list the node, and its neighbors,
%    with an asterisk to indicate the adjacency of the node to itself
%    (in some cases, you want to count this self adjacency and in some
%    you don't).  On the right, we list the number of adjancencies to
%    lower-indexed nodes, to the node itself, to higher-indexed nodes,
%    the total number of adjacencies for this node, and the location
%    of the first and last entries required to list this set of adjacencies
%    in a single list of all the adjacencies.
%
%    N   Adjacencies                Below  Self    Above  Total First  Last
%
%   --  -- -- -- -- -- -- --           --    --      --      --   ---     0
%    1:  *  2  6                        0     1       2       3     1     3
%    2:  1  *  3  6  7                  1     1       3       5     4     8
%    3:  2  *  4  7  8                  1     1       3       5     9    13
%    4:  3  *  5  8  9                  1     1       3       5    14    18
%    5:  4  *  9 10                     1     1       2       4    19    22
%    6:  1  2  *  7 11                  2     1       2       5    23    27
%    7:  2  3  6  *  8 11 12            3     1       3       7    28    34
%    8:  3  4  7  *  9 12 13            3     1       3       7    35    41
%    9:  4  5  8  * 10 13 14            3     1       3       7    42    48
%   10:  5  9  * 14 15                  2     1       2       5    49    53
%   11:  6  7  * 12 16                  2     1       2       5    54    58
%   12:  7  8 11  * 13 16 17            3     1       3       7    59    65
%   13:  8  9 12  * 14 17 18            3     1       3       7    66    72
%   14:  9 10 13  * 15 18 19            3     1       3       7    73    79
%   15: 10 14  * 19 20                  2     1       2       5    80    84
%   16: 11 12  * 17 21                  2     1       2       5    85    89
%   17: 12 13 16  * 18 21 22            3     1       3       7    90    96
%   18: 13 14 17  * 19 22 23            3     1       3       7    97   103
%   19: 14 15 18  * 20 23 24            3     1       3       7   104   110
%   20: 15 19  * 24 25                  2     1       2       5   111   115
%   21: 16 17  * 22                     2     1       1       4   116   119
%   22: 17 18 21  * 23                  3     1       1       5   120   124
%   23: 18 19 22  * 24                  3     1       1       5   125   129
%   24: 19 20 23  * 25                  3     1       1       5   130   134
%   25: 20 24  *                        2     1       0       3   135   137
%   --  -- -- -- -- -- -- --           --    --      --      --   138   ---
%
%    For this example, the initial portion of the IA and JA arrays will be:
%
%      (1,1), (1,2), (1,6),
%      (2,1), (2,2), (2,3), (2,6), (2,7),
%      (3,2), (3,3), (3,4), (3,7), (3,8),
%      ...
%      (25,20), (25,24), (25,25)
%
%    for a total of 137 pairs of values.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 July 2007
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NODE_NUM, the number of nodes.
%
%    integer TRIANGLE_NUM, the number of triangles.
%
%    integer TRIANGLE_NODE(3,TRIANGLE_NUM), lists the nodes
%    that make up each triangle in counterclockwise order.
%
%    integer TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), for each
%    side of a triangle, lists the neighboring triangle, or -1 if there is
%    no neighbor.
%
%    integer ADJ_NUM, the number of adjacencies.
%
%    integer ADJ_COL(NODE_NUM+1).  Information about
%    column J is stored in entries ADJ_COL(J) through ADJ_COL(J+1)-1 of ADJ.
%
%  Output:
%
%    integer IA(ADJ_NUM), JA(ADJ_NUM), the adjacency
%    information.
%
  triangle_order = 3;
  ia(1:adj_num) = -1;
  ja(1:adj_num) = -1;

  adj_copy(1:node_num) = adj_col(1:node_num);
%
%  Set every node to be adjacent to itself.
%
  for node = 1 : node_num
    ia(adj_copy(node)) = node;
    ja(adj_copy(node)) = node;
    adj_copy(node) = adj_copy(node) + 1;
  end
%
%  Examine each triangle.
%
  for triangle = 1 : triangle_num

    n1 = triangle_node(1,triangle);
    n2 = triangle_node(2,triangle);
    n3 = triangle_node(3,triangle);
%
%  Add edge (1,2) if this is the first occurrence,
%  that is, if the edge (1,2) is on a boundary (TRIANGLE2 <= 0)
%  or if this triangle is the first of the pair in which the edge
%  occurs (TRIANGLE < TRIANGLE2).
%
    triangle2 = triangle_neighbor(1,triangle);

    if ( triangle2 < 0 || triangle < triangle2 )

      ia(adj_copy(n1)) = n1;
      ja(adj_copy(n1)) = n2;
      adj_copy(n1) = adj_copy(n1) + 1;

      ia(adj_copy(n2)) = n2;
      ja(adj_copy(n2)) = n1;
      adj_copy(n2) = adj_copy(n2) + 1;

    end
%
%  Add edge (2,3).
%
    triangle2 = triangle_neighbor(2,triangle);

    if ( triangle2 < 0 || triangle < triangle2 )

      ia(adj_copy(n2)) = n2;
      ja(adj_copy(n2)) = n3;
      adj_copy(n2) = adj_copy(n2) + 1;

      ia(adj_copy(n3)) = n3;
      ja(adj_copy(n3)) = n2;
      adj_copy(n3) = adj_copy(n3) + 1;

    end
%
%  Add edge (3,1).
%
    triangle2 = triangle_neighbor(3,triangle);

    if ( triangle2 < 0 || triangle < triangle2 )

      ia(adj_copy(n1)) = n1;
      ja(adj_copy(n1)) = n3;
      adj_copy(n1) = adj_copy(n1) + 1;

      ia(adj_copy(n3)) = n3;
      ja(adj_copy(n3)) = n1;
      adj_copy(n3) = adj_copy(n3) + 1;

    end

  end
%
%  Lexically sort the IA, JA values.
%
  [ ia, ja ] = i4vec2_sort_a ( adj_num, ia, ja );

  return
end
