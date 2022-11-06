function [ adj_num, adj_col ] = ns_adj_col_set ( node_num, triangle_num, ...
  variable_num, triangle_node, triangle_neighbor, node_u_variable, ...
  node_v_variable, node_p_variable )

%*****************************************************************************80
%
%% ns_adj_col_set() sets the COL array in a Navier Stokes triangulation.
%
%  Discussion:
%
%    This routine also counts the the value and returns the value of
%    ADJ_NUM, the number of Navier-Stokes variable adjacencies, which
%    should be identical to the value that would have been computed
%    by calling NS_ADJ_COUNT.
%
%    This routine is called to set up the ADJ_COL array, which indicates
%    the number of entries needed to store each column in the sparse
%    compressed column storage to be used for the adjancency matrix.
%
%    The triangulation is assumed to involve 6-node triangles.
%
%    Variables for the horizontal and vertical velocities are associated
%    with every node.  Variables for the pressure are associated only with
%    the vertex nodes.
%
%    We are interested in determining the number of nonzero entries in the
%    stiffness matrix of the Stokes equations, or the jacobian matrix of
%    the Navier Stokes equations.  To this end, we will say, somewhat
%    too broadly, that two variables are "adjacent" if their associated 
%    nodes both occur in some common element.  This adjacency of variables
%    I and J is taken to be equivalent to the possible nonzeroness of
%    matrix entries A(I,J) and A(J,I).
%
%    A sparse compressed column format is used to store the counts for
%    the nonzeroes.  In other words, while the value ADJ_NUM reports the
%    number of adjacencies, the vector ADJ_COL is sufficient to allow us
%    to properly set up a sparse compressed matrix for the actual storage
%    of the sparse matrix, if we desire to proceed.
%
%  Local Node Numbering:
%
%       3
%    s  |\
%    i  | \
%    d  |  \
%    e  6   5  side 2
%       |    \
%    3  |     \
%       |      \
%       1---4---2
%
%         side 1
%
%  Variable Diagram:
%
%      UVP
%       |\
%       | \
%       |  \
%      UV   UV
%       |    \
%       |     \
%       |      \
%      UVP--UV--UVP
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 September 2006
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
%    integer VARIABLE_NUM, the number of variables.
%
%    integer TRIANGLE_NODE(6,TRIANGLE_NUM), lists the nodes that
%    make up each triangle.  The first three nodes are the vertices,
%    in counterclockwise order.  The fourth value is the midside
%    node between nodes 1 and 2; the fifth and sixth values are
%    the other midside nodes in the logical order.
%
%    integer TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), for each side of
%    a triangle, lists the neighboring triangle, or -1 if there is
%    no neighbor.
%
%    integer NODE_U_VARIABLE(NODE_NUM), NODE_V_VARIABLE(NODE_NUM),
%    NODE_P_VARIABLE(NODE_NUM), the index of the horizontal velocity, 
%    vertical velocity and pressure variables associated with a node,
%    or -1 if no such variable is associated with the node.
%
%  Output:
%
%    integer ADJ_NUM, the number of Navier Stokes variable adjacencies.
%
%    integer ADJ_COL(VARIABLE_NUM+1).  Information about variable J 
%    is stored in entries ADJ_COL(J) through ADJ_COL(J+1)-1 of ADJ.
%
  triangle_order = 6;

  adj_num = 0;
%
%  Set every variable to be adjacent to itself.
%
  adj_col(1:variable_num) = 1;
%
%  Set every variable to be adjacent to the other variables associated with
%  that node. 
%
%  U <=> V
%  U <=> P (if there is a P variable)
%  V <=> P (if there is a P variable)
%
  for node = 1 : node_num

    u1 = node_u_variable(node);
    v1 = node_v_variable(node);
    p1 = node_p_variable(node);

    adj_col(u1) = adj_col(u1) + 1;
    adj_col(v1) = adj_col(v1) + 1 ; 

    if ( 0 < p1 )
      adj_col(u1) = adj_col(u1) + 1;
      adj_col(v1) = adj_col(v1) + 1;
      adj_col(p1) = adj_col(p1) + 2;
    end

  end
%
%  Examine each triangle.
%
  for triangle = 1 : triangle_num

    n1 = triangle_node(1,triangle);
    n2 = triangle_node(2,triangle);
    n3 = triangle_node(3,triangle);
    n4 = triangle_node(4,triangle);
    n5 = triangle_node(5,triangle);
    n6 = triangle_node(6,triangle);

    u1 = node_u_variable(n1);
    v1 = node_v_variable(n1);
    p1 = node_p_variable(n1);

    u2 = node_u_variable(n2);
    v2 = node_v_variable(n2);
    p2 = node_p_variable(n2);

    u3 = node_u_variable(n3);
    v3 = node_v_variable(n3);
    p3 = node_p_variable(n3);

    u4 = node_u_variable(n4);
    v4 = node_v_variable(n4);

    u5 = node_u_variable(n5);
    v5 = node_v_variable(n5);

    u6 = node_u_variable(n6);
    v6 = node_v_variable(n6);
%
%  For sure, we add the new adjacencies:
%
%    U5 V5 <=> U1 V1 P1
%    U6 V6 <=> U2 V2 P2
%    U4 V4 <=> U3 V3 P3
%    U5 V5 <=> U4 V4
%    U6 V6 <=> U4 V4
%    U6 V6 <=> U5 V5
%
    adj_col(u1) = adj_col(u1) + 2;
    adj_col(v1) = adj_col(v1) + 2;
    adj_col(p1) = adj_col(p1) + 2;

    adj_col(u2) = adj_col(u2) + 2;
    adj_col(v2) = adj_col(v2) + 2;
    adj_col(p2) = adj_col(p2) + 2;

    adj_col(u3) = adj_col(u3) + 2;
    adj_col(v3) = adj_col(v3) + 2;
    adj_col(p3) = adj_col(p3) + 2;

    adj_col(u4) = adj_col(u4) + 7;
    adj_col(v4) = adj_col(v4) + 7;

    adj_col(u5) = adj_col(u5) + 7;
    adj_col(v5) = adj_col(v5) + 7;

    adj_col(u6) = adj_col(u6) + 7;
    adj_col(v6) = adj_col(v6) + 7;
%
%  Add edges (1,2), (1,4), (2,4) if this is the first occurrence,
%  that is, if the edge (1,4,2) is on a boundary (TRIANGLE2 <= 0)
%  or if this triangle is the first of the pair in which the edge
%  occurs (TRIANGLE < TRIANGLE2).
%
%  Maybe add
%
%    U1 V1 P1 <=> U2 V2 P2
%    U1 V1 P1 <=> U4 V4
%    U2 V2 P2 <=> U4 V4
%
    triangle2 = triangle_neighbor(1,triangle);

    if ( triangle2 < 0 || triangle < triangle2 )

      adj_col(u1) = adj_col(u1) + 5;
      adj_col(v1) = adj_col(v1) + 5;
      adj_col(p1) = adj_col(p1) + 5;

      adj_col(u2) = adj_col(u2) + 5;
      adj_col(v2) = adj_col(v2) + 5;
      adj_col(p2) = adj_col(p2) + 5;

      adj_col(u4) = adj_col(u4) + 6;
      adj_col(v4) = adj_col(v4) + 6;

    end
%
%  Maybe add
%
%    U2 V2 P2 <=> U3 V3 P3
%    U2 V2 P2 <=> U5 V5
%    U3 V3 P3 <=> U5 V5
%
    triangle2 = triangle_neighbor(2,triangle);

    if ( triangle2 < 0 || triangle < triangle2 )

      adj_col(u2) = adj_col(u2) + 5;
      adj_col(v2) = adj_col(v2) + 5;
      adj_col(p2) = adj_col(p2) + 5;

      adj_col(u3) = adj_col(u3) + 5;
      adj_col(v3) = adj_col(v3) + 5;
      adj_col(p3) = adj_col(p3) + 5;

      adj_col(u5) = adj_col(u5) + 6;
      adj_col(v5) = adj_col(v5) + 6;

    end
%
%  Maybe add
%
%    U1 V1 P1 <=> U3 V3 P3
%    U1 V1 P1 <=> U6 V6
%    U3 V3 P3 <=> U6 V6
%
    triangle2 = triangle_neighbor(3,triangle);

    if ( triangle2 < 0 || triangle < triangle2 )

      adj_col(u1) = adj_col(u1) + 5;
      adj_col(v1) = adj_col(v1) + 5;
      adj_col(p1) = adj_col(p1) + 5;

      adj_col(u3) = adj_col(u3) + 5;
      adj_col(v3) = adj_col(v3) + 5;
      adj_col(p3) = adj_col(p3) + 5;

      adj_col(u6) = adj_col(u6) + 6;
      adj_col(v6) = adj_col(v6) + 6;

    end
      
  end
%
%  We used ADJ_COL to count the number of entries in each column.
%  Convert it to pointers into the ADJ array.
%
  adj_col(2:variable_num+1) = adj_col(1:variable_num);

  adj_col(1) = 1;
  for variable = 2 : variable_num + 1
    adj_col(variable) = adj_col(variable-1) + adj_col(variable);
  end

  adj_num = adj_col(variable_num+1) - 1;

  return
end
