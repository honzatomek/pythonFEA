function [ node_num2, tetra_num2, edge_data ] = ...
  tet_mesh_order4_refine_size ( node_num1, tetra_num1, tetra_node1 )

%*****************************************************************************80
%
%% tet_mesh_order4_refine_size() sizes a refined order 4 tet mesh.
%
%  Discussion:
%
%    A refined tet mesh can be derived from an existing one by interpolating
%    nodes at the midpoint of every edge of the mesh.
%
%    The mesh is described indirectly, as the sum of individual
%    tetrahedrons.  A single physical edge may be a logical edge of
%    any number of tetrahedrons.  It is important, however, that a
%    new node be created exactly once for each edge, assigned an index,
%    and associated with every tetrahedron that shares this edge.
%
%    This routine handles that problem.
%
%    The primary amount of work occurs in sorting a list of 6 * TETRA_NUM
%    data items, one item for every edge of every tetrahedron.  Each
%    data item records, for a given tetrahedron edge, the global indices
%    of the two endpoints, the local indices of the two endpoints,
%    and the index of the tetrahedron.
%
%    Through careful sorting, it is possible to arrange this data in
%    a way that allows the proper generation of the interpolated nodes.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 January 2007
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NODE_NUM1, the number of nodes in the original mesh.
%
%    integer TETRA_NUM1, the number of tetrahedrons in the
%    original mesh.
%
%    integer TETRA_NODE1(4,TETRA_NUM1), the indices of the nodes
%    that form the tetrahedrons in the input mesh.
%
%  Output:
%
%    integer NODE_NUM2, the number of nodes in the refined mesh.
%
%    integer TETRA_NUM2, the number of tetrahedrons in the
%    refined mesh.
%
%    integer EDGE_DATA(5,6*TETRA_NUM1), edge data.
%

%
%  Step 1.
%  From the list of nodes for tetrahedron T, of the form: (I,J,K,L)
%  construct the six edge relations:
%
%    (I,J,1,2,T)
%    (I,K,1,3,T)
%    (I,L,1,4,T)
%    (J,K,2,3,T)
%    (J,L,2,4,T)
%    (K,L,3,4,T)
%
%  In order to make matching easier, we reorder each pair of nodes
%  into ascending order.
%
  for tetra = 1 : tetra_num1

    i = tetra_node1(1,tetra);
    j = tetra_node1(2,tetra);
    k = tetra_node1(3,tetra);
    l = tetra_node1(4,tetra);

    [ a, b ] = i4i4_sort_a ( i, j );

    edge_data(1:5,6*(tetra-1)+1) = [ a, b, 1, 2, tetra ]';

    [ a, b ] = i4i4_sort_a ( i, k );

    edge_data(1:5,6*(tetra-1)+2) = [ a, b, 1, 3, tetra ]';

    [ a, b ] = i4i4_sort_a ( i, l );

    edge_data(1:5,6*(tetra-1)+3) = [ a, b, 1, 4, tetra ]';

    [ a, b ] = i4i4_sort_a ( j, k );

    edge_data(1:5,6*(tetra-1)+4) = [ a, b, 2, 3, tetra ]';

    [ a, b ] = i4i4_sort_a ( j, l );

    edge_data(1:5,6*(tetra-1)+5) = [ a, b, 2, 4, tetra ]';

    [ a, b ] = i4i4_sort_a ( k, l );

    edge_data(1:5,6*(tetra-1)+6) = [ a, b, 3, 4, tetra ]';

  end
%
%  Step 2. Perform an ascending dictionary sort on the neighbor relations.
%  We only intend to sort on rows 1:2; the routine we call here
%  sorts on the full column but that won't hurt us.
%
%  What we need is to find all cases where tetrahedrons share an edge.
%  By sorting the columns of the EDGE_DATA array, we will put shared edges
%  next to each other.
%
  edge_data = i4col_sort_a ( 5, 6*tetra_num1, edge_data );
%
%  Step 3. All the tetrahedrons which share an edge show up as consecutive
%  columns with identical first two entries.  Figure out how many new
%  nodes there are, and allocate space for their coordinates.
%
  node_num2 = node_num1;

  n1_old = -1;
  n2_old = -1;

  for edge = 1 : 6 * tetra_num1
    n1 = edge_data(1,edge);
    n2 = edge_data(2,edge);
    if ( n1 ~= n1_old | n2 ~= n2_old )
      node_num2 = node_num2 + 1;
      n1_old = n1;
      n2_old = n2;
    end
  end

  tetra_num2 = 8 * tetra_num1;

  return
end
