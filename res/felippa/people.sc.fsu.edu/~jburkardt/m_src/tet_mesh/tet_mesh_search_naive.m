function [ tet_index, step_num ] = tet_mesh_search_naive ( node_num, ...
  node_xyz, tet_order, tet_num, tet_node, p )

%*****************************************************************************80
%
%% tet_mesh_search_naive() naively searches a tet mesh.
%
%  Discussion:
%
%    The algorithm simply checks each tetrahedron to see if point P is
%    contained in it.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 August 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NODE_NUM, the number of nodes.
%
%    real NODE_XYZ(3,NODE_NUM), the coordinates
%    of the nodes.
%
%    integer TET_ORDER, the order of the tetrahedrons.
%
%    integer TET_NUM, the number of tetrahedrons in
%    the mesh.
%
%    integer TET_NODE(TET_ORDER,TET_NUM),
%    the nodes that make up each tetrahedron.
%
%    real P(3), the coordinates of a point.
%
%  Output:
%
%    integer TET_INDEX, the index of the tetrahedron
%    where the search ended, or -1 if no tetrahedron was found containing
%    the point.
%
%    integer STEP_NUM, the number of tetrahedrons searched.
%
  tet_index = -1;
  step_num = 0;

  for tet = 1 : tet_num

    alpha = tetrahedron_barycentric ( node_xyz(1:3,tet_node(1:4,tet)), p );

    if ( all ( 0 <= alpha(1:4) ) )
      tet_index = tet;
      step_num = tet;
      return
    end

  end

  return
end
