function poly_write ( file_name, node_num, segment, edge_num, ...
  edge_nodes, hole_num, hole_point )

%*****************************************************************************80
%
%% poly_write() writes data to a POLY file.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string FILE_NAME, the name of the file to create.
%
%    integer NODE_num, the number of nodes.
%
%    real SEGMENT(2,NODE_num), the nodes.
%
%    integer EDGE_num, the number of edges.
%
%    integer EDGE_NODES(2,EDGE_num), the nodes that form each edge.
%
%    integer HOLE_num, the number of holes in the mesh.
%
%    real HOLE_point(2,HOLE_num), a point in each hole.
%
  region_num = 0;

  poly_unit = fopen ( file_name, 'w' );

  if ( poly_unit < 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'POLY_WRITE - Fatal error!\n' );
    fprintf ( 1, '  Could not open the output POLY file.\n' );
    error ( 'POLY_WRITE - Fatal error!' );
  end

  fprintf ( poly_unit, '#  %s\n', file_name );
  fprintf ( poly_unit, '#  Created by poly_write.m\n' );
  fprintf ( poly_unit, '#\n' );
  fprintf ( poly_unit, '#  Vertex  Dimension  Attribute  Marker\n' );
  fprintf ( poly_unit, '#  Count              Count      0/1\n' );
  fprintf ( poly_unit, '#\n' );
  fprintf ( poly_unit, '  %d  2  0  0\n', node_num );
  fprintf ( poly_unit, '#\n' );
  fprintf ( poly_unit, '#  Vertex  X  Y  Attributes  Marker\n' );
  fprintf ( poly_unit, '#  Index\n' );
  fprintf ( poly_unit, '#\n' );
  for node = 1 : node_num
    fprintf ( poly_unit, '  %8d  %10f  %10f\n', node, segment(1:2,node) );
  end
  fprintf ( poly_unit, '#\n' );
  fprintf ( poly_unit, '#  Segment  Marker\n' );
  fprintf ( poly_unit, '#  Count    0/1\n' );
  fprintf ( poly_unit, '#\n' );
  fprintf ( poly_unit, '  %d  0\n', edge_num );
  fprintf ( poly_unit, '#\n' );
  fprintf ( poly_unit, '#  Segment_index  Node1  Node2  Marker\n' );
  fprintf ( poly_unit, '#\n' );
  for edge = 1 : edge_num
    fprintf ( poly_unit, '  %8d  %8d  %8d  0\n', edge, edge_nodes(1:2,edge) );
  end
  fprintf ( poly_unit, '#\n' );
  fprintf ( poly_unit, '#  Hole\n' );
  fprintf ( poly_unit, '#  Count\n' );
  fprintf ( poly_unit, '#\n' );
  fprintf ( poly_unit, '  %d\n', hole_num );
  fprintf ( poly_unit, '#\n' );
  fprintf ( poly_unit, '#  Hole_index  X  Y\n' );
  fprintf ( poly_unit, '#\n' );
  for hole = 1 : hole_num
    fprintf ( poly_unit, '  %8d  %10f  %10f\n', hole, hole_point(1:2,hole) );
  end
  fprintf ( poly_unit, '#\n' );
  fprintf ( poly_unit, '#  Region\n' );
  fprintf ( poly_unit, '#  Count\n' );
  fprintf ( poly_unit, '#\n' );
  fprintf ( poly_unit, '  %d\n', region_num );

  fclose ( poly_unit );

  return
end

