function node_xyz = sphere_grid_q9_node_xyz ( nelemx, nelemy )

%*****************************************************************************80
%
%% sphere_grid_q9_node_xyz() produces node coordinates for a Q9 sphere grid.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 September 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NELEMX, NELEMY, the number of elements along the
%    X and Y directions.  
%
%  Output:
%
%    real NODE_XYZ(3,4*NELEMX*NELEMY-2*NELEMX+2), 
%    the node coordinates.
%
  node = 0;

  node = node + 1;
  node_xyz(1,node) =  0.0;
  node_xyz(2,node) =  0.0;
  node_xyz(3,node) = -1.0;

  for j = 2 * nelemy : -1 : 2

    phi = ( j - 1 ) * pi / ( 2 * nelemy );

    for i = 1 : 2 * nelemx

      theta = ( i - 1 ) * 2.0 * pi / ( 2 * nelemx );

      node = node + 1;
      node_xyz(1,node) = cos ( theta ) * sin ( phi );
      node_xyz(2,node) = sin ( theta ) * sin ( phi );
      node_xyz(3,node) =                 cos ( phi );

    end
  end

  node = node + 1;
  node_xyz(1,node) =  0.0;
  node_xyz(2,node) =  0.0;
  node_xyz(3,node) =  1.0;

  return
end
