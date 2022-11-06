function inside = polyhedron_contains_point_3d ( node_num, face_num, ...
  face_order_max, v, face_order, face_point, p )

%*****************************************************************************80
%
%% polyhedron_contains_point_3d() determines if a point is inside a polyhedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 August 2005
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Paulo Cezar Pinto Carvalho and Paulo Roma Cavalcanti,
%    Point in Polyhedron Testing Using Spherical Polygons,
%    in Graphics Gems V,
%    edited by Alan Paeth,
%    Academic Press, 1995, T385.G6975.
%
%  Input:
%
%    integer NODE_NUM, the number of vertices.
%
%    integer FACE_NUM, the number of faces.
%
%    integer FACE_ORDER_MAX, the maximum order of any face.
%
%    real V(3,NODE_NUM), the coordinates of the vertices.
%
%    integer FACE_ORDER(FACE_NUM), the order of each face.
%
%    integer FACE_POINT(FACE_ORDER_MAX,FACE_NUM), the indices of the
%    nodes that make up each face.
%
%    real P(3), the point to be tested.
%
%  Output:
%
%    logical INSIDE, is true if the point
%    is inside the polyhedron.
%
  p = p(:);

  dim_num = 3;

  area = 0.0;

  for face = 1 : face_num

    node_num_face = face_order(face);

    for k = 1 : node_num_face

      node = face_point(k,face);

      v_face(1:3,k) = v(1:3,node);

    end

    solid_angle = polygon_solid_angle_3d ( node_num_face, v_face, p );

    area = area + solid_angle;

  end
%
%  AREA should be -4*PI, 0, or 4*PI.
%  So this test should be quite safe!
%
  if ( area < -2.0 * pi | 2.0 * pi < area )
    inside = 1;
  else
    inside = 0;
  end

  return
end
