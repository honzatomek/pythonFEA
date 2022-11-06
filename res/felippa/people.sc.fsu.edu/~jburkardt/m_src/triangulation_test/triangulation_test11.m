function triangulation_test11 ( )

%*****************************************************************************80
%
%% triangulation_test11() tests triangulation_node_order().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 August 2006
%
%  Author:
%
%    John Burkardt
%
  node_num = 36;
  triangle_num = 41;
  triangle_order = 3;

  triangle_node = [ ...
     1,  8,  7; ...
     1,  2,  8; ...
     2,  9,  8; ...
     2,  3,  9; ...
     3, 10,  9; ...
     3,  4, 10; ...
     4, 11, 10; ...
     4,  5, 11; ...
     5, 12, 11; ...
     5,  6, 12; ...
     7, 14, 13; ...
     7,  8, 14; ...
     8, 15, 14; ...
     8,  9, 15; ...
    11, 18, 17; ...
    11, 12, 18; ...
    13, 20, 19; ...
    13, 14, 20; ...
    14, 21, 20; ...
    14, 15, 21; ...
    15, 22, 21; ...
    15, 16, 22; ...
    16, 23, 22; ...
    16, 17, 23; ...
    17, 24, 23; ...
    17, 18, 24; ...
    19, 26, 25; ...
    19, 20, 26; ...
    21, 28, 27; ...
    21, 22, 28; ...
    25, 30, 29; ...
    25, 26, 30; ...
    26, 31, 30; ...
    27, 32, 31; ...
    27, 28, 32; ...
    29, 34, 33; ...
    29, 30, 34; ...
    30, 35, 34; ...
    30, 31, 35; ...
    31, 36, 35; ...
    31, 32, 36 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangulation_test11\n' );
  fprintf ( 1, '  TRIANGULATION_NODE_ORDER computes the order\n' );
  fprintf ( 1, '  of the nodes in a triangulation.\n' );

  node_order = triangulation_node_order ( triangle_order, triangle_num, ...
    triangle_node, node_num );

  i4vec_print ( node_num, node_order, '  NODE ORDER:' );

  return
end
