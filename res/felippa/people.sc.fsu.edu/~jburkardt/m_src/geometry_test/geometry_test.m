function geometry_test ( )

%*****************************************************************************80
%
%% geometry_test() tests geometry().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 April 2022
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../geometry' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'geometry_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test geometry()\n' );

  angle_box_2d_test ( );
  angle_contains_point_2d_test ( );
  angle_deg_2d_test ( );
  angle_half_test ( );
  angle_rad_test ( );
  angle_rad_3d_test ( );
  angle_rad_nd_test ( );
  angle_turn_2d_test ( );

  annulus_area_2d_test ( );

  annulus_sector_area_2d_test ( );
  annulus_sector_centroid_2d_test ( );

  ball01_sample_2d_test ( );
  ball01_sample_3d_test ( );
  ball01_sample_nd_test ( );
  ball01_volume_test ( );

  basis_map_3d_test ( );

  box_segment_clip_2d_test ( );
  box_ray_int_2d_test ( );

  circle_area_2d_test ( );

  circle_dia2imp_2d_test ( );

  circle_exp_contains_point_2d_test ( );
  circle_exp2imp_2d_test ( );

  circle_imp_points_test ( );
  circle_imp_points_arc_test ( );
  circle_imp_point_dist_2d_test ( );
  circle_imp_points_3d_test ( );
  circle_imp_print_2d_test ( );
  circle_imp_segment_intersect_test ( );
  circle_imp2exp_2d_test ( );

  circle_llr2imp_2d_test ( );

  circle_lune_angle_by_height_2d_test ( );
  circle_lune_area_by_angle_2d_test ( );
  circle_lune_area_by_height_2d_test ( );
  circle_lune_centroid_2d_test ( );
  circle_lune_height_by_angle_2d_test ( );

  circle_pppr2imp_3d_test ( );
  circle_ppr2imp_2d_test ( );

  circle_sector_area_2d_test ( );
  circle_sector_centroid_2d_test ( );

  circle_triangle_area_2d_test ( );

  circles_intersect_area_2d_test ( );
  circles_intersect_points_2d_test ( );

  cone_volume_test ( );

  cube_shape_test ( );

  cube01_volume_test ( );

  cylinder_point_dist_test ( );
  cylinder_point_dist_signed_test ( );
  cylinder_point_inside_test ( );
  cylinder_point_near_test ( );
  cylinder_sample_test ( );
  cylinder_volume_test ( );

  degrees_to_radians_test ( );

  direction_pert_3d_test ( );
  direction_uniform_3d_test ( );
  direction_uniform_nd_test ( );

  disk_point_dist_3d_test ( );

  dms_to_radians_test ( );

  dodec_shape_test ( );

  dual_shape_test ( );
  dual_size_test ( );

  halfplane_contains_point_2d_test ( );

  halfspace_imp_triangle_int_3d_test ( );
  halfspace_normal_triangle_int_3d_test ( );

  hexagon_contains_point_2d_test ( );

  hexagon01_shape_2d_test ( );
  hexagon01_vertices_test ( );

  hyperball01_volume_test ( );
  hyperball01_volume_pnorm_test ( );

  i4_wrap_test ( );

  i4col_find_item_test ( );
  i4col_find_pair_wrap_test ( );

  i4vec_heap_d_test ( );
  i4vec_indicator1_test ( );
  i4vec_lcm_test ( );
  i4vec_print_test ( );

  icos_shape_test ( );

  line_exp_perp_2d_test ( );
  line_exp_normal_test ( );
  line_exp_point_dist_test ( );
  line_exp_point_dist_3d_test ( );
  line_exp_point_dist_signed_test ( );
  line_exp_point_near_2d_test ( );
  line_exp2imp_2d_test ( );
  line_exp2par_2d_test ( );

  line_imp2exp_2d_test ( );
  line_imp_point_dist_2d_test ( );

  line_par_point_dist_2d_test ( );
  line_par_point_dist_3d_test ( );
  line_par_point_near_2d_test ( );
  line_par_point_near_3d_test ( );
  line_par2exp_2d_test ( );

  lines_exp_angle_3d_test ( );
  lines_exp_dist_3d_test ( );
  lines_exp_equal_2d_test ( );
  lines_exp_int_2d_test ( );
  lines_exp_near_3d_test ( );

  lines_imp_angle_2d_test ( );
  lines_imp_int_2d_test ( );
  lines_imp_dist_3d_test ( );

  lines_par_int_2d_test ( );

  minabs_test ( );

  minquad_test ( );

  octahedron_shape_test ( );

  parabola_ex_test ( );

  parallelipiped_point_dist_3d_test ( );

  parallelogram_area_test ( );
  parallelogram_area_3d_test ( );
  parallelogram_contains_point_3d_test ( );
  parallelogram_contains_point_2d_test ( );

  plane_exp_normal_test ( );
  plane_exp2imp_3d_test ( );
  plane_exp2normal_3d_test ( );
  plane_exp_project_test ( );

  plane_imp2exp_3d_test ( );
  plane_imp2normal_3d_test ( );
  plane_imp_line_par_int_3d_test ( );
  plane_imp_point_dist_3d_test ( );
  plane_imp_point_dist_signed_3d_test ( );
  plane_imp_segment_near_3d_test ( );
  plane_imp_triangle_near_3d_test ( );
  plane_imp_triangle_int_3d_test ( );

  plane_normal_basis_3d_test ( );
  plane_normal_line_exp_int_3d_test ( );
  plane_normal_qr_to_xyz_test ( );
  plane_normal_xyz_to_qr_test ( );

  plane_normal_tetrahedron_intersect_test ( );
  plane_normal_triangle_int_3d_test ( );
  plane_normal2exp_3d_test ( );
  plane_normal2imp_3d_test ( );

  points_centroid_2d_test ( );
  points_colin_2d_test ( );

  polar_to_xy_test ( );

  polyhedron_area_3d_test ( );
  polyhedron_centroid_3d_test
  polyhedron_contains_point_3d_test ( );
  polyhedron_volume_3d_test ( );
  polyhedron_volume_3d_2_test ( );

  polyline_arclength_nd_test ( );
  polyline_points_nd_test ( );

  polyloop_arclength_nd_test ( );
  polyloop_points_nd_test ( );

  provec_test ( );

  pyramid_volume_3d_test ( );
  pyramid01_volume_3d_test ( );

  r8_haversine_test ( );

  r82vec_part_quick_a_test ( );
  r82vec_permute_test ( );
  r82vec_print_test ( );
  r82vec_sort_quick_a_test ( );
  r82vec_sort_heap_index_a_test ( );

  r8mat_inverse_3d_test ( );

  r8vec_any_normal_test ( );
  r8vec_scalar_triple_product_test ( );

  r8vec3_print_test ( );

  radec_distance_3d_test ( );
  radec_to_xyz_test ( );

  radians_to_degrees_test ( );
  radians_to_dms_test ( );
 
  rtp_to_xyz_test ( );

  segment_contains_point_1d_test ( );
  segment_point_dist_test ( );
  segment_point_near_test ( );
  segment_point_dist_3d_test ( );
  segment_point_near_3d_test ( );

  segments_curvature_2d_test ( );
  segments_dist_2d_test ( );
  segments_dist_3d_test ( );
  segments_int_1d_test ( );
  segments_int_2d_test ( );

  shape_point_dist_2d_test ( );
  shape_point_near_2d_test ( );
  shape_ray_int_2d_test ( );

  simplex_lattice_layer_point_next_test ( );
  simplex_lattice_point_next_test ( );
  simplex_volume_nd_test ( );

  simplex01_volume_nd_test ( );

  soccer_shape_test ( );

  sort_heap_external_test ( );

  sphere_cap_area_3d_test ( );
  sphere_cap_volume_3d_test ( );
  sphere_dia2imp_3d_test ( );
  sphere_distance_test ( );

  sphere_exp_contains_point_3d_test ( );
  sphere_exp_point_near_3d_test ( );
  sphere_exp2imp_3d_test ( );
  sphere_exp2imp_nd_test ( );

  sphere_imp_area_nd_test ( );
  sphere_imp_contains_point_3d_test ( );
  sphere_imp_gridfaces_3d_test ( );
  sphere_imp_point_near_3d_test ( );
  sphere_imp_point_project_3d_test ( );
  sphere_imp_volume_nd_test ( );
  sphere_imp2exp_3d_test ( );

  sphere_triangle_sides_to_angles_test ( );

  sphere01_area_nd_test ( );
  sphere01_sample_2d_test ( );
  sphere01_sample_3d_test ( );
  sphere01_sample_3d_2_test ( );
  sphere01_sample3_nd_test ( );
  sphere01_sample_nd_test ( );
  sphere01_sample2_nd_test ( );
  sphere01_volume_nd_test ( );

  string_2d_test ( );

  super_ellipse_points_2d_test ( );

  tmat_test ( );
  tmat_mxp2_test ( );

  tp_to_xyz_test ( );

  triangle_angles_2d_test ( );
  triangle_angles_3d_test ( );
  triangle_area_test ( );
  triangle_area_3d_test ( );
  triangle_area_heron_test ( );
  triangle_barycentric_test ( );
  triangle_centroid_test ( );
  triangle_centroid_3d_test ( );
  triangle_circumcenter_test ( );
  triangle_circumcenter_nd_test ( );
  triangle_circumcircle_test ( );
  triangle_circumradius_test ( );
  triangle_contains_line_exp_3d_test ( );
  triangle_contains_line_par_3d_test ( );
  triangle_contains_point_test ( );
  triangle_diameter_test ( );
  triangle_gridpoints_test ( );
  triangle_incenter_test ( );
  triangle_inradius_test ( );
  triangle_lattice_layer_point_next_test ( );
  triangle_lattice_point_next_test ( );
  triangle_orientation_test ( );
  triangle_orthocenter_2d_test ( );
  triangle_point_dist_test ( );
  triangle_point_dist_signed_2d_test ( );
  triangle_point_dist_3d_test ( );
  triangle_point_near_test ( );
  triangle_quality_2d_test ( );
  triangle_sample_test ( );
  triangle_xsi_to_xy_test ( );
  triangle_xy_to_xsi_test ( );

  tube_2d_test ( );

  tuple_next2_test ( );

  vector_directions_nd_test ( );
  vector_rotate_2d_test ( );
  vector_rotate_3d_test ( );
  vector_rotate_base_2d_test ( );
  vector_separation_nd_test ( );
  vector_unit_nd_test ( );

  voxels_dist_l1_nd_test ( );
  voxels_line_3d_test ( );
  voxels_region_3d_test ( );
  voxels_step_3d_test ( );

  wedge01_volume_test ( );

  xy_to_polar_test ( );

  xyz_to_radec_test ( );
  xyz_to_rtp_test ( );
  xyz_to_tp_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'geometry_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../geometry' );

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp() prints the current YMDHMS date as a timestamp.
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
  t = now;
  c = datevec ( t );
  s = datestr ( c, 0 );
  fprintf ( 1, '%s\n', s );

  return
end

