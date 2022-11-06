function polygon_area_lattice_test ( )

%*****************************************************************************80
%
%% polygon_area_lattice_test() tests polygon_area_lattice().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 April 2022
%
%  Author:
%
%    John Burkardt
%
  in_test = [ 1, 5, 13 ];
  bound_test = [ 4, 8, 12 ];
  area_test = [ 2, 8, 18 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_area_lattice_test():\n' );
  fprintf ( 1, '  polygon_area_lattice() computes the area of a lattice polygon\n' );
  fprintf ( 1, '  based on the number of interior and boundary lattice points.\n' );

  for i = 1 : 3

    in = in_test(i);
    bound = bound_test(i);
    area_exact = area_test(i);

    area = polygon_area_lattice ( in, bound );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  interior points  %d\n', in );
    fprintf ( 1, '  boundary points  %d\n', bound );
    fprintf ( 1, '  Exact area is    %f\n', area_exact );
    fprintf ( 1, '  Computed area    %f\n', area );
 
  end

  return
end
