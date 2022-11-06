function polygon_area_test ( )

%*****************************************************************************80
%
%% polygon_area_test() tests polygon_area().
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
  dim_num = 2;
  test_num = 2;
  area_exact_test = [ 2.0, 6.0 ];
  n_test = [ 4, 8 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_area_test():\n' );
  fprintf ( 1, '  polygon_area()  computes the area of a polygon.\n' );
  fprintf ( 1, '  polygon_area2() computes the area of a polygon.\n' );
  fprintf ( 1, '  polygon_area3() computes the area of a polygon.\n' );

  for test = 1 : 2

    n = n_test(test);
    area_exact = area_exact_test(test);

    if ( test == 1 )

      v = [ ...
        1.0, 0.0; ...
        2.0, 1.0; ...
        1.0, 2.0; ...
        0.0, 1.0 ]';

    elseif ( test == 2 )

      v = [ ...
        0.0, 0.0; ...
        3.0, 0.0; ...
        3.0, 3.0; ...
        2.0, 3.0; ...
        2.0, 1.0; ...
        1.0, 1.0; ...
        1.0, 2.0; ...
        0.0, 2.0 ]';

    end

    fprintf ( 1, '\n' );
    fprintf ( 1, '  polygon vertices:\n' );
    disp ( v );

    area1 = polygon_area ( n, v );
    area2 = polygon_area_2 ( n, v );
    area3 = polygon_area_3 ( n, v );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Exact area is     %f\n', area_exact );
    fprintf ( 1, '  polygon_area():   %f\n', area1 );
    fprintf ( 1, '  polygon_area_2(): %f\n', area2 );
    fprintf ( 1, '  polygon_area_3(): %f\n', area3 );
 
  end

  return
end
