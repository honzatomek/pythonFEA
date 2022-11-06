function circle_imp_segment_intersect_test ( )

%*****************************************************************************80
%
%% circle_imp_segment_intersect_test() tests circle_imp_segment_intersect().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    15 September 2020
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'circle_imp_segment_intersect_test():\n' );
  fprintf ( 1, '  circle_imp_segment_intersect() finds the intersection\n' );
  fprintf ( 1, '  of an implicit circle and a line segment.\n' );
 
  r = 5.0;
  pc = [ 5.0; 2.0 ];

  circle_imp_print_2d ( r, pc, '  The implicit circle:' );

  for i = 1 : 3

    if ( i == 1 )
      p1 = [ 13.0;  8.0 ];
      p2 = [ 17.0; 11.0 ];
    elseif ( i == 2 )
      p1 = [ -3.0; -4.0 ];
      p2 = [ 17.0; 11.0 ];
    elseif ( i == 3 )
      p1 = [ -3.0; -4.0 ];
      p2 = [  5.0;  2.0 ];
    end

    line_exp_print_2d ( p1, p2, '  The line segment:' );

    [ int_num, p ] = circle_imp_segment_intersect ( r, pc, p1, p2 );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Number of intersections found = %d\n', int_num );
    for j = 1 : int_num
      fprintf ( 1, '  %d  %f  %f\n', j, p(1,j), p(2,j) );
    end

  end

  return
end
