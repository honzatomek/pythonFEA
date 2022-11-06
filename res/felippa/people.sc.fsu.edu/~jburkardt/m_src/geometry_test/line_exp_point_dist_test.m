function line_exp_point_dist_test ( )

%*****************************************************************************80
%
%% line_exp_point_dist_test() tests line_exp_point_dist().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 December 2010
%
%  Author:
%
%    John Burkardt
%
  ntest = 3;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'line_exp_point_dist_test():\n' );
  fprintf ( 1, '  line_exp_point_dist() finds the distance from\n' );
  fprintf ( 1, '  an explicit line to a point in 2D.\n' );

  p1(1:2,1) = [ 1.0; 3.0 ];
  p2(1:2,1) = [ 4.0; 0.0 ];

  ptest(1:2,1:ntest) = [ ...
    0.0,  0.0; ...
    5.0, -1.0; ...
    5.0,  3.0 ]';

  r8vec_print ( 2, p1, '  Point 1: ' );
  r8vec_print ( 2, p2, '  Point 2: ' );

  for i = 1 : ntest

    p(1:2,1) = ptest(1:2,i);

    r8vec_print ( 2, p, '  Point: ' );

    dist = line_exp_point_dist ( p1, p2, p );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Distance = %f\n', dist );

  end

  return
end
