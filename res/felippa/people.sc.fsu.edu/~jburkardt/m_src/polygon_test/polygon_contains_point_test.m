function polygon_contains_point_test ( )

%*****************************************************************************80
%
%% polygon_contains_point_test() tests polygon_contains_point_*().
%
%  Discussion:
%
%    x-x-x-x-x-x-x
%    |i          |
%    x x-x-x-x-x-x
%    | |o
%    x x x-x-x-x x
%    | | |i    |
%    x x x x-x x x
%    | | | |o| |
%    x x x-x x x x
%    | |     |i|
%    x x-x-x-x x x
%    |         |o
%    x-x-x-x-x-x x
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 March 2021
%
%  Author:
%
%    John Burkardt
%
  ptest = [ ...
    0.5, 5.5; ...
    1.5, 4.5; ...
    2.5, 3.5; ...
    3.5, 2.5; ...
    4.5, 1.5; ...
    5.5, 0.5 ]';

  ntest = 6;

  n = 14;

  v = [ ...
    0.0, 0.0; ...
    5.0, 0.0; ...
    5.0, 4.0; ...
    2.0, 4.0; ...
    2.0, 2.0; ...
    3.0, 2.0; ...
    3.0, 3.0; ...
    4.0, 3.0; ...
    4.0, 1.0; ...
    1.0, 1.0; ...
    1.0, 5.0; ...
    6.0, 5.0; ...
    6.0, 6.0; ...
    0.0, 6.0 ]';

  inside_correct = [ ...
    true; false; true; false; true; false ];
 
  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_contains_point_test():\n' );
  fprintf ( 1, '  polygon_contains_point_1() determines if \n' );
  fprintf ( 1, '  a point is in a polygon.\n' );
  fprintf ( 1, '  polygon_contains_point_2() determines if \n' );
  fprintf ( 1, '  a point is in a convex polygon.\n' );
  fprintf ( 1, '  polygon_contains_point_3() determines if \n' );
  fprintf ( 1, '  a point is in a simple polygon.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v )

  fprintf ( 1, '\n' );
  fprintf ( 1, '          P               Inside  Inside1  Inside2  Inside3\n' );
  fprintf ( 1, '\n' );

  for j = 1 : ntest
 
    p(1:2,1) = ptest(1:2,j);
 
    inside1 = polygon_contains_point_1 ( n, v, p );
    inside2 = polygon_contains_point_2 ( n, v, p );
    inside3 = polygon_contains_point_3 ( n, v, p );

    s0 = boolean_to_string ( inside_correct(j) );
    s1 = boolean_to_string ( inside1 );
    s2 = boolean_to_string ( inside2 );
    s3 = boolean_to_string ( inside3 );

    fprintf ( 1, '  %12f  %12f  %s  %s   %s   %s\n', p(1:2), s0, s1, s2, s3 );

  end
 
  return
end
