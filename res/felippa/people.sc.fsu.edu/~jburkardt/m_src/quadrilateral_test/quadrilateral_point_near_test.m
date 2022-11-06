function quadrilateral_point_near_test ( )

%*****************************************************************************80
%
%% quadrilateral_point_near_test() tests quadrilateral_point_near().
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
  ntest = 7;

  ptest = [ ...
     0.25,   0.25; ...
     0.75,   0.25; ...
     1.00,   1.00; ...
    11.00,   0.50; ...
     0.00,   0.50; ...
     0.50, -10.00; ...
     2.00,   2.00 ]';
  q = [ ...
    0.0, 0.0; ...
    1.0, 0.0; ...
    1.0, 1.0; ...
    0.0, 1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'quadrilateral_point_near_test()\n' );
  fprintf ( 1, '  quadrilateral_point_near() computes the nearest quadrilateral\n' );
  fprintf ( 1, '  point to a given point.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );

  fprintf ( 1, '\n' );
  fprintf ( 1, '        P1       P2\n' );
  fprintf ( 1, '\n' );

  for i = 1 : ntest

    p1(1:2,1) = ptest(1:2,i);

    p2 = quadrilateral_point_near ( q, p1 );

    fprintf ( 1, '  (%12f  %12f)  (%12f  %12f)\n', p1, p2 );

  end

  return
end
