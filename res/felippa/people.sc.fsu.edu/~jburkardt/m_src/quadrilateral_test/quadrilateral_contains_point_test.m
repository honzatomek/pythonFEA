function quadrilateral_contains_point_test ( )

%*****************************************************************************80
%
%% quadrilateral_contains_point_test() quadrilateral_contains_point().
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
  fprintf ( 1, 'quadrilateral_contains_point_test()\n' );
  fprintf ( 1, '  quadrilateral_contains_point() tells if a point is inside\n' );
  fprintf ( 1, '  a quadrilateral.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );

  fprintf ( 1, '\n' );
  fprintf ( 1, '        P        Contains  Dist    Dist\n' );
  fprintf ( 1, '                          Signed  Unsigned\n' );
  fprintf ( 1, '\n' );

  for i = 1 : ntest

    p(1:2,1) = ptest(1:2,i);

    inside = quadrilateral_contains_point ( q, p );

    fprintf ( 1, '  %12f  %12f  %1d\n', p(1:2,1), inside );

  end

  return
end
