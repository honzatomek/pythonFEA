function quadrilateral_point_dist_signed_test ( )

%*****************************************************************************80
%
%% quadrilateral_point_dist_signed_test() tests quadrilateral_point_dist_signed().
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
  fprintf ( 1, 'quadrilateral_point_dist_signed_test\n' );
  fprintf ( 1, '  quadrilateral_point_dist_signed() computes the signed distance\n' );
  fprintf ( 1, '  from a point to a quadrilateral.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  quadrilateral vertices:\n' );
  disp ( q );

  fprintf ( 1, '\n' );
  fprintf ( 1, '        P        Dist\n' );
  fprintf ( 1, '\n' );

  for i = 1 : ntest

    p(1:2,1) = ptest(1:2,i);

    dist_signed = quadrilateral_point_dist_signed ( q, p );

    fprintf ( 1, '  ( %12f  %12f )  %10f\n', p, dist_signed );

  end

  return
end
