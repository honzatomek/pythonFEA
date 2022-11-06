function geometry_test041 ( )

%*****************************************************************************80
%
%% geometry_test041() tests lines_imp_dist_2d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 April 2009
%
%  Author:
%
%    John Burkardt
%
  dim_num = 2;
  ntest = 3;

  a1test = [  4.0,  2.0, 1.0 ];
  a2test = [  4.0,  4.0, 2.0 ];
  b1test = [ -1.0, -1.0, 2.0 ];
  b2test = [ -1.0, -2.0, 3.0 ];
  c1test = [  3.0,  0.0, 2.0 ];
  c2test = [ 12.0,  6.0, 1.0 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'geometry_test041():\n' );
  fprintf ( 1, '  lines_imp_dist_3d() finds the distance between\n' );
  fprintf ( 1, '  two implicit lines in 2D.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '   A1      B1      C1      A2      B2      C2   DIST\n' );
  fprintf ( 1, '\n' );

  for i = 1 : ntest

    a1 = a1test(i);
    b1 = b1test(i);
    c1 = c1test(i);
    a2 = a2test(i);
    b2 = b2test(i);
    c2 = c2test(i);

    dist = lines_imp_dist_2d ( a1, b1, c1, a2, b2, c2 );

    fprintf ( 1, '  %10f  %10f  %10f  %10f  %10f  %10f  %10f\n', ...
      a1, b1, c1, a2, b2, c2, dist );

  end

  return
end
