function geometry_test0803 ()

%*****************************************************************************80
%
%% polygon_data_test() tests polygon_data_*();
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 March 2009
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_data_test()\n' );
  fprintf ( 1, '  For a REGULAR polygon:\n' );
  fprintf ( 1, '  the inradius, outradius and side are related.\n' );
  fprintf ( 1, '  polygon_data_inrad() uses the inradius;\n' );
  fprintf ( 1, '  polygon_data_outrad() uses the inradius;\n' );
  fprintf ( 1, '  polygon_data_side() uses the inradius;\n' );

  for n = 3 : 5

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Number of polygonal sides = %d\n', n );

    side = 1.0;

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Assuming SIDE = %f\n', side );

    [ area, radin, radout ] = polygon_data_side ( n, side );

    fprintf ( 1, '    AREA =   %f\n', area );
    fprintf ( 1, '    RADIN =  %f\n', radin );
    fprintf ( 1, '    RADOUT = %f\n', radout );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Assuming RADIN = %f\n', radin );

    [ area, radout, side ] = polygon_data_inrad ( n, radin );

    fprintf ( 1, '    AREA =   %f\n', area );
    fprintf ( 1, '    RADOUT = %f\n', radout );
    fprintf ( 1, '    SIDE =   %f\n', side );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Assuming RADOUT = %f\n', radout );

    [ area, radin, side ] = polygon_data_outrad ( n, radout );

    fprintf ( 1, '    AREA =   %f\n', area );
    fprintf ( 1, '    RADIN =  %f\n', radin );
    fprintf ( 1, '    SIDE =   %f\n', side );
 
  end

  return
end
