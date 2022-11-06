function polygon_diameter_test ()

%*****************************************************************************80
%
%% polygon_diameter_test() tests polygon_diameter();
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
  n = 4;
  dim_num = 2;

  diameter_exact = 2.0;
  v = [ ...
    1.0, 0.0; ...
    2.0, 1.0; ...
    1.0, 2.0; ...
    0.0, 1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_diameter_test():\n' );
  fprintf ( 1, '  polygon_diameter() computes the diameter of a polygon.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  diameter = polygon_diameter ( n, v );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Diameter ( computed ) %f\n', diameter );
  fprintf ( 1, '  Diameter ( exact )    %f\n', diameter_exact );
 
  return
end
