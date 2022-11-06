function polygon_perimeter_test ( )

%*****************************************************************************80
%
%% polygon_perimeter_test() tests polygon_perimeter().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    16 October 2015
%
%  Author:
%
%    John Burkardt
%
  n1 = 4;
  n2 = 3;
 
  v1 = [ ...
    0.0, 0.0; ...
    1.0, 0.0; ...
    1.0, 1.0; ...
    0.0, 1.0 ]';
  v2 = [ ...
    1.0, 1.0; ...
    4.0, 3.0; ...
    2.0, 5.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_perimeter_test():\n' );
  fprintf ( 1, '  polygon_perimeter() computes the perimeter of a polygon.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v1 );

  perimeter = polygon_perimeter ( n1, v1 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Perimeter of V1 = %g\n', perimeter );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v2 );

  perimeter = polygon_perimeter ( n2, v2 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Perimeter of V2 = %g\n', perimeter );

  return
end
