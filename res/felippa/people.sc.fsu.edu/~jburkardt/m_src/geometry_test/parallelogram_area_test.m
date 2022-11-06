function parallelogram_area_test ( )

%*****************************************************************************80
%
%% parallelogram_area_test() tests parallelogram_area().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 May 2010
%
%  Author:
%
%    John Burkardt
%
  p = [ ...
    2.0, 7.0; ...
    5.0, 7.0; ...
    6.0, 9.0; ...
    3.0, 9.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'parallelogram_area_test():\n' );
  fprintf ( 1, '  parallelogram_area() finds the area of a parallelogram.\n' );

  r8mat_transpose_print ( 2, 4, p, '  Vertices:' );

  area = parallelogram_area ( p );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  AREA = %f\n', area );

  return
end
