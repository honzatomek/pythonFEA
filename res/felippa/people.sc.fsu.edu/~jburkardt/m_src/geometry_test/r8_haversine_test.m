function r8_haversine_test ( )

%*****************************************************************************80
%
%% r8_haversine_test() tests r8_haversine().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 July 2018
%
%  Author:
%
%    John Burkardt
%
  n_test = 12;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'r8_haversine_test():\n' );
  fprintf ( 1, '  r8_haversine() computes the haversine of an angle.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Degrees  Radians  Haversine\n' );
  fprintf ( 1, '\n' );

  for i = 0 : n_test
    x = i * 2.0 * pi / n_test;
    d = radians_to_degrees ( x );
    hx = r8_haversine ( x );
    fprintf ( 1, '  %10f  %10f  %12f\n', d, x, hx );
  end

  return
end
