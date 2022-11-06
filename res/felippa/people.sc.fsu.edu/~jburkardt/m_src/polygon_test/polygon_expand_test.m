function polygon_expand_test ()

%*****************************************************************************80
%
%% polygon_expand_test() tests polygon_expand();
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

  v = [ ...
    1.0, 1.0; ...
    5.0, 1.0; ...
    2.0, 4.0; ...
    1.0, 3.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_expand_test():\n' );
  fprintf ( 1, '  polygon_expand() "expands" a polygon by an amount H.\n' );

  h = 0.5;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  The expansion amount H = %f\n', h );

  w = polygon_expand ( n, v, h );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  expanded polygon vertices:\n' );
  disp ( w );

  return
end
