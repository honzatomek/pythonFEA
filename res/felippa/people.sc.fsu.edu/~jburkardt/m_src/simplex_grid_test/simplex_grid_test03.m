function simplex_grid_test03 ( )

%*****************************************************************************80
%
%% simplex_grid_test03() tests simplex_grid_index_sample().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 July 2014
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'SIMPLEX_GRID_TEST03:\n' );
  fprintf ( 1, '  SIMPLEX_GRID_INDEX_SAMPLE returns a randomly selected\n' );
  fprintf ( 1, '  index of a simplex grid that uses N+1 points on a side, \n' );
  fprintf ( 1, '  in an M-dimensional simplex.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '   #:  1  2  3  (*)\n' );
  fprintf ( 1, '\n' );

  m = 3;
  n = 3;

  for j = 1 : 20

    g = simplex_grid_index_sample ( m, n );

    fprintf ( 1, '  %2d:', j );
    for i = 1 : m
      fprintf ( 1, ' %2d', g(i) );
    end
    fprintf ( 1, ' (%2d)\n', g(m+1) );

  end

  return
end
