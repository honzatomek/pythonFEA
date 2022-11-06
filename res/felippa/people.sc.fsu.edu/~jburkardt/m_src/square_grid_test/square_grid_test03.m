function square_grid_test03 ( )

%*****************************************************************************80
%
%% square_grid_test03() uses a square with different sizes in each dimension.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    31 August 2014
%
%  Author:
%
%    John Burkardt
%
  a = [   0.0, -2.0 ];
  b = [ +10.0, +2.0 ];
  c = [ 3, 4 ];
  ns = [ 3, 3 ];

  n = ns(1) * ns(2);

  fprintf ( 1, '\n' );
  fprintf ( 1, 'SQUARE_GRID_TEST03\n' );
  fprintf ( 1, '  Create a grid using SQUARE_GRID.\n' );
  fprintf ( 1, '  Use a different physical size in every dimension.\n' );
  fprintf ( 1, '  Number of grid points N = %d\n', n );
  fprintf ( 1, '\n' );
  fprintf ( 1, '     I    NS     C      A         B\n' );
  fprintf ( 1, '\n' );
  for i = 1 : 2
    fprintf ( 1, '  %4d  %4d  %4d  %8.4f  %8.4f\n', i, ns(i), c(i), a(i), b(i) );
  end

  x = square_grid ( n, ns, a, b, c );
  r8mat_transpose_print ( 2, n, x, '  Grid points:' );

  return
end
