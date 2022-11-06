function simplex_coordinates2_test ( n )

%*****************************************************************************80
%
%% simplex_coordinates2_test() tests simplex_coordinates2().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the spatial dimension.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'SIMPLEX_COORDINATES2_TEST\n' );
  fprintf ( 1, '  Call SIMPLEX_COORDINATES2\n' );

  x = simplex_coordinates2 ( n );

  r8mat_transpose_print ( n, n + 1, x, '  Simplex vertex coordinates:' );

  side = sqrt ( sum ( ( x(1:n,1) - x(1:n,2) ).^2 ) );

  volume = simplex_volume ( n, x );

  volume2 = sqrt ( n + 1 ) / factorial ( n ) / sqrt ( 2.0^n ) * side^n;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Side length =     %f\n', side );
  fprintf ( 1, '  Volume =          %f\n', volume );
  fprintf ( 1, '  Expected volume = %f\n', volume2 );

  xtx = x' * x;

  r8mat_transpose_print ( n + 1, n + 1, xtx, '  Dot product matrix:' );

  return
end
