function fejer2_compute_test ( )

%*****************************************************************************80
%
%% fejer2_compute_test() tests fejer2_compute().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    15 April 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'FEJER2_COMPUTE_TEST\n' );
  fprintf ( 1, '  FEJER2_COMPUTE computes the abscissas and weights\n' );
  fprintf ( 1, '  of a Fejer type 2 quadrature rule.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '    Order  W             X\n' );

  for n = 1 : 10

    [ x, w ] = fejer2_compute ( n );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  %8d\n', n );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
