function gegenbauer_ss_compute_test ( )

%*****************************************************************************80
%
%% gegenbauer_ss_compute_test() tests gegenbauer_ss_compute().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    12 June 2015
%
%  Author:
%
%    John Burkardt
%
  alpha = 0.5;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'GEGENBAUER_SS_COMPUTE_TEST\n' );
  fprintf ( 1, '  GEGENBAUER_SS_COMPUTE computes Gauss-Gegenbauer rules;\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Abscissas and weights for a generalized Gauss Gegenbauer rule\n' );
  fprintf ( 1, '  with ALPHA = %f\n', alpha );

  for n = 1 : 10

    [ x, w ] = gegenbauer_ss_compute ( n, alpha );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, w(i), x(i) );
    end

  end

  return
end

